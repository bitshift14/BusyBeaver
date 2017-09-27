# BusyBeaver team for MAPC 2017

# pyson required, see https://github.com/niklasf/pyson


# USAGE: py agents.py host username password
# configuration:
# server port to connect to
port = 12300
# folder with graphhopper maps 
graphs_path = "massim/server/graphs/"

import asyncio
import pyson.mapc2017
import pyson.runtime
from pyson import Literal
import logging
import struct
from math import radians, cos, sin, pi, asin, sqrt, floor, ceil
from haversine import haversine
from heapq import heappush, heappop
import sys
from itertools import product, combinations
import os
import time
import sys

int_deg_fac = 2 ** 31 / 400
node_invalid = -1
edge_invalid = -1
# millimeters
radius_earth = 6371000000
# high distance to return when calculation fails
dist_invalid = 1000000000
# snap towards tower nodes in graph positioning
edge_penalty = 2


class node:
    def __init__(self, gh_node):
        edge_ref, lat, lon = gh_node
        self.edge = edge_ref
        self.pos = lat / int_deg_fac, lon / int_deg_fac
    
class edge:
    def __init__(self, gh_edge):
        nodea, nodeb, linka, linkb, dist, flags, geo, name = gh_edge
        self.nodea = nodea
        self.nodeb = nodeb
        self.linka = linka
        self.linkb = linkb
        self.dist = dist
        self.flags = flags
        self.geo = geo
        self.name = name

class graph_pos:
    def __init__(self, id, edge_pos, is_node):
        assert(0 <= edge_pos and edge_pos <= 1)
        self.id = id
        self.edge_pos = edge_pos
        self.is_node = is_node


class graph:
    def __init__(self, name):
        # read graphhopper files
        with open(graphs_path + name + "/nodes", mode='rb') as file:
            ndata = file.read()
            
            assert struct.unpack(">i", ndata[0:4])[0] & 0xffff == 0x4748
            assert struct.unpack(">i", ndata[20:24])[0] == 12
            node_count, self.min_lon, self.max_lon, self.min_lat, self.max_lat = struct.unpack(">iiiii", ndata[24:44])
            self.min_lon /= int_deg_fac
            self.max_lon /= int_deg_fac
            self.min_lat /= int_deg_fac
            self.max_lat /= int_deg_fac
            
            self.nodes = [node(struct.unpack("<iii", ndata[12*i + 100 : 12*i + 112])) for i in range(node_count)]
        
        with open(graphs_path + name + "/edges", mode='rb') as file:
            edata = file.read()
            
            assert struct.unpack(">i", edata[0:4])[0] & 0xffff == 0x4748
            assert struct.unpack(">i", edata[16:20])[0] == 32
            edge_count = struct.unpack(">i", edata[20:24])[0]
            
            self.edges = [edge(struct.unpack("<iiiiiiii", edata[32*i + 100 : 32*i + 132])) for i in range(edge_count)]
        
        with open(graphs_path + name + "/geometry", mode='rb') as file:
            gdata = file.read()
            
            assert struct.unpack(">i", gdata[0:4])[0] & 0xffff == 0x4748
            geo_length = struct.unpack(">i", gdata[16:20])[0]
            
            self.geometry = [struct.unpack("<i", gdata[4*i + 100 : 4*i + 104])[0] for i in range(geo_length)]
        
        self.scale_lat = pi * radius_earth / 180
        lon_radius = radius_earth * cos(pi * (self.max_lat + self.min_lat) / 360)
        self.scale_lon = pi * lon_radius / 180
    
    def geo(self, ofs):
        # yield pillar nodes
        len = self.geometry[ofs]
        assert len < 256
        list = []
        for i in range(len):
            yield self.geometry[ofs + 2*i] / int_deg_fac, self.geometry[ofs + 2*i + 1] / int_deg_fac
    
    def dist_air(self, a, b):
        dlat = (a[0] - b[0]) * self.scale_lat
        dlon = (a[1] - b[1]) * self.scale_lon
        return sqrt(dlat*dlat + dlon*dlon)
        
        
    def pos(self, pos):
        min = dist_invalid
        id = node_invalid
        edge_pos = 0
        is_node = True
        bsize = 16
        
        # select nearest tower nodes
        best = [(dist_invalid, node_invalid) for i in range(bsize)]
        
        for i in range(len(self.nodes)):
            d = self.dist_air(pos, self.nodes[i].pos)
            c = d, i
            if(c < best[-1]):
                best[-1] = c
                best.sort()
        
        min, id = best[0]
        if min < edge_penalty:
            return graph_pos(id, 0, True)
        else:
            min -= edge_penalty
        
        # filter remote edges
        valid_nodes = [id for min, id in best]
        
        for edge_id in range(len(self.edges)):
            edge = self.edges[edge_id]
            if edge.nodea == edge_invalid or edge.nodeb == edge_invalid:
                continue
            if edge.nodea not in valid_nodes and edge.nodeb not in valid_nodes:
                continue
            
            pos_node = [self.nodes[edge.nodea].pos]
            for pos in self.geo(edge.geo):
                pos_node.append(pos)
            n = len(pos_node)
            dist = 0
            dist_node = [0]
            
            for i in range(n - 1):
                dist += self.dist_air(pos_node[i], pos_node[i + 1])
                dist_node.append(dist)
            
            for i in range(1, n - 1):
                d = self.dist_air(pos, pos_node[i])
                if d < min:
                    min = d
                    id = edge_id
                    edge_pos = dist_node[i] / dist
                    is_node = False
            
            # check each line between nodes
            a = pos_node[0]
            for i in range(1, n):
                b = pos_node[i]
                dlat = (b[0] - a[0]) * self.scale_lat
                dlon = (b[1] - a[1]) * self.scale_lon
                dplat = (pos[0] - a[0]) * self.scale_lat
                dplon = (pos[1] - a[1]) * self.scale_lon
                dir = abs(dlat) > abs(dlon)
                if dir:
                    r = (dplat + dplon*dlon / dlat) / (dlat + dlon*dlon / dlat)
                else:
                    r = (dplon + dplat*dlat / dlon) / (dlon + dlat*dlat / dlon);
                if r > 0 and r < 1:
                    if dir:
                        d = (dplon - dlon*r) / dlat
                    else:
                        d = (dplat - dlat*r) / dlon
                    d = abs(d) * self.dist_air(a, b)
                    if(d < min):
                        # update lowest
                        min = d
                        id = edge_id
                        edge_pos = (dist_node[i - 1] + r * (dist_node[i] - dist_node[i - 1])) / dist
                        is_node = False
                a = b

        return graph_pos(id, edge_pos, is_node)
    
    def dist_road(self, a, b):
        # calculate positions on graph
        s = self.pos(a)
        t = self.pos(b)
        dist = [dist_invalid for i in range(len(self.nodes))]
        open = []
        firstnode = node_invalid
        r = dist_invalid
        
        # A*
        if t.is_node:
            dist[t.id] = 0
            heappush(open, (0, t.id))
        else:
            edge = self.edges[t.id]
            if edge.flags & 1:
                dist[edge.nodea] = edge.dist * t.edge_pos
                heappush(open, (0, edge.nodea))
            if edge.flags & 2:
                dist[edge.nodeb] = edge.dist * (1 - t.edge_pos)
                heappush(open, (0, edge.nodeb))
        
        while True:
            if(len(open) == 0):
                return r
            # get current node
            est, node = heappop(open)
            
            if s.is_node and s.id == node:
                # node reached
                return dist[node]
            if est >= r:
                # optimality proven
                return r
            
            # iterate adjacent nodes
            id = self.nodes[node].edge
            while id != edge_invalid:
                edge = self.edges[id]
                is_nodea = edge.nodea == node
                # skip misaligned one-ways
                if (is_nodea and edge.flags & 2) or (edge.nodeb == node and edge.flags & 1):
                    if not s.is_node and s.id == id:
                        # path found
                        d = dist[node] + edge.dist * (s.edge_pos if is_nodea else 1 - s.edge_pos)
                        if firstnode == node_invalid:
                            r = d
                            firstnode = node
                            # terminate early if path has to be optimal
                            if (edge.flags & 3) < 3:
                                return r
                            if is_nodea and edge.linkb == edge_invalid and self.nodes[edge.nodeb].edge == id:
                                return r
                            if not is_nodea and edge.linka == edge_invalid and self.nodes[edge.nodea].edge == id:
                                return r
                        else:
                            if d < r:
                                r = d
                                firstnode = node
                            return r
                    
                    other = edge.nodeb if is_nodea else edge.nodea
                    newdist = dist[node] + edge.dist
                    if newdist < dist[other]:
                        # update distance
                        if dist[other] == dist_invalid:
                            heappush(open, (newdist + self.dist_air(a, self.nodes[other].pos), other))
                        dist[other] = newdist
                
                # select next edge
                if is_nodea:
                    id = edge.linka
                else:
                    id = edge.linkb


pyson.get_logger("pyson.mapc2017").setLevel(logging.ERROR)
actions = pyson.Actions(pyson.mapc2017.actions)

# internal actions

@actions.add_function(".floor", (float))
def _floor(val):
    return floor(val)

@actions.add_function(".ceil", (float))
def _ceil(val):
    return ceil(val)

actions.add_function(".sum", (tuple, ), sum)

@actions.add(".init_map", 1)
def _init_map(self, term, intention):
    try:
        arg = pyson.grounded(term.args[0], intention.scope)
        if pyson.is_atom(arg):
            str = arg.functor
        else:
            str = arg
        global map
        map = graph(str)
        yield
    except:
        pass

@actions.add_function(".dist", (float, float, float, float))
def _dist(lat1, lon1, lat2, lon2):
    try:
        return map.dist_air((lat1, lon1), (lat2, lon2)) / 1000
    except:
        return haversine((lat1, lon1), (lat2, lon2)) * 1000
    
@actions.add_function(".dist_road", (float, float, float, float))
def _dist_road(lat1, lon1, lat2, lon2):
    try:
        return map.dist_road((lat1, lon1), (lat2, lon2)) / 1000
    except:
        return haversine((lat1, lon1), (lat2, lon2)) * 1500
    
# check whether agent is still able to perform an action this step
@actions.add(".ready", 0)
def _busy(self, term, intention):
    if self.action_id is not None:
        yield

# list concatenation
@actions.add_function(".concatL", (tuple, tuple))
def _concatL(L1, L2):
    return L1+L2

# list difference
@actions.add_function(".removeL", (tuple, tuple))
def _removeL(L1, L2):
    return tuple([l for l in L1 if l not in L2])

@actions.add(".eq", 2)
def _eq(self, term, intention):
    # compare strings with atoms
    arg1 = pyson.grounded(term.args[0], intention.scope)
    arg2 = pyson.grounded(term.args[1], intention.scope)
    if pyson.is_atom(arg1):
        str1 = arg1.functor
    else:
        str1 = arg1
    if pyson.is_atom(arg2):
        str2 = arg2.functor
    else:
        str2 = arg2
    if str1 == str2:
        yield
    
@actions.add(".str", 2)
def _str(self, term, intention):
    arg = pyson.grounded(term.args[0], intention.scope)
    if pyson.is_atom(arg):
        str = arg.functor
    else:
        str = arg
    if pyson.unify(term.args[1], str, intention.scope, intention.stack):
        yield


@actions.add(".turn", 1)
def _turn(self, term, intention):
    # map current step to an agent
    name_ = tname[0].lower() + tname[1:]
    step = pyson.grounded(term.args[0], intention.scope) & 31
    turn = ((step << 3) + (step >> 2)) & 31
    if name_ + str(turn) == self.name:
        yield

@actions.add(".strB", 2)
def _strB(self, term, intention):
    # convert agent name to username
    arg = pyson.grounded(term.args[0], intention.scope)
    if pyson.is_atom(arg):
        str = arg.functor
    else:
        str = arg
    if tname[0].isupper():
        if pyson.unify(term.args[1], str[0].upper() + str[1:], intention.scope, intention.stack):
            yield
    else:
        if pyson.unify(term.args[1], str, intention.scope, intention.stack):
            yield

@actions.add(".knapsack", 4)
def _knapsack(self, term, intention):
    # distribute items onto agents
    item_infos = pyson.grounded(term.args[0], intention.scope)
    offers = pyson.grounded(term.args[1], intention.scope)
    
    items = []
    weights = []
    agents = []
    costs = []
    caps = []
    
    for info in item_infos:
        item, amount, weight = info.args
        for i in range(amount):
            items.append(item)
            weights.append(weight)
    for offer in offers:
        agl, cost, cap = offer.args
        ag = agl.functor
        agents.append(ag)
        costs.append(cost)
        caps.append(cap)
    
    m = len(items)
    total = sum(weights)
    incc = 10000
    incd = []
    n = len(agents)
    for r in range(1, 5):
        for c in combinations(range(n), r):
            cost = max(costs[ag] for ag in c) + 0.6*sum(costs[ag] for ag in c)
            if cost >= incc:
                continue
            if sum(caps[ag] for ag in c) < total:
                continue
            for dist in product(range(r), repeat=m):
                possible = True
                curd = [[] for ag in range(n)]
                for i in range(r):
                    ag = c[i]
                    curd[ag] = [it for it in range(m) if dist[it] == i]
                    load = sum([weights[it] for it in range(m) if dist[it] == i])
                    if load > caps[ag]:
                        possible = False
                        break
                if possible:
                    incc = cost
                    incd = curd
    
    plan = []
    if incc < 10000:
        for ag in range(n):
            if(incd[ag] == []):
                continue
            alloc = []
            amounts = [0 for i in range(m)]
            for it in incd[ag]:
                amounts[it] += 1
            for it in range(m):
                if amounts[it] > 0:
                    alloc.append(Literal("parts", (items[it], amounts[it])))
            if alloc != []:
                plan.append(Literal("ag", (Literal(agents[ag]), tuple(alloc))))
        pyson.unify(term.args[2], tuple(plan), intention.scope, intention.stack)
        pyson.unify(term.args[3], incc, intention.scope, intention.stack)
        yield

@actions.add(".restart")
def _restart(self, term, intention):
    os.system("start py agents.py " + sys.argv[1] + " " + sys.argv[2] + " " + sys.argv[3])
    asyncio.get_event_loop().stop()
    while True:
        time.sleep(10000)
    sys.exit()


async def main():
    env = pyson.mapc2017.Environment()
    host = sys.argv[1]
    global tname
    tname = sys.argv[2]
    password = sys.argv[3]
    
    # assign and compile agents
    def ag(source, id):
        return env.build_agent(open(source), actions, pyson.mapc2017.Agent, (tname + str(int(id) + 1)))
    
    print("building agents...")
    agent = [0 for i in range(28)]
    for i in [20]:
        agent[i] = ag("leader.asl", i)
    for i in [0, 1, 4, 12]:
        agent[i] = ag("builder.asl", i)
    for i in [2, 5, 6, 7, 13, 14, 15, 21, 22, 23]:
        agent[i] = ag("deliverer.asl", i)
    for i in [3, 8, 9, 10, 11, 16, 17, 18, 19, 24, 25, 26, 27]:
        agent[i] = ag("supplier.asl", i)
    
    # connect to server
    print("connecting agents...")
    for i in range(28):
        await agent[i].connect(tname + str(i + 1), password, host, port)


if __name__ == "__main__":
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(main())
        loop.run_forever()
    finally:
        loop.close()
