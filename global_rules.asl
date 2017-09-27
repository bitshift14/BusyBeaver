// ********************************
// *       belief wrappers        *
// ********************************

my_role(Name, Speed, Load, Battery, Tools) :- role(Name, Speed, Load, Battery, Tools)[source(percept)].
other_role(Ag, Name, Speed, Load, Battery, Tools) :- role_(Name, Speed, Load, Battery, Tools)[source(Ag)] & Ag \== self & Ag \== percept.
// any resource node the team has discovered
resource_node(Name, Lat, Lon, Res) :- resourceNode_(Name, Lat, Lon, Res).

/**
 * amount in inventory (or 0 if not present)
 * item\2 here is equivalent to hasItem\2 in eismassim
 */
item_(Item, Amount) :-
    item(Item, _, _, _) & (
        item(Item, Amount) |
        not item(Item, _) &
            Amount = 0
    ) &
true.

/**
 * any facility
 */
facility(Name, Lat, Lon) :-
    chargingStation(Name, Lat, Lon, _) |
    shop(Name, Lat, Lon, _, _) |
    workshop(Name, Lat, Lon) |
    storage(Name, Lat, Lon, _, _, _) |
    dump(Name, Lat, Lon) |
    resource_node(Name, Lat, Lon, _) |
false.

/**
 * job, auction or mission
 */
any_job(Job, Storage, Reward, Start, End, Required)  :-
    job(Job, Storage, Reward, Start, End, Required) |
    auction(Job, Storage, Max, Start, End, Fine, Lowest, Time, Required) &
        .min([Max, Lowest], Reward) |
    mission(Job, Storage, Reward, Start, End, Fine, Bid, Time, Required) |
false.


// ********************************
// *         item rules           *
// ********************************

free_load(LoadF) :-
    load(LoadC) &
    LoadR = 0 & 
    my_role(_, _, LoadM, _, _) &
    LoadF = LoadM - LoadC - LoadR &
true.

/**
 * check whether item is carryable
 */
carryable(Item, Amount) :-
    free_load(LoadF) &
    carryable(Item, Amount, LoadF) &
    true.
    carryable(Item, Amount, Max) :-
        item(Item, Volume, _, []) &
        TVol = Volume * Amount & TVol <= Max &
true.

/**
 * calculate how many items of that type are carryable
 */
carryable_max(Item, Cap) :-
    free_load(LoadF) &
    carryable_max(Item, Cap, LoadF) &
    true.
    carryable_max(Item, Cap, Max) :-
        item(Item, Volume, _, []) &
        Cap_ = Max / Volume &
        .floor(Cap_, Cap) &
true.

/**
 * get total volume of an item list
 * L = [parts(Item1, Amount1), ...]
 */
total_volume(L, Volume) :-
    .findall(Vol, total_volume_(L, Vol), LV) &
    .sum(LV, Volume) &
    true.
    total_volume_(L, Volume) :-
        .member(parts(Item, Amount), L) &
        item(Item, Vol, Tools, Parts) &
        Volume = Vol * Amount &
true.

/**
 * find usable tools
 */
usable_tools(Tools, Usable, Unusable) :-
    Tools = [] &
        Usable = [] &
        Unusable = [] |
    Tools \== [] &
        Tools = [Tool|T] &
        usable_tools(T, U2, NU2) &
        my_role(_, _, _, _, ToolsR) & (
            .member(Tool, ToolsR) &
                .concatL([Tool], U2, Usable) &
                Unusable = NU2 |
            not .member(Tool, ToolsR) &
                Usable = U2 &
                .concatL([Tool], NU2, Unusable)
        ) &
true.

/**
 * Tools = [Tool1, ...]
 * Parts = [parts(Tool1, 1), ...]
 */
tools_to_parts(Tools, Parts) :-
        Tools = [] &
            Parts = [] |
        Tools \== [] &
            .nth(0, Tools, Tool) &
            .tail(Tools, Tail) &
            tools_to_parts(Tail, P2) &
            .concatL([parts(Tool, 1)], P2, Parts) &
true.

/**
 * remove duplicate tools
 * L = [Tool1, ...]
 */
merge_tools(L, LM) :-
    .findall(Tool, merge_tools_(L, Tool), LM) &
    true.
    merge_tools_(L, Tool) :-
        item(Tool, _, _, _) &
        .findall(Tool, merge_tools__(Tool, L), T) &
        T \== [] &
        true.
        merge_tools__(Tool, L) :-
            .member(Tool, L) &
true.

/**
 * an item in inventory with associated load
 */
heavy_item(Item, Load) :-
    item(Item, Amount) &
    item(Item, Volume, _, _) &
    Load = Amount * Volume &
true.


// ********************************
// *  facilities and positioning  *
// ********************************

/**
 * check whether agent is in range of a facility or location
 */
in_range(Fac) :-
    facility(Fac, Lat, Lon) &
    in_range(Lat, Lon) &
true.
in_range(Lat, Lon) :-
    lat(Lat2) &
    lon(Lon2) &
    Lat == Lat2 &
    Lon == Lon2 &
true.


/**
 * calculate distance to another location
 */
dist(Lat, Lon, Dist) :-
    lat(Lat2) &
    lon(Lon2) &
    .dist(Lat, Lon, Lat2, Lon2, Dist__) & (
        my_role(drone, _, _, _, _) & Dist = Dist__ |
        not my_role(drone, _, _, _, _) & Dist = Dist__ * 1.5) &
true.

/**
 * calculate the number of goto actions needed to reach a location or facility [from another location]
 */
steps(Lat, Lon, Steps) :-
    dist(Lat, Lon, Dist) &
    cell_size(CS) &
    my_role(_, Speed, _, _, _) &
    .ceil(Dist / (CS * Speed), Steps) &
true.
steps(Fac, Steps) :-
    facility(Fac, Lat, Lon) &
    steps(Lat, Lon, Steps) &
true.
//    from      to
steps(Lat, Lon, Lat2, Lon2, Steps) :-
    .dist(Lat, Lon, Lat2, Lon2, Dist) &
    cell_size(CS) &
    my_role(_, Speed, _, _, _) &
    .ceil(Dist / (CS * Speed), Steps) &
true.

steps_to_cs(Lat, Lon, LatCS, LonCS, Steps) :-
    chargingStation(Name, LatCS, LonCS, _) &
    steps(Lat, Lon, LatCS, LonCS, Steps) &
true.

/**
 * check whether location is in battery range
 */
in_battery_range(Lat, Lon) :-
    battery_steps(StepsB) &
    steps(Lat, Lon, StepsT) &
    steps_to_cs(Lat, Lon, _, _, StepsCS) &
    StepsB >= StepsT + StepsCS &
    true.
    battery_steps(Steps) :-
        charge(Charge) &
        goto_cost(GC) &
        .floor(Charge / GC, Steps) &
true.

/**
 * fastest charging station to reach location
 */
best_cs(LatT, LonT, LatCS, LonCS) :-
    .findall(Steps, route_cs(LatT, LonT, LatCS, LonCS, Steps), L) &
    L \== [] &
    .min(L, Steps) &
    route_cs(LatT, LonT, LatCS, LonCS, Steps) &
    true.
    route_cs(LatT, LonT, LatCS, LonCS, Steps) :-
        chargingStation(_, LatCS, LonCS, Rate) &
        battery_steps(StepsB) &
        steps(LatCS, LonCS, StepsCS) &
        StepsB >= StepsCS &
        charge(Charge) &
        my_role(_, _, _, CMax, _) &
        .ceil((CMax - Charge) / Rate, StepsC) &
        steps(LatCS, LonCS, LatT, LonT, StepsT) &
        Steps = StepsCS + StepsC + StepsT &
true.

/**
 * find shop with buyable amount of an item (i.e. excluding reserved items)
 */
stocked(Shop, Item, Amount) :-
    stocked_max(Shop, Item, Max) &
    Amount < Max &
true.
stocked_max(Shop, Item, Amount) :-
    shop(Shop, _, _, _, Items) &
    .member(item(Item, _, AmountS), Items) &
    .findall(Am, shop_reserved(ID, Shop, Item, Am), L) &
    .sum(L, Res) &
    AmountS - Res = Amount &
    Amount > 0 &
true.

/**
 * find fastest route to buy an item and reach workshop
 */
nearest_shop(Shop, Item, Amount, Workshop, Steps) :-
    .findall(shop(Steps, Name), route_shop(Name, Item, Amount, Workshop, Steps), L) &
    L \== [] &
    .min(L, shop(Steps, Shop)) &
    true.
    route_shop(Shop, Item, Amount, Workshop, Steps) :-
        shop(Shop, LatS, LonS, _, _) &
        stocked(Shop, Item, Amount) &
        workshop(Workshop, LatW, LonW) & steps(LatS, LonS, StepsS) &
        steps(LatS, LonS, LatW, LonW, StepsW) &
        Steps = StepsS + StepsW &
true.


