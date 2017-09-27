{ include("global_beliefs.asl") }
leader.
cnp_id(1).

{ include("global_rules.asl") }

// ********************************
// *       belief wrappers        *
// ********************************

/**
 * amount of an item available from an agent (including 0)
 */
item_available_(Item, Amount, Ag) :-
    item(Item, _, _, _) &
    step(Step) & (
        item_available(Step, Ag, L) &
        .member(parts(Item, Amount), L) |
        not (item_available(Step, Ag, L) &
        .member(parts(Item, _), L)) &
            Amount = 0
    ) &
true.

reserved_job(Job) :-
    reserved_jobs(R) &
    .member(Job, R) &
true.




// ********************************
// *          item rules          *
// ********************************

/**
 * multiplicate all amounts in a list of items with a factor
 * L = [parts(Item1, Amount1), ...]
 */
mult_parts(L, Factor, M) :-
    .findall(parts(Item, Amount), mult_parts_(L, Factor, Item, Amount), M) &
    true.
    mult_parts_(L, Factor, Item, Amount) :-
        .member(parts(Item, Am), L) &
        .ceil(Am * Factor, Amount) &
true.

/**
 * merge duplicate items in and remove nonpositive amounts from list, i.e.
 * [parts(item0, 2), parts(item1, 0), parts(item0, 3)] --> [parts(item0, 5)]
 */
merge_parts(L, LM) :-
    .findall(parts(Item, Amount), merge_parts_(L, Item, Amount), LM) &
    true.
    merge_parts_(L, Item, Amount) :-
        item(Item, _, _, _) &
        .findall(Am, merge_parts__(L, Item, Am), Ams) &
        .sum(Ams, Amount) &
        Amount > 0 &
        true.
        merge_parts__(L, Item, Amount) :-
            .member(parts(Item, Amount), L) &
true.

/**
 * get base items and tools for item list
 * Items = [parts(Item1, Amount1), ...]
 * Base = [parts(Base1, AmountB1), ...]
 * Tools = [Tool1, ...]
 * Difficulty (sometimes referred to as assembleValue) represents the number of
 *  assemble-actions needed to craft all items from base items
 */
base_items(Items, Base, Tools, Difficulty) :-
    base_items_(Items, B, T, Difficulty) &
    merge_parts(B, Base) &
    merge_tools(T, Tools) &
    true.
    base_items_(Items, Base, ToolsB, Difficulty) :- 
        /* empty list */
        Items = [] &
            Base = [] &
            ToolsB = [] &
            Difficulty = 0 |
        Items \== [] &
            // get first element
            .nth(0, Items, parts(Item, AmountP)) &
            base_cache(Item, LC, T1, DC) &
            mult_parts(LC, AmountP, L1) &
            D1 = DC * AmountP &
            // process tail 
            .tail(Items, T) &
            base_items_(T, L2, T2, D2) &
            // merge results
            .concatL(L1, L2, Base) &
            .concatL(T1, T2, ToolsB) &
            Difficulty = D1 + D2 &
true.

// original version for initializing base_cache
base_items__(Items, Base, Tools, Difficulty) :-
    base_items___(Items, 1, L, T, Difficulty) &
    merge_parts(L, Base) &
    merge_tools(T, Tools) &
    true.
    base_items___(Items, Amount, Base, ToolsB, Difficulty) :-
        /* empty list */
        Items == [] &
            Base = [] &
            ToolsB = [] &
            Difficulty = 0 |
        Items \== [] &
            // get first element and tail
            Items = [parts(Item, AmountP)|Tail] &
            // calculate total amount
            AmountT = Amount * AmountP &
            // get item info
            item(Item, _, Tools, Parts) & (
            Parts = [] &
                // base item
                B1 = [parts(Item, AmountT)] &
                T1 = [] &
                D1 = 0 |
            Parts \== [] &
                // recurse if item has to be assembled
                base_items___(Parts, AmountT, B1, TSub, DSub) &
                D1 = DSub + AmountT &
                .concatL(TSub, Tools, T1)
            ) &
            // process tail 
            base_items___(Tail, Amount, B2, T2, D2) &
            // merge results
            .concatL(B1, B2, Base) &
            .concatL(T1, T2, ToolsB) &
            Difficulty = D1 + D2 &
true.

/**
 * depth of an element in the item tree
 */
item_level_(Item, Level) :-
    item(Item, _, _, Parts) & (
        Parts = [] &
            Level = 0 |
        Parts \== [] &
            .findall(L2, item_part_level(Parts, L2), L) &
            .max(L, LS) &
            Level = LS + 1
    ) &
    true.
    item_part_level(P, L) :-
        .member(parts(Item, _), P) &
        item_level_(Item, L) &
true.

/**
 * LReq = [reuired(Item1, Amount1), required(Item2, Amount2), ...]
 * LParts = [parts(Item1, Amount1), parts(Item2, Amount2), ...]
 */
req_to_parts(LReq, LParts) :-
    LReq = [] &
        LParts = [] |
    LReq \== [] &
        .nth(0, LReq, required(Item, Amount)) &
        .tail(LReq, T) & req_to_parts(T, L2) &
        .concatL([parts(Item, Amount)], L2, LParts) &
true.

/**
 * available or promised items
 */
item_total(Item, Amount) :-
    item(Item, _, _, _) &
    reserved(Item, Res) &
    item_(Item, Stock) &
    Amount = Res + Stock &
    Amount > 0 &
    true.
    reserved(Item, Amount) :-
        .findall(Am, item_promise(ID, Item, Am), P) &
        .sum(P, Prom) &
        .findall(Am, available_items_(Item, Am, S), A) &
        .sum(A, Av) &
        Amount = Prom + Av &
        true.
        promised_items(L) :-
            .findall(parts(Item, Amount), item_promise(ID, Item, Amount), P) &
            merge_parts(P, L) &
        true.
        available_items(L) :-
            .findall(parts(Item, Amount), available_items_(Item, Amount, S), A) &
            merge_parts(A, L) &
        true.
        available_items_(Item, Amount, S) :-
            step(Step) &
            item_available(Step, S, I) &
            .member(parts(Item, Amount), I) &
true.
item_total_(Item, Amount) :-
    reserved(Item, Res) &
    item_(Item, Stock) &
    Amount = Res + Stock &
true.

/**
 * base items needed to buy
 */
base_needed(L) :-
    .findall(parts(Item, Amount), base_needed(Item, Amount), L) &
    true.
    base_needed(Item, Amount) :-
        required_base(B) &
        .member(parts(Item, Req), B) &
        item_total_(Item, Av) &
        Amount = Req - Av &
        Amount > 0 &
true.

/**
 * alloacte agents for assembly of an item
 */
assemblable(Main, Item, Al, Rem) :-
    item(Item, _, Tools, Parts) &
    tools_to_parts(Tools, T) &
    .concatL(T, Parts, Req) &
    step(Step) &
    available_agents(Step, AG) &
    prune_req(Req, Main, ReqM) &
    allocate(Main, ReqM, AG, Al, Rem) &
    true.
    prune_req(Req, Ag, ReqN) :-
        Req = [] &
            ReqN = [] |
        Req \== [] &
            .nth(0, Req, parts(Item, Amount)) &
            .tail(Req, RT) & (
                .my_name(Ag) & 
                    item_(Item, Av) |
                step(Step) &
                not .my_name(Ag) & (
                    item_available(Step, Ag, L) |
                    not item_available(Step, Ag, _) & L = [] 
                ) & (
                    .member(parts(Item, Av), L) |
                    not .member(parts(Item, Av), L) & Av = 0
            )) & (
                Av >= Amount & RN1 = [] |
                Av < Amount & RN1 = [parts(Item, Amount - Av)]
            ) &
            prune_req(RT, Ag, RN2) &
            .concatL(RN1, RN2, ReqN) &
    true.
    allocate(Main, Req, AV, Al, Rem) :-
        Req = [] &
            Al = [] &
            Rem = AV |
        AV \== [] &
            .nth(0, AV, Ag) &
            .tail(AV, AVT) & (
                Ag == Main &
                    Al1 = [] &
                    Rem1 = [] &
                    ReqN = Req |
                Ag \== Main &
                    prune_req(Req, Ag, ReqN) & (
                        Req == ReqN &
                            Al1 = [] &
                            Rem1 = [Ag] |
                        Req \== ReqN &
                            Al1 = [Ag] &
                            Rem1 = []
            )) &
            allocate(Main, ReqN, AVT, Al2, Rem2) &
            .concatL(Al1, Al2, Al) &
            .concatL(Rem1, Rem2, Rem) &
true.


// ********************************
// *          job rules           *
// ********************************

/**
 * item still needed for deliver plan
 */
required_item_for_job(Ag, It, Item, Amount) :-
    .member(parts(Item, AmN), It) &
    item_available_(Item, AmA, Ag) &
    Amount = AmN - AmA &
    Amount > 0 &
true.

/**
 * assemblies needed to construct required items from available items
 */
items_buildable(Items, Asm) :-
    .findall(parts(Item, Am), available_items_(Item, Am, S), A) &
    .findall(parts(Item, Am), item(Item, Am), S) &
    .concatL(A, S, AvT) &
    merge_parts(AvT, Av) &
    //.print("IB:", Items, Av) &
    items_buildable_(Items, Av, Rem, Asm) &
    true.
    items_buildable_(Items, Av, Rem, Asm) :-
        Items = [] &
            Rem = Av &
            Asm = [] |
        Items \== [] &
            .nth(0, Items, parts(Item, AmR)) & (
                .member(parts(Item, AmA), Av) |
                not .member(parts(Item, _), Av) &
                    AmA = 0
            ) & (
                AmA >= AmR &
                    .concatL(Av, [parts(Item, -AmR)], AvT) &
                    merge_parts(AvT, AvN) &
                    AsmC = [] |
                AmA < AmR &
                    item(Item, _, _, Parts) &
                    Parts \== [] &
                    mult_parts(Parts, AmR - AmA, ItemsSub) &
                    .concatL(Av, [parts(Item, -(AmR - AmA))], AvT) &
                    merge_parts(AvT, AvSub) &
                    items_buildable_(ItemsSub, AvSub, AvN, AsmSub) &
                    .concatL(AsmSub, [Item], AsmC)
            ) &
            .tail(Items, T) &
            items_buildable_(T, AvN, Rem, AsmT) &
            .concatL(AsmC, AsmT, Asm) &
true.

/**
 * check whether all items for a job can be delivered
 */
job_in_item_bounds(Job) :-
    any_job(Job, _, _, _, _, Required) &
    not (
        .member(required(Item, AmR), Required) &
        not item_in_bounds(Item, AmR)
    ) &
    true.
    item_in_bounds(Item, AmR) :-
        item_level(Item, Level) &
        base_cache(Item, _, _, D) &
        D <= 10 &
        required_items(C) &(
            .member(parts(Item, AmC), C) |
            not .member(parts(Item, _), C) &
                AmC = 0
        ) & (
            AmR <= AmC
        ) &
true.

/**
 * check whether a job can be reserved, i.e. all required items can be built from available items
 */
job_feasible(Job) :-
    job_in_item_bounds(Job) &
    reserved_jobs(Jobs) &
    any_job(Job, _, _, _, _, Required) &
    req_to_parts(Required, Parts) &
    .concatL(Jobs, [Job], JN) &
    jobs_items(JN, Items) &
    items_buildable(Items, Asm) &
true.

/**
 * all items required for a list of jobs
 */
jobs_items(R, Items) :-
    R = [] &
        Items = [] |
    R \== [] &
        .nth(0, R, Job) & (
        deliver_plan(Job, Plan) &
            plan_items(Plan, Parts) |
        not deliver_plan(Job, Plan) &
            any_job(Job, _, _, _, _, Required) &
            req_to_parts(Required, Parts) 
        ) &
        .tail(R, T) &
        jobs_items(T, ItemsT) &
        .concatL(Parts, ItemsT, PartsN) &
        merge_parts(PartsN, Items) &
    true.
    plan_items(Plan, Items) :-
        .findall(parts(Item, Amount), plan_items_(Plan, Ag, Item, Amount), P) &
        merge_parts(P, Items) &
        true.
        plan_items_(Plan, Ag, Item, Amount) :-
            .member(ag(Ag, It), Plan) &
            .member(parts(Item, Amount), It) &
true.

req_asm(Asm) :-
    reserved_jobs(R) &
    jobs_items(R, Items) &
    items_buildable(Items, Asm) &
true.

/**
 * maximum base item cost and assemble value for a job
 */
job_cost(Job, CMax, AV) :-
    any_job(Job, _, _, _, _, Required) &
    req_to_parts(Required, Parts) &
    base_items(Parts, Base, _, D) &
    .findall(Cost, base_item_cost_ub(Base, Item, Amount, Cost), L) &
    .sum(L, CMax) &
    AV = D * 100 &
    true.
    base_item_cost_ub(Parts, Item, Amount, Cost) :-
        .member(parts(Item, Amount), Parts) &
        cost_ub(Item, CPU) &
        Cost = Amount * CPU &
true.

/**
 * evaluate job profit
 */
any_job_rating(Job, Rating) :-
    any_job(Job, _, Reward, _, _, _) &
    not job_processed(Job) &
    job_cost(Job, CMax, AC) &
    Rating = Reward - CMax &
    Rating > 0 &
true.
job_rating(Job, Rating) :-
    job(Job, _, _, _, _, _) &
    any_job_rating(Job, Rating) &
true.
auction_rating(Job, Rating) :-
    auction(Job, _, _, _, _, _, _, _, _) &
    any_job_rating(Job, Rating) &
true.


// ********************************
// * recursion or .findall rules  *
// ********************************

workshop_dist(Workshop, Dist) :-
    workshop(Workshop, LatW, LonW) &
    .findall(Dist, storage_dist(Workshop, Storage, Dist), L) &
    .sum(L, Dist) &
    true.
    storage_dist(Workshop, Storage, Dist) :-
        workshop(Workshop, LatW, LonW) &
        storage(Storage, LatS, LonS, _, _, _) &
        .dist(LatW, LonW, LatS, LonS, Dist) &
true.

shop_price(Shop, Item, Cost) :-
    shop(Shop, _, _, _, Items) &
    .member(item(Item, Cost, _), Items) &
true.

builder_load(Ag, Load) :-
    builder_[source(Ag)] &
    other_role(Ag, _, _, Load, _, _) &
true.

initial_allocation(Step, A, F) :-
    A = [] &
    .findall(S, item_available(Step, S, _), IA) &
    .my_name(Self) &
    .concatL(IA, [Self], F) &
true.

part_member(Item, Amount, L) :-
    .member(parts(Item, Amount), L) |
    not .member(parts(Item, _), L) &
        Amount = 0 &
true.

member_desc(Member, L) :-
    .length(L, Len) &
    Len > 0 &
    .nth(Len - 1, L, Last) & (
         Member = Last |
        .removeL(L, [Last], T) &
        member_desc(Member, T)
    ) &
true.

plan_member(Plan, Ag, It) :-
    .member(ag(Ag, Parts), Plan) &
    merge_parts(Parts, It) &
true.

available_role(Role, Ag) :-
    deliverer_available(Ag) &
    other_role(Ag, Role, _, _, _, _) &
true.

job_item_volume(Required, Item, Amount, Volume) :-
    .member(required(Item, Amount), Required) &
    item(Item, Volume, _, _) &
true.

deliver_offer(ID, Ag, Steps, Free, Max) :-
    deliver_propose(ID, Steps, Free)[source(Ag)] &
    Steps < Max &
true.





{ include("global_plans.asl") }

+connected(Username) <-
    .print("Connected");
true.

-connected(_) <-
    .dump;
true.

+step(Step) : not initializing & not initialized <-
    +initializing;
    !!init;
true.


+!add_reserved(Job) <-
    ?reserved_jobs(R);
    any_job(Job, Storage, Reward, Start, End, Required);
    req_to_parts(Required, Parts);
    .concatL(R, [Job], RN);
    -reserved_jobs(R);
    +reserved_jobs(RN);
true.

+!remove_reserved(Job) <-
    ?reserved_jobs(R);
    .member(Job, R);
    .removeL(R, [Job], RN);
    -reserved_jobs(R);
    +reserved_jobs(RN);
true.

+step(Step) : prephase <-
    !wait_ms(100);
    if(prephase) {
        .broadcast(tell, step_done(Step));
    }
true.


+job(Job, _, _, Start, _, _) : initialized & not job_init(Job) <-
    !rate_job(Job);
true.

+auction(Job, _, _, Start, _, _, _, _, _) : initialized & not job_init(Job) <-
    !rate_job(Job);
true.

+mission(Job, _, _, Start, _, _, _, _, _) : initialized & not job_init(Job) <-
    !rate_job(Job);
true.


+!rate_job(Job) : any_job(Job, Storage, Reward, Start, End, Required) <-
    +job_init(Job);
    if(job_cost(Job, CMax, AC)) {
        .print("+JOB", Job, Reward, CMax, AC, (Reward/(CMax+AC) - 1) * 100);
        +job_cost(Job, CMax, AC);
    } else {
        .print("JOB COST ERROR");
        //+job_processed(Job);
    }
true.


+!shop_allocation_cnp(ID, Shop) <-
true.

+!item_cnp(Item, Amount, Succ) <-
    ?cnp_id(ID);
    -cnp_id(ID);
    +cnp_id(ID + 1);
    ?step(Step);
    +cnp_step(ID, Step);
    .broadcast(achieve, item_cfp(ID, Item, Amount));
    !item_cnp_wait(ID);
    .findall(offer(Val, Ag), item_propose(ID, Val)[source(Ag)], L);
    if(L \== []) {
        .min(L, offer(_, WAg));
        .broadcast(tell, item_cnp_winner(ID, WAg));
        Succ = Amount;
    } else {
        .findall(offer(Am, -Val, Ag), item_partial(ID, Val, Am)[source(Ag)], P);
        if(P \== []) {
            .max(P, offer(MAm, _, WAg));
            .broadcast(tell, item_cnp_winner(ID, WAg));
            Succ = MAm;
        } else {
            Succ = 0;
        }
    }
    while(item_propose(ID, Val)) {
        -item_propose(ID, Val);
    }
    while(item_partial(ID, Val, Am)) {
        -item_partial(ID, Val, Am);
    }
    while(item_refuse(ID)) {
        -item_refuse(ID);
    }
    // give other agents enough time to sort their stuff out
    !wait_ms(10);
true.

+!item_cnp_wait(ID) <-
    while(.count(item_propose(ID, _), CProp) & .count(item_partial(ID, _, _), CPart) & .count(item_refuse(ID), CRef)
            & .count(supplier_[source(_)], CAg) & CProp + CRef + CPart < CAg) {
        !wait_ms(1);
    }
true.

+!deliver_cnp(Job) : any_job(Job, Storage, Reward, Start, End, Required) <-
    ?cnp_id(ID);
    -cnp_id(ID);
    +cnp_id(ID + 1);
    .broadcast(achieve, deliver_cfp(ID, Job));
    .findall(item(Item, Am, Vol), job_item_volume(Required, Item, Am, Vol), Items);
    step(Step);
    Max = End - Step - 5;
    !deliver_cnp_wait(ID);
    .findall(offer(Ag, Steps, Free), deliver_offer(ID, Ag, Steps, Free, Max), Offers);
    if(.knapsack(Items, Offers, Plan, Cost)) {
        .findall(ag(Ag, It), plan_member(Plan, Ag, It), PlanN);
        .print("Plan:", PlanN);
        +deliver_plan(Job, PlanN);
        +plan_step(Job, Step);
        .findall(Ag, plan_member(Plan, Ag, It), L);
        .broadcast(tell, deliver_cnp_winner(ID, L));
        +job_processed(Job);
    } else {
        .print("knapsack fail", Offers);
    }
    while(deliver_propose(ID, Steps, Free)) {
        -deliver_propose(ID, Steps, Free);
    }
    while(deliver_refuse(ID)) {
        -deliver_refuse(ID);
    }
true.

+!deliver_cnp_wait(ID) <-
    while(.count(deliver_propose(ID, _, _), CProp) & .count(deliver_refuse(ID), CRef)
            & .count(deliverer_[source(_)], CAg) & CProp + CRef < CAg) {
        !wait_ms(1);
    }
true.

+!remove_available(Ag) <-
    step(Step);
    ?available_agents(Step, A);
    .removeL(A, [Ag], AN);
    -available_agents(Step, A);
    +available_agents(Step, AN);
    not (
        available_agents(Ags) &
        .member(Ag, Ags)
    );
true.

+!timeout_guard_main(Step) <-
    if(not step(Step)) {
        .print("TIMEOUT LAG");
    }
    -log;
    if(Step > 5) {
        !wait_ms(3000);
        if(.ready & step(Step)) {
            +timeout(Step);
            .print("TIMEOUT SKIP");
            .skip;
            .broadcast(tell, step_done(Step));
            if(timeout(Step - 1)) {
                +log;
            }
            if(timeout(Step - 1) & timeout(Step - 2) & timeout(Step - 3)) {
                .print("TIMEOUT ABORT");
                .dump;
                .restart;
                while(true) {
                }
                .stopMAS;
            }
        }
    }
true.

/**
 * find suppliers to buy required base items
 */
+!restock <-
    if(step(Step) & not (
            first_step(Step) &
            Step > 5
    )) {
        ?base_needed(L);
        .print("requesting ", L);
        +cnp_loop(L);
        // do cnps for each base item
        while(cnp_loop(Items) & Items \== []) {
            Items = [parts(Item, Amount)|T];
            !item_cnp(Item, Amount, Succ);
            -cnp_loop(Items);
            if(0 < Succ & Succ < Amount) {
                .concatL(T, [parts(Item, Amount-Succ)], TN);
                +cnp_loop(TN);
            } else {
                +cnp_loop(T);
            }
        }
        while(cnp_loop(_)) {
            -cnp_loop(_);
        }
    }
    !wait_ms(100);
    ?required_base(B);
    for(.member(parts(Item, Amount), B)) {
        // resource gathering recommendation
        item_total_(Item, AmT);
        .broadcast(untell, resource_rating(Item, _));
        .broadcast(tell, resource_rating(Item, Amount * 1.5 - AmT));
    }
true.


/**
 * initialize map, main workshop, cache beliefs, tool distribution,
 * carried items, required items and move to main workshop
 */
+!init <-
    ?step(StepF);
    +first_step(StepF);
    // give other agents some time to initialize
    !wait_ms(200);
    // initialize map
    ?map(Map);
    .init_map(Map) | .print("INIT_MAP FAILED");
    // find central workshop
    .findall(workshop(Dist, Workshop), workshop_dist(Workshop, Dist), WD);
    .min(WD, workshop(_, Workshop));
    .broadcast(tell, main_workshop(Workshop));
    +main_workshop(Workshop);
    +relevant_tools([]);
    +carried_items([]);
    +reserved_jobs([]);
    
    // iterate items
    .findall(item(Item), item(Item, _, _, _), Items);
    .sort(Items, ItemsS);
    for(.member(item(Item), ItemsS) & item(Item, Volume, Tools, Parts)) {
        ?item_level_(Item, Level);
        +item_level(Item, Level);
        // initialize base cache
        ?base_items__([parts(Item, 1)], B, T, D);
        +base_cache(Item, B, T, D);
        // print some information
        .print(Item, " V =", Volume, " T =", Tools, " P =", Parts);
        if(Level > 1) {
            .print("       L =", Level, "-", D, " B =", T, B);
        }
        // only require tools actually needed in an assembly
        ?relevant_tools(RT);
        .concatL(RT, T, RT2) & merge_tools(RT2, RN);
        -relevant_tools(_);
        +relevant_tools(RN);
        if(Parts == []) {
            // initialize base item price bounds
            .findall(Cost, shop_price(Shop, Item, Cost), P);
            .min(P, LB);
            .min(P, UB);
            +cost_lb(Item, LB);
            +cost_ub(Item, UB)
        }
    }
    
    // print tools
    for(.member(Role, [truck, car, motorcycle, drone])) {
        other_role(_, Role, _, _, _, Tools);
        .print("Role Tools:", Role, Tools);
    }
    ?relevant_tools(T);
    .print("Relevant Tools:", T);
    +selectable_tools(T);
    // distribute tools among builders
    .findall(ag(Load, S), builder_load(S, Load), BL);
    .sort(BL, BLS);
    for(.member(ag(_, S), BLS)) {
        ?selectable_tools(ST);
        .send(S, tell, selectable_tools(ST));
        while(not other_tools(OT)[source(S)]) {
            !wait_ms(1);
        }
        ?other_tools(OT)[source(S)];
        -selectable_tools(ST);
        +selectable_tools(OT);
    }
    ?selectable_tools(M);
    if(my_role(truck, _, _, _, ToolsR) & (.member(Tool, M) & not .member(Tool, ToolsR))) {
        .print("TOOL SHORTAGE");
    }
    .print("Wielded Tools:", M);
    .broadcast(tell, truck_tools(M));
    tools_to_parts(M, MP) & total_volume(MP, MVol);
    
    // determine carried items
    +req_loop;
    while(req_loop) {
        ?carried_items(C);
        if( item_level(Item, 1) & part_member(Item, Amount, C) & Amount < 2
        |
            item_level(Item, 2) & part_member(Item, Amount, C) & Amount < 1 &
                item(Item, _, _, Parts) & 
                not (
                    .member(parts(Part, AmR), Parts) &
                    item_level(Part, 1) & AmR > 2
                )
        |
            item_level(Item, 1) & part_member(Item, Amount, C) & Amount < 3
        |
            item_level(Item, 2) & part_member(Item, Amount, C) & Amount < 2 &
                item_level(ItemB, 3) &
                item(ItemB, _, _, Parts) &
                .member(parts(Item, 2), Parts)
        |   
            item_level(Item, 2) & part_member(Item, Amount, C) & Amount < 2
        |
            item_level(Item, 1) & part_member(Item, Amount, C) & Amount < 4
        |
            item_level(Item, 2) & part_member(Item, Amount, C) & Amount < 3 &
                item_level(ItemB, 3) &
                item(ItemB, _, _, Parts) &
                .member(parts(Item, 3), Parts)

        |
            item_level(Item, 1) & part_member(Item, Amount, C) & Amount < 5
        ) {
            N = parts(Item, 1);
            .concatL(C, [N], CT);
            merge_parts(CT, CN);
            if(total_volume(CN, Vol) & my_role(_, _, LoadM, _, _) & Vol + MVol < LoadM) {
                -carried_items(C);
                +carried_items(CN);
            } else {
                -req_loop;
            }
        } else {
            -req_loop;
        }
        
    }
    
    ?carried_items(I);
    // determine required items
    +required_items(I);
    +req_loop;
    while(req_loop) {
        ?required_items(R);
        if( item_level(Item, Level) & part_member(Item, Amount, R) & Amount < 1 &
                Level > 1 &
                base_cache(Item, _, _, D) &
                D <= 10 &
                item(Item, _, _, Parts) &
                not (
                    .member(parts(ItemS, AmR), Parts) &
                    item_level(ItemS, LevelS) &
                    LevelS > 0 &
                    part_member(ItemS, AmC, I) &
                    AmR > AmC
                )
        |
            item_level(Item, 1) & part_member(Item, Amount, R) & Amount < 4
        |
            item_level(Item, 2) & part_member(Item, Amount, R) & Amount < 2 &
                item(Item, _, _, Parts) &
                not (
                    .member(parts(ItemS, AmR), Parts) &
                    item_level(ItemS, LevelS) &
                    LevelS > 0 &
                    part_member(ItemS, AmC, I) &
                    AmR > AmC
                )
        |
            item_level(Item, 2) & part_member(Item, Amount, R) & base_cache(Item, _, _, D) & D == 2 & Amount < 3 & 
                item(Item, _, _, Parts) &
                not (
                    .member(parts(ItemS, AmR), Parts) &
                    item_level(ItemS, LevelS) &
                    LevelS > 0 &
                    part_member(ItemS, AmC, I) &
                    AmR > AmC
                )
        ) {
            N = parts(Item, 1);
            .concatL(R, [N], RT);
            merge_parts(RT, RN);
            if(base_items(RN, Base, _, _) & total_volume(Base, Vol) & Vol < 10000) {
                -required_items(R);
                +required_items(RN);
            } else {
                -req_loop;
            }
        } else {
            -req_loop;
        }
        
    }
    ?required_items(R);
    // print carried and required items
    total_volume(I, IVol);
    total_volume(R, RVol);
    .print("Carried Items:", IVol, I);
    .print("Required Items:", RVol, R);
    base_items(R, B, _, _) & total_volume(B, BVol);
    +required_base(B);
    .print("Required Base Items:", B, BVol);
    .print("Init done");
    !restock;
    ?step(Step);
    .broadcast(tell, step_done(Step));
    +initialized;
    +prephase;
    -initializing;
    // buy tools
    for(.member(Tool, M)) {
        if(not item(Tool, _)) {
            nearest_shop(Shop, Tool, 1, Workshop, _);
            !goto(Shop);
            !buy(Tool, 1);
        }
    }
    !goto(Workshop);
    -prephase;
    ?step(StepE);
    -step(StepE);
    +step(StepE);
true.

/**
 * handle jobs and select actions for deliverers and self
 */
+step(Step) : initialized <-
    if(Step == 999) {
        .restart;
        .dump;
    }
    !!timeout_guard_main(Step);
    !collect_garbage;
    while(cnp_step(ID, Step2) & Step2 < Step - 80) {
        -cnp_step(ID, Step2);
        -item_promise(ID, _, _);
    }
    while(deliver_plan(Job, Plan) & plan_step(Job, Step2) & Step2 < Step - 30) {
        for(.member(ag(Ag, It), Plan)) {
            .send(Ag, tell, task(Step, fail));
        }
        -deliver_plan(Job, Plan);
        +job_processed(Job);
        !remove_reserved(Job);
    }
    ?money(Money);
    deliver_plan(JobC, _) | JobC = "none";
    .print("Step:", Step, " / Money:", Money, " / Job:", JobC);
    !wait_ms(100);
    
    // wait for item reports
    while(  providing(Ag) &
            not item_available(Step, Ag, _) |
            deliver_plan(Job, Plan) &
            .member(ag(Ag, It), Plan) &
            not item_available(Step, Ag, _)
    ) {
        !wait_ms(10);
    }
    // determine available agents
    ?initial_allocation(Step, _, Ags);
    .broadcast(tell, step_start(Step));
    .my_name(Self);
    +available_agents(Step, Ags);
    
    // dispatch fully loaded deliverers
    while(  deliver_plan(Job, L) &
            .member(ag(Ag, It), L) &
            not (
                .member(parts(Item, AmR), It) &
                item_available_(Item, AmA, Ag) &
                AmA < AmR
            )) {
        .send(Ag, tell, task(Step, deliver));
        -item_available(Step, Ag, _);
        .removeL(L, [ag(Ag, It)], LN);
        -deliver_plan(Job, L);
        !remove_available(Ag);
        .wait(5);
        if(LN \== []) {
            +deliver_plan(Job, LN);
        } else {
            +job_processed(Job);
            !remove_reserved(Job);
        }
    }
    
    // reserve jobs
    while((
        (   mission(Job, _, Reward, Start, _, _, _, Time, Required))
    |
        (   // prefer auctions over regular jobs
            reserved_jobs(Res) &
            .length(Res, Len) &
            Len <= 1 &
            .findall(job(Rating, Job), auction_rating(Job, Rating), L) &
            .sort(L, LS) &
            member_desc(job(_, Job), LS) &
            auction(Job, _, Reward, Start, End, _, _, Time, Required) &
            Step < Start + Time - 1 &
            Step < End - 25)
    |
        (   reserved_jobs(Res) &
            .length(Res, Len) &
            Len <= 1 &
            .findall(job(Rating, Job), job_rating(Job, Rating), L) &
            .sort(L, LS) &
            member_desc(job(_, Job), LS) &
            job(Job, _, Reward, Start, End, Required) &
            Step < End - 25)) &
        not job_processed(Job) &
        not reserved_job(Job) &
        job_feasible(Job)
    ) {
        .print("reserving", Job);
        !add_reserved(Job);
    }
    
    // bid on auctions
    while(  reserved_job(Job) &
            auction(Job, _, Reward, Start, _, _, Lowest, Time, Required) &
            not bid_on(Job) &
            Step == Start + Time - 1 &
            deliverer_available(Ag)) {
        .min([Reward * 0.9 - 2, Lowest - 1], Bid);
        .floor(Bid, BidF);
        if(job_cost(Job, Cost, AV) & Cost < BidF) {
            .send(Ag, achieve, bid_for_job(Job, BidF));
            +bid_on(Job);
        } else {
            +job_processed(Job);
            !remove_reserved(Job);
        }
        !wait_ms(10);
    }
    
    // unreserve nonexisting jobs
    while(  reserved_job(Job) &
            not any_job(Job, _, _, _, _, _)) {
        .print("unreserving", Job);
        +job_processed(Job);
        !remove_reserved(Job);
    }
    
    for(true){if(
            not deliver_plan(_, _) &
            deliverer_available(_) &
            reserved_job(Job) & (
                mission(Job, Storage, Reward, Start, End, Fine, Bid, Time, Required) |
                job(Job, Storage, Reward, Start, End, Required) |
                auction(Job, Storage, Reward, Start, End, Fine, Bid, Time, Required) &
                    Step > Start + Time 
            ) &
            not job_processed(Job)) {
        // select pursued job and deliverers to carry required items
        !deliver_cnp(Job);
        !wait_ms(40);
    }}
    
    // determine assemblies needed for reserved jobs
    req_asm(ReqAsm) | ReqAsm = [];
    if(deliver_plan(Job, L)) {
        for(.member(ag(Ag, It), L)) {
            !remove_available(Ag);
            .wait(5);
        }
        for(.member(ag(Ag, It), L)) {
            .strB(Ag, AgB);
            +tmp_job_items(Step, Ag, []);
            for(    required_item_for_job(Ag, It, Item, Amount) &
                    step(Step) &
                    item_available(Step, Prov, L) &
                    .member(parts(Item, Av), L) &
                    step(Step) &
                    avavailable_agents(Step, A) &
                    .member(Prov, A) &
                    Prov \== Ag &
                    tmp_job_items(Step, Ag, Tmp) & (
                        not .member(parts(Item, _), Tmp) &
                            .min([Amount, Av], AmP) |
                        .member(parts(Item, AmT), Tmp) &
                            .min([Amount - AmT, Av], AmP)
                    ) &
                    AmP > 0
            ) {
                // order existing item to be transferred to deliverer
                .send(Prov, tell, task(Step, give(AgB, Item, AmP)));
                !remove_available(Prov);
                .wait(5);
                .concatL(Tmp, [parts(Item, AmP)], TmpT);
                merge_parts(TmpT, TmpN);
                -tmp_job_items(Step, Ag, Tmp);
                +tmp_job_items(Step, Ag, TmpN);
            }
            ?tmp_job_items(Step, Ag, Tmp);
            if(Tmp \== []) {
                .send(Ag, tell, task(Step, receive));
            } else { if(
                    required_item_for_job(Ag, It, Item, AmR) & (
                        .member(Item, ReqAsm) |
                        ReqAsm == []
                    ) & (
                        // do not assemble if give would be faster
                        item_(Item, AmA) &
                        AmR > AmA |
                        AmR == 1
                    ) &
                    not assembling(Step, Ag, _) &
                    assemblable(Ag, Item, Al, Rem)
            ) {
                // make deliverer assemble an item
                .send(Ag, tell, task(Step, assemble(Item)));
                +assembling(Step, Ag, Item);
                .strB(Ag, AgB);
                for(.member(AgA, Al)) {
                    !remove_available(AgA);
                    .wait(5);
                    if(Self == AgA) {
                        .assist_assemble(AgB) | true;
                    } else {
                        .send(AgA, tell, task(Step, assist_assemble(AgB)));
                    }
                }
            }}
        }
    }
    while(step(Step) & available_agents(Step, AV) & .member(Self, AV)) {
        if(     deliver_plan(Job, L) &
                .member(ag(Ag, It), L) &
                not assembling(Step, Ag, _) &
                required_item_for_job(Ag, It, Item, Amount) &
                item(Item, Av) &
                Av >= Amount
        ) {
            // transfer item to deliverer
            ?tmp_job_items(Step, Ag, Tmp);
            if(Tmp == []) {
                .send(Ag, tell, task(Step, receive));
            }
            .concatL(Tmp, [parts(Item, Amount)], TmpT);
            merge_parts(TmpT, TmpN);
            -tmp_job_items(Step, Ag, Tmp);
            +tmp_job_items(Step, Ag, TmpN);
            .strB(Ag, AgB);
            .print("giving", Amount, "x", Item, "to", AgB);
            .give(AgB, Item, Amount) | true;
            !remove_available(Self);
            .wait(5);
            
        } else { if((
                        (.member(Item, ReqAsm) | ReqAsm == []) &
                        carried_items(C) &
                        .member(parts(Item, Req), C) &
                        item_(Item, Av) &
                        Av < Req
                    ) &
                    assemblable(Self, Item, Al, Rem)) {
            // assemble an item
            if(.member(Item, ReqAsm)) {
                .print("*assembling", Item);
            } else {
                .print("assembling", Item);
            }
            .strB(Self, SelfB);
            .assemble(Item) | true;
            !remove_available(Self);
            .wait(5);
            for(.member(AgA, Al)) {
                !remove_available(AgA);
                .wait(5);
                .send(AgA, tell, task(Step, assist_assemble(SelfB)));
            }
        } else {
            // skip the step
            .print("skipping");
            .recharge | true;
            !remove_available(Self);
            .wait(5);
        }}
    }
    // restock every 5 steps
    if(Y = Step / 5 & .floor(Y, Y)) {
        !restock;
    }
    .broadcast(tell, step_done(Step));
true.

