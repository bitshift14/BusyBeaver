{ include("global_beliefs.asl") }
supplier.
{ include("global_rules.asl") }

/**
 * find fastest route to partially buy an item and reach workshop
 */
nearest_shop_partial(Shop, Item, Cap, Workshop, Steps, Amount) :-
    Cap > 0 &
    .findall(shop(Am, Steps, Name), route_shop_partial(Name, Item, Workshop, Steps, Am), L) &
    L \== [] & .max(L, shop(MaxS, _, _)) &
    .min([0.5*MaxS, Cap], Cutoff) &
    .findall(shop(Steps, Am, Name), nearest_shop_member(L, Cutoff, Am, Name, Steps), L2) &
    .min(L2, shop(Steps, Am, Shop)) &
    .min([Am, Cap], Amount) &
    true.
    route_shop_partial(Shop, Item, Workshop, Steps, Amount) :-
        shop(Shop, LatS, LonS, _, _) &
        stocked_max(Shop, Item, Amount) &
        workshop(Workshop, LatW, LonW) &
        steps(LatS, LonS, StepsS) &
        steps(LatS, LonS, LatW, LonW, StepsW) &
        Steps = StepsS + StepsW &
    true.
    nearest_shop_member(L, Cutoff, Amount, Shop, Steps) :-
        .member(shop(Amount, Steps, Shop), L) &
        Amount >= Cutoff &
true.

/**
 * find resource node
 */
best_resource_node(Node) :-
    .findall(node(Score, N), rate_resource_node(N, Score), L) &
    L \== [] &
    .max(L, node(Score, Node)) &
    Score > 0 &
    true.
    rate_resource_node(Node, Score) :-
        resource_node(Node, Lat, Lon, Res) &
        steps(Lat, Lon, Steps) &
        resource_rating(Res, R) &
        .count(gathering(Res)[source(S)], Ag) &
        Score = R / (Ag + 1) - Steps / 10 &
true.

/**
 * all items already promised to buy
 */
busy_items(ID, Items) :-
    item_cnp_info(ID, Step, Item, Amount, Shop, Steps, Prev) &
    I = [parts(Item, Amount)] & (
        Prev == 0 &
            Items = I |
        Prev \== 0 &
            busy_items(Prev, I2) &
            .concatL(I, I2, Items)) &
true.

{ include("global_plans.asl") }

/**
 * buy base items at current shop
 */
+!buy_items(CID) <-
    ?main_workshop(Workshop)[source(S)];
    +item_loop(CID);
    while(item_loop(ID)) {
        // iterate all contracts
        ?item_cnp_info(ID, _, Item, Amount, Shop, _, Prev);
        +buy_loop;
        while(buy_loop) {
            // available items at shop
            ?shop(Shop, _, _, _, SI);
            .member(item(Item, _, AmS), SI) |
                AmS = 0;
            if(AmS > 0) {
                // buy item if available
                !wait;
                .min([Amount, AmS], AmB);
                if(.buy(Item, AmB)) {
                    !wait;
                    if(lastActionResult(successful)) {
                        -buy_loop;
                    }
                }
            } else {
                -buy_loop;
            }
            if(not buy_loop) {
                .broadcast(untell, shop_reserved(ID, Shop, Item, Amount));
                -shop_reserved(ID, Shop, Item, Amount);
                if(AmS < Amount) {
                    // notify leader of item shortage
                    .send(S, untell, item_promise(ID, Item, Amount));
                    if(AmS > 0) {
                        .send(S, tell, item_promise(ID, Item, AmS));
                    }
                }
            }
        }
        -item_loop(ID);
        if(Prev > 0) {
            +item_loop(Prev);
        }
    }
    !goto(Workshop);
    +item_loop(CID);
    while(item_loop(ID)) {
        ?item_cnp_info(ID, _, Item, Amount, Shop, _, Prev);
       .send(S, untell, item_promise(ID, _, _));
        -item_loop(ID);
        if(Prev > 0) {
            +item_loop(Prev);
        }
    }
    !provide_items;
    while(busy(_)) {
        -busy(_);
    }
    if(.ready & step(StepN) & step_done(StepN)) {
        .print("unexpected ready");
        .recharge | true;
    }
true.


// ********************************
// *      general behaviour       *
// ********************************

/**
 * initialize
 */
+step(Step) : not initialized <-
    +initialized;
    !!start;
true.
+!start <-
    .broadcast(tell, supplier_);
    if(item(_, _)) {
        !provide_items;
    }
true.

+step(Step) <-
    !!timeout_guard(Step);
    !collect_garbage;
true.

/**
 * find and/or execute idle action, such as charge or gather
 */
+step_done(Step) : step(Step) & not busy(_) & .ready <-
    charge(Charge) & my_role(_, _, _, Battery, _) & Charge_P = Charge / Battery;
    if(idle_action(charge(_, _)) & Charge_P == 1) {
        -idle_action(_);
    }
    if( idle_action(gather(Node)) &
            resource_node(Node, _, _, Res) &
            item(Res, Vol, _, _) &
            item_(Res, Am) &
            free_load(F) & (
                F < Vol |
                Am >= 8
    )) {
        .print("gathered", Am, "x", Res);
        -idle_action(_);
        +busy(tmp);
        !provide_items;
        -busy(tmp);
    }
    if(idle_action(gather(Node)) & .turn(Step)) {
        -idle_action(gather(Node));
    }
    if(not idle_action(_)) {
        if(load(Load) & my_role(_, _, LoadM, _, _) & (Load / LoadM >= 0.4 | Load >= 400)) {
            !provide_items;
        } else { if(charge(Charge) & my_role(_, _, _, Battery, _) & Charge / Battery <= 0.3) {
            lat(LatS) & lon(LonS);
            .findall(cs(Steps, LatC, LonC), steps_to_cs(LatS, LonS, LatC, LonC, Steps), C);
            .min(C, cs(_, LatC, LonC));
            +idle_action(charge(LatC, LonC));
        } else { if(best_resource_node(Node)) {
            +idle_action(gather(Node));
        }}}
    }
    if(idle_action(charge(LatC, LonC))) {
        if(in_range(LatC, LonC)) {
            .charge;
        } else {
            !go_towards(LatC, LonC);
        }
    } else { if(idle_action(gather(Node))) {
        if(in_range(Node)) {
            if(X = Step / 10 & .floor(X, X)) {
                !redist;
            } else {
                .gather | true;
            }
        } else {
            !go_towards(Node);
        }
    } else { if(.ready) {
        .recharge | true;
    }}}
    if(.ready) {
        .print("unexpected ready");
        .recharge | true;
    }
true.

+idle_action(gather(Node)) : resource_node(Node, _, _, Res) <-
    .broadcast(tell, gathering(Res));
true.

-idle_action(gather(Node)) : resource_node(Node, _, _, Res) <-
    .broadcast(untell, gathering(Res));
true.


// ********************************
// *    contract net protocol     *
// ********************************

/**
 * handle call for proposals
 */
+!item_cfp(ID, Item, Amount)[source(S)] :
        not busy(_) &
        main_workshop(Workshop) &
        carryable(Item, Amount) &
        nearest_shop(Shop, Item, Amount, Workshop, Steps) &
        step(Step) <-
    // idle, full offer
    +item_cnp_info(ID, Step, Item, Amount, Shop, Steps, 0);
    .send(S, tell, item_propose(ID, Steps));
true.
+!item_cfp(ID, Item, Amount)[source(S)] :
        not busy(_) &
        main_workshop(Workshop) &
        carryable_max(Item, Cap) &
        nearest_shop_partial(Shop, Item, Cap, Workshop, Steps, Part) &
        Part > 3 &
        step(Step) <-
    // idle, partial offer
    +item_cnp_info(ID, Step, Item, Part, Shop, Steps, 0);
    .send(S, tell, item_partial(ID, Steps, Part));
true.
+!item_cfp(ID, Item, Amount)[source(S)] :
        busy(buy(Prev)) &
        item_cnp_info(Prev, _, _, _, Shop, Steps, _) &
        busy_items(Prev, Items) &
        .concatL(Items, [parts(Item, Amount)], LNew) &
        total_volume(LNew, Vol) &
        free_load(LoadF) &
        LoadF >= Vol &
        nearest_shop(Shop, Item, Amount, Workshop, Steps) &
        step(Step) <-
    // buying, full offer
    +item_cnp_info(ID, Step, Item, Amount, Shop, Steps, Prev);
    .send(S, tell, item_propose(ID, 1));
true.
+!item_cfp(ID, Item, Amount)[source(S)] :
        busy(buy(Prev)) &
        item_cnp_info(Prev, _, _, _, Shop, Steps, _) &
        busy_items(Prev, Items) &
        .concatL(Items, [parts(Item, Amount)], LNew) &
        totel_volume(LNew, Vol) &
        free_load(LoadF) &
        Max = LoadF - Vol &
        carryable_max(Item, Cap, Max) &
        nearest_shop_partial(Shop, Item, Cap, Workshop, Steps, Part) &
        step(Step) <-
    // buying, partial offer
    +item_cnp_info(ID, Step, Item, Part, Shop, Steps, Prev);
    .send(S, tell, item_partial(ID, 1, Part));
true.
+!item_cfp(ID, Item, Amount)[source(S)] <-
    // refusal
    .send(S, tell, item_refuse(ID));
true.

/**
 * handle cnp winner declaration
 */
+item_cnp_winner(ID, Ag) : not (.my_name(Self) & .eq(Ag, Self)) <-
    // other agent has won
    -item_cnp_winner(ID, Ag);
    -item_cnp_info(ID, _, _, _, _, _, _);
true.

+item_cnp_winner(ID, Ag)[source(S)] : not busy(_) <-
    // idle
    ?item_cnp_info(ID, Step, Item, Amount, Shop, Steps, _);
    .broadcast(tell, shop_reserved(ID, Shop, Item, Amount));
    +shop_reserved(ID, Shop, Item, Amount);
    .send(S, tell, item_promise(ID, Item, Amount));
    +busy(buy(ID));
    -idle_action(_);
    ?main_workshop(Workshop)[source(S)];
    .print("BUY:", Amount, "x", Item);
    !goto(Shop);
    if(not buy_stop(ID)) {
        +busy(buy2);
        -busy(buy(ID));
        !wait_ms(500);
        if(not buy_stop(ID)) {
            !buy_items(ID);
        }
    }
true.

+item_cnp_winner(ID, Ag)[source(S)] : busy(buy(Prev)) <-
    // already buying another item
    ?item_cnp_info(ID, Step, Item, Amount, Shop, Steps, Prev);
    +buy_stop(Prev);
    +busy(buy(ID));
    -busy(buy(Prev));
    .broadcast(tell, shop_reserved(ID, Shop, Item, Amount));
    +shop_reserved(ID, Shop, Item, Amount);
    .send(S, tell, item_promise(ID, Item, Amount));
    .print("*BUY:", Amount, "x", Item);
    !goto(Shop);
    if(not buy_stop(ID)) {
        +busy(buy2);
        -busy(buy(ID));
        !wait_ms(500);
        if(not buy_stop(ID)) {
            !buy_items(ID);
        }
    }
true.

+item_cnp_winner(ID, Ag) <-
    .print("item_cnp_winner fail");
true.


