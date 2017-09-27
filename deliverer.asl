{ include("global_beliefs.asl") }
deliverer.
{ include("global_rules.asl") }
{ include("global_plans.asl") }

// ********************************
// *      general behaviour       *
// ********************************

+step(Step) : not initializing & not initialized <-
    +initializing;
    !!init;
true.

+step(Step) <-
    !!timeout_guard(Step);
    !collect_garbage;
true.

+step_done(Step) : initialized & step(Step) & not busy(_) & .ready <-
    .recharge;
true.

+!init <-
    .broadcast(tell, deliverer_);
    while(not main_workshop(_)) {
        !wait_ms(1);
    }
    ?main_workshop(Workshop)[source(S)];
    !goto(Workshop);
    .my_name(Self);
    if(item(_, _)) {
        !provide_items;
    }
    .broadcast(tell, deliverer_available(Self));
    +initialized;
    -initializing;
true.

+!bid_for_job(Job, Bid)[source(S)] : auction(Job, _, Reward, _, _, _, Lowest, Time, _) <-
    +busy(bidding);
    .my_name(Self);
    .broadcast(untell, deliverer_available(Self));
    .print("BID:", Bid, "on", Job);
    .bid_for_job(Job, Bid);
    !wait;
    .broadcast(tell, deliverer_available(Self));
    -busy(bidding);
true.


// ********************************
// *    contract net protocol     *
// ********************************

+!deliver_cfp(ID, Job)[source(S)] :
        initialized &
        not busy(_) &
        any_job(Job, Storage, _, _, _, R) &
        free_load(Free) &
        steps(Storage, Steps) &
        main_workshop(Workshop) &
        in_range(Workshop) <-
    // available
    +deliver_cnp_info(ID, Job);
    .send(S, tell, deliver_propose(ID, Steps, Free));
true.
+!deliver_cfp(ID, Job)[source(S)] <-
    .send(S, tell, deliver_refuse(ID));
true.

+deliver_cnp_winner(ID, W) : not (.my_name(Self) & .member(Ag, W) & .eq(Ag, Self)) <-
    // other agent has won
    -deliver_cnp_winner(ID, W);
    -deliver_cnp_info(ID, _);
true.

+deliver_cnp_winner(ID, W) : initialized  & not busy(_) & main_workshop(Workshop)[source(S)] & in_range(Workshop) <-
    ?deliver_cnp_info(ID, Job);
    +busy(deliver(Job));
    .my_name(Self);
    .broadcast(untell, deliverer_available(Self));
    ?any_job(Job, Storage, Reward, _, _, R);
    lat(Lat) & lon(Lon) & storage(Storage, LatS, LonS, _, _, _);
    .dist_road(Lat, Lon, LatS, LonS, Dist) & my_role(_, Speed, _, _, _) & cell_size(CS) & .ceil(Dist / (Speed * CS), Steps);
    .print("DELIVER LOAD:", Job, Reward, R);
    .my_name(Self);
    +deliver_loop;
    while(deliver_loop) {
        // report inventory to leader
        .findall(parts(It, Am), item(It, Am), I);
        ?step(Step);
        .send(S, tell, item_available(Step, Self, I));
        while(step(Step) & not (task(Step, _) | step_done(Step))) {
            !wait_ms(4);
        }
        if(task(Step, Task)) {
            // follow order
            if(Task = assemble(It)) {
                .print("assembling", It);
                .assemble(It) | true;
            } else { if(Task = receive) {
                .receive | true;
            } else { if(Task = assist_assemble(Ag)) {
                .assist_assemble(Ag) | true;
            } else { if(Task = deliver) {
                -deliver_loop;
            } else { if(Task = fail) {
                .print("DELIVER FAIL", Job);
                -deliver_loop;
                +fail(Job);
            } else {
                .print("UNKNOWN TASK", Task);
                .recharge | true;
            }}}}}
        } else { if(step(Step) & .ready) {
            .recharge | true;
        }}
        !wait;
        if(deliver_loop) {
            !wait_until(Step + 1);
        }
    }
    if(not fail(Job)) {
        // deliver items
        .findall(parts(Item, Amount), item(Item, Amount), Inventory);
        .print("DELIVER GOTO:", Job, Reward, Inventory, " / Steps:", Steps);
        !deliver_job(Job);
        while(  storage(Storage, _, _, _, _, Stored) &
                .member(item(Item, _, Delivered), Stored) &
                Delivered > 0 &
                carryable_max(Item, Cap) &
                Cap > 0) {
            .max([Cap, Delivered], Amount);
            .print("RETRIEVE:", Amount, "x", Item);
            .retrieve_delivered(Item, Amount) | true;
            !wait;
        }
    }
    !goto(Workshop);
    !provide_items;
    .broadcast(tell, deliverer_available(Self));
    -busy(_);
true.

+deliver_cnp_winner(ID, W) <-
    .print("deliver_cnp_winner fail");
    while(not initialized) {
        !wait_ms(1);
    }
    !wait_busy;
    +busy(tmp);
    while(not main_workshop(_)) {
        !wait_ms(1);
    }
    ?main_workshop(Workshop);
    !goto(Workshop);
    -busy(tmp);
    -deliver_cnp_winner(ID, W);
    +deliver_cnp_winner(ID, W);
true.


