// ********************************
// *     belief sharing plans     *
// ********************************

/**
 * notify other agents about own role
 */
+role(Name, Speed, Load, Battery, Tools)[source(percept)] <-
    .broadcast(tell, role_(Name, Speed, Load, Battery, Tools));
    +role(Name, Speed, Load, Battery, Tools);
true.

/**
 * notify other agents about resource nodes
 */
+resourceNode(Name, Lat, Lon, Res)[source(percept)] :
        not resourceNode_(Name, Lat, Lon, Res) <-
    .broadcast(tell, resourceNode_(Name, Lat, Lon, Res));
    +resourceNode_(Name, Lat, Lon, Res);
    .print("found", Name, "-", Res);
true.


// ********************************
// *          wait plans          *
// ********************************

+!wait_ms(Time) <-
    .wait(Time);
true.

/**
 * wait until agent is execute a new action
 */
+!wait <-
    if(not .ready) {
        ?step(Step);
        while(step(Step) | not step(StepN)) {
            !wait_ms(10);
        }
    }
true.

/**
 * wait until specified step
 */
+!wait_until(Step) <-
    while(not (step(StepC) & Step <= StepC)) {
        !wait_ms(10);
    }
true.

/**
 * wait until the busy-belief is removed
 */
+!wait_busy <-
    while(busy(_)) {
        !wait_ms(10);
    }
true.


// ********************************
// *       action wrappers        *
// ********************************

/**
 * move agent to desied location (coordinates or facility name)
 *  this may take multiple steps
 */
+!goto(Lat, Lon) : in_range(Lat, Lon) <- true.
+!goto(Lat, Lon) : not .ready <-
    !wait;
    !goto(Lat, Lon);
true.
+!goto(Lat, Lon) : in_battery_range(Lat, Lon) & lat(LatS) & lon(LonS) <-
    .goto(Lat, Lon) | true;
    !goto(Lat, Lon);
true.
+!goto(Lat, Lon) : best_cs(Lat, Lon, LatCS, LonCS)  <-
    !goto(LatCS, LonCS);
    !charge_full;
    !goto(Lat, Lon);
true.
+!goto(Lat, Lon) <-
    //.print("recharging...");
    .recharge | true;
    !goto(Lat, Lon);
true.
+!goto(Fac) : facility(Fac, Lat, Lon) <-
    !goto(Lat, Lon);
true.


/**
 * nonblocking goto (one step)
 */
+!go_towards(Lat, Lon) : not .ready | in_range(Lat, Lon) <- true.
+!go_towards(Lat, Lon) : chargingStation(_, LatC, LonC, _) & in_range(LatC, LonC) & charge(Charge) & my_role(_, _, _, CMax, _) & Charge < CMax <-
    .charge;
true.
+!go_towards(Lat, Lon) : in_battery_range(Lat, Lon) <-
    .goto(Lat, Lon) | true;
true.
+!go_towards(Lat, Lon) : best_cs(Lat, Lon, LatCS, LonCS) <-
    !go_towards(LatCS, LonCS);
true.
+!go_towards(Lat, Lon) <-
    //.print("recharging...");
    .recharge | true;
true.
+!go_towards(Fac) : facility(Fac, Lat, Lon) <-
    !go_towards(Lat, Lon);
true.

/**
 * completely charge the agent. requires being located at a charging station
 */
+!charge_full : charge(Charge) & my_role(_, _, _, CMax, _) & Charge = CMax <- true.
+!charge_full <-
    !wait;
    .charge | true;
    !wait;
    !charge_full;
true.

/**
 * try to buy an item until success
 */
+!buy(Item, Amount) <-
    !wait;
    .buy(Item, Amount);
    !wait;
    if(not lastActionResult(successful)) {
        !buy(Item, Amount);
    };
true.

/**
 * try to deliver items for a job until successful
 * items have to be present in inventory
 */
+!deliver_job(Job) : any_job(Job, Storage, _, _, End, _) <-
    !goto(Storage);
    if(not .deliver_job(Job)) {
        !deliver_job(Job);
    } else {
        !wait;
        if(not (
                lastActionResult(failed_job_status) &
                    .print("FAILED JOB STATUS")
            ) & not (
                lastActionResult(successful) &
                    money(Money) &
                    .print("delivery successful, money:", Money)
            ) & not (
                lastActionResult(successful_partial) &
                    .print("delivery partially successful")
        )) {
            ?step(Step);
            if(Step < End) {
                !deliver_job(Job);
            } else {
                .print(Job, "already ended");
                +fail(Job);
            }
        }
    }
true.


// ********************************
// *  belief and goal management  *
// ********************************

+!item_cfp(ID, Item, Amount) : not supplier <- true.
+item_cnp_winner(ID, W) : not supplier <- -item_cnp_winner(ID, W).
+!deliver_cfp(ID, Job) : not deliverer <- true.
+deliver_cnp_winner(ID, W) : not deliverer <- -deliver_cnp_winner(ID, W).

/**
 * remove some unnecessary beliefs
 */
+!collect_garbage : step(StepN) <-
    Step = StepN - 1;
    while(step_start(Step2) & Step2 < Step) {
        -step_start(Step2);
    }
    while(step_done(Step2) & Step2 < Step) {
        -step_done(Step2);
    }
    while(available_agents(Step2, _) & Step2 < Step) {
        -available_agents(Step2, _);
    }
    while(item_available(Step2, _, _) & Step2 < Step) {
        -item_available(Step2, _, _);
    }
    while(task(Step2, _) & Step2 < Step) {
        -task(Step2, _);
    }
    while(redist(Step2, _, _, _, _) & Step2 < Step) {
        -redist(Step2, _, _, _, _);
    }
    while(ready(Step2) & Step2 < Step) {
        -ready(Step2);
    }
    while(job_processed(Job) & not any_job(Job, _, _, _, _, _)) {
        -job_processed(Job);
    }
    while(tmp_job_item(Step2, _, _) & Step2 < Step) {
        -tmp_job_item(Step2, _, _);
    }
    while(assembling(Step2, _, _) & Step2 < Step) {
        -assembling(Step2, _, _);
    }
    while(tmp_job_items(Step2, _, _) & Step2 < Step) {
        -tmp_job_items(Step2, _, _);
    }
true.


// ********************************
// *      base item handling      *
// ********************************

/**
 * offer items and work to leader
 */
+!provide_items <-
    while(not main_workshop(_)) {
        !wait_ms(1);
    }
    ?main_workshop(Workshop)[source(S)];
    +busy(provide);
    .my_name(Self);
    while(.findall(parts(It, Am), item(It, Am), I) & I \== []) {
        if(not in_range(Workshop)) {
            !goto(Workshop);
        }
        ?step(Step);
        if(not providing) {
            .send(S, tell, providing(Self));
            +providing;
        }
        // report inventory to leader
        .broadcast(tell, item_available(Step, Self, I));
        while(step(Step) & not (task(Step, _) | step_done(Step))) {
            !wait_ms(4);
        }
        if(task(Step, Task)) {
            // follow order
            if(Task = assist_assemble(Ag)) {
                .assist_assemble(Ag) | true;
            } else { if(Task = give(Ag, Item, Amount)) {
                .print("giving", Amount, "x", Item, "to", Ag);
                .give(Ag, Item, Amount) | true;
            } else {
                .print("UNKNOWN TASK", Task);
                .recharge | true;
            }}
        } else { if(step(Step) & step_done(Step)) {
            // try to free some agents for other contracts
            !redist;
        }}
        if(step(Step) & .ready) {
            .recharge | true;
        }
        !wait;
        !wait_until(Step + 1);
    }
    .send(S, untell, providing(Self));
    -providing;
    -busy(provide);
true.

/**
 * redistribute items among several agents in the same location,
 *  aiming to free the inventory of some of them
 */
+!redist <-
    !wait_ms(20);
    lat(Lat) & lon(Lon) & Pos = pos(Lat, Lon);// & facility(Pos, Lat, Lon);
    // select largest item stack
    .findall(item(Load, Item), heavy_item(Item, Load), IL);
    if(IL \== []) {
        .max(IL, item(LoadS, Item));
    } else {
        LoadS = 10000 & Item = 0;
    }
    if(deliverer) {
        // do not redistribute onto deliverers
        FreeS = 0;
    } else {
        ?free_load(FreeS);
    }
    .my_name(Self);
    ?step(Step);
    .broadcast(tell, redist(Step, Pos, FreeS, LoadS, Self));
    .send(Self, tell, redist(Step, Pos, FreeS, LoadS, Self));
    !wait_ms(20);
    // find agent to redistribute onto
    while(.findall(redist(Free, R), redist(Step, Pos, Free, _, R), P) & P \== []
            & .max(P, redist(Free, R)) & Free > 0) {
        +maxload(Free);
        -redist(Step, Pos, Free, _, R);
        // find agent to redistribute from
        while(maxload(M) & .findall(redist(Load, G), redist(Step, Pos, _, Load, G), A) & A \== []
                & .min(A, redist(Load, G)) & M > Load) {
            -maxload(M);
            +maxload(M - Load);
            -redist(Step, Pos, _, Load, G);
            if(G == Self) {
                .strB(R, RB);
                ?item(Item, Amount);
                .give(RB, Item, Amount) | true;
            }
        }
        ?maxload(M);
        -maxload(M);
        if(R == Self) {
            if(M == Free) {
                .recharge | true;
            } else {
                .receive | true;
            }
        }
    }
    if(.ready) {
        FreeS == 0;
        .recharge | true;
    }
true.


// ********************************
// *    logging and debugging     *
// ********************************

+!timeout_guard(Step) <-
    if(not step(Step)) {
        .print("TIMEOUT LAG");
    }
    -log;
    if(Step > 5) {
        !wait_ms(3500);
        if(.ready & step(Step)) {
            +timeout(Step);
            //.print("TIMEOUT SKIP");
            .skip;
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

+lastActionResult(Res) :
        not Res = successful &
        not Res = successful_partial &
        not Res = failed_counterpart &
        not Res = failed <-
    ?lastAction(Action);
    .print(Action, Res);
true.

+!print_inventory <-
    .findall(item(Item, Amount), item(Item, Amount), L);
    .print("Inventory:", L);
true.



