{ include("global_beliefs.asl") }
builder.
{ include("global_rules.asl") }

tool_volume_(T, Tool, Vol) :-
    .member(Tool, T) &
    item(Tool, Vol, _, _) &
true.

tool_volume(T, Vol) :-
    .findall(Vol, tool_volume_(T, Tool, Vol), L) &
    .sum(L, Vol) &
true.

{ include("global_plans.asl") }

// ********************************
// *      general behaviour       *
// ********************************

/**
 * initialize agent
 */
+step(Step) : not initializing & not initialized <-
    if(step(Step)) {
        +initializing;
        !!init;
    }
true.
+!init <-
    .broadcast(tell, builder_);
    while(not main_workshop(_)) {
        !wait_ms(1);
    }
    ?main_workshop(Workshop)[source(S)];
    
    while(not selectable_tools(_)) {
        !wait_ms(1);
    }
    ?selectable_tools(T)[source(S)];
    ?my_role(Role, _, LoadM, _, Tools);
    ?usable_tools(T, U, NU);
    +my_tools([]);
    +shared_tools([]);
    +other_tools(NU);
    for(.member(Tool, T) & .member(Tool, Tools)) {
        if(not(other_role(_, Role2, _, _, _, Tools2) & Role2 \== Role & .member(Tool, Tools2))) {
            my_tools(M) & .concatL(M, [Tool], MN) & tool_volume(MN, Vol);
            if(Vol <= LoadM) {
                -my_tools(_);
                +my_tools(MN);
            } else {
                ?other_tools(O);
                .concatL(O, [Tool], ON);
                -other_tools(_);
                +other_tools(ON);
            }
        } else {
            ?shared_tools(O);
            .concatL(O, [Tool], ON);
            -shared_tools(_);
            +shared_tools(ON);
        }
    }
    ?shared_tools(ST);
    for(.member(Tool, ST)) {
        my_tools(M) & .concatL(M, [Tool], MN) & tool_volume(MN, Vol);
        if(Vol <= LoadM) {
            -my_tools(_);
            +my_tools(MN);
        } else {
            ?other_tools(O);
            .concatL(O, [Tool], ON);
            -other_tools(_);
            +other_tools(ON);
        }
    }
    ?other_tools(O);
    .send(S, tell, other_tools(O));
    +initialized;
    -initializing;
    
    ?my_tools(M);
    .print("Wielded Tools:", M);
    for(.member(Tool, M)) {
        if(not item(Tool, _)) {
            nearest_shop(Shop, Tool, 1, Workshop, _);
            !goto(Shop);
            !buy(Tool, 1);
        }
    }
    !goto(Workshop);
    +at_workshop;
    tools_to_parts(M, P);
    .my_name(Self);
    .send(S, tell, providing(Self));
    ?step(Step);
    -step(Step);
    +step(Step);
true.

+step(Step) : initialized & at_workshop <-
    !!timeout_guard(Step);
    !collect_garbage;
    ?main_workshop(Workshop)[source(S)];
    .my_name(Self);
    .findall(parts(It, Am), item(It, Am), I);
    .broadcast(tell, item_available(Step, Self, I));
    while(step(Step) & not task(Step, _) & not step_done(Step)) {
        !wait_ms(4);
    }
    if(task(Step, Task)) {
        if(Task = assist_assemble(Ag)) {
            .assist_assemble(Ag) | true;
        } else { if(Task = give(Ag, Item, Amount)) {
            .print("Something went wrong here");
            //.give(Ag, Item, Amount) | true;
        } else {
            .print("UNKNOWN TASK", Task);
            .recharge | true;
        }}
    } else {
        .strB(S, Ag);
        .assist_assemble(Ag) | true;
    }
true.

