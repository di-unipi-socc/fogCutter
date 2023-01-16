% node(NodeId, SWCaps, HWCaps, IoTCaps).
node(smartphone8).
    hardwareCaps(smartphone8, 8). % the hardware capabilities of the node 
    softwareCaps(smartphone8, [android, gcc, make]). % the software capabilities of the node
    iotCaps(smartphone8, [temperature, humidity, light]). % the IoT capabilities of the node (e.g. sensors, actuators
    energyProfile(smartphone8, 0.8). % a number between 0 and 1 that indicates how much energy for the node is provided by renewable sources
    availability(smartphone8, 0.9). % the availability of the node
    profit(smartphone8, 10). % the profit that the infrastructureCut provider gets for providing a node
    location(smartphone8, eu). % the location of the node
    security(smartphone8, [antimalware, encryptedStorage]). % the security of the node
node(smartphone9).
    hardwareCaps(smartphone9, 8).
    softwareCaps(smartphone9, [android, gcc, make]).
    iotCaps(smartphone9, [temperature, humidity, light]).
    energyProfile(smartphone9, 0.99).
    availability(smartphone9, 0.95).
    profit(smartphone9, 4).
    location(smartphone9, us).
    security(smartphone9, [antimalware]).
node(accesspoint9).
    hardwareCaps(accesspoint9, 12).
    softwareCaps(accesspoint9, [android, gcc, make]).
    iotCaps(accesspoint9, [vrViewer, humidity, light]).
    energyProfile(accesspoint9, 0.8).
    availability(accesspoint9, 0.9999).
    profit(accesspoint9, 0).
    location(accesspoint9, eu).
    security(accesspoint9, [wpa2, firewall, antimalware]).

% link(NodeId1, NodeId2, Latency, Bandwidth).
link(accesspoint9, smartphone9, 10, 15).
link(smartphone9, accesspoint9, 10, 15).
link(accesspoint9, smartphone8, 10, 15).
link(smartphone8, accesspoint9, 10, 15).

% request(RequestId, SourceNodeId, TotHardware, MaxLatency, MinBandwidth, MaxNodes, Security, Locations).
% TODO: Add thresholds, e.g. [(sustainability, 0.5), (availability, 0.99)]
% TODO: consider suggesting a cut that is not the best one, but that is "good enough", return all results when it is not possible to find a best one.
request(req42, accesspoint9, (5, 200, 1, 2, [antimalware], [eu,us]), [sustainability,availability]).

findCuts(RequestId, Cuts) :-
    setof((Cut, Scores), fogcutter(RequestId, Cut, Scores), Cuts).

fogcutter(RequestId, Cut, Scores) :-
    request(RequestId, N, ReqsSpecs, Params),
    node(N), hardwareCaps(N, H),
    infrastructureCut(N, H, ReqsSpecs, [N], Nodes), sort(Nodes,Cut),
    cutScores(Cut, Params, Scores). 

infrastructureCut(_, H, (ReqHW, _, _, MaxNodes, _), Cut, Cut) :- 
    H >= ReqHW, length(Cut, L), L =< MaxNodes.
infrastructureCut(N, CurrHW, ReqsSpecs, Cut, NewCut) :-
    ReqsSpecs = (_, MaxLat, MinBW, _, Security, Locs), 
    goodNeighbour(N,M,H,MaxLat,MinBW,Security,Locs), \+ member(M,Cut),
    NewCurrHW is CurrHW + H,
    infrastructureCut(N, NewCurrHW, ReqsSpecs, [M|Cut], NewCut).

goodNeighbour(N, M, H, MaxLat, MinBW, SecReqs, Locs) :- 
    node(M), dif(N,M),
    location(M, Loc), member(Loc, Locs), hardwareCaps(M, H),
    security(M, SecCaps), subset(SecReqs, SecCaps),
    link(N,M,LNM,BNM), LNM < MaxLat, BNM > MinBW,
    link(M,N,LMN,BMN), LMN < MaxLat, BMN > MinBW.

%Params=[availability,cost]
cutScores(C,Params,Eval) :-
    initialize(Params,InitEval),
    cutScores(C,Params,InitEval,Eval).

initialize([],[]).
initialize([P|Ps],[E|Es]) :-
    initializeOne(P,E),
    initialize(Ps,Es).

initializeOne(availability,1).
initializeOne(profit,0).
initializeOne(sustainability,1).

cutScores([],_,E,E).
cutScores([N|Ns],Params,Eval,NewEval):-
    updateEval(N,Params,Eval,TempEval),
    cutScores(Ns,Params,TempEval,NewEval).

updateEval(_,[],E,E).
updateEval(N,[P|Ps],[E|Es],[NewE|NewEs]) :-
    updateOne(N,P,E,NewE),
    updateEval(N,Ps,Es,NewEs).

updateOne(N,availability,E,NewE) :-
    availability(N,X),
    NewE is E*X.
updateOne(N,sustainability,E,NewE) :-
    energyProfile(N,X),
    NewE is E*X.
updateOne(N,profit,E,NewE) :-
    profit(N,X),
    NewE is E+X.