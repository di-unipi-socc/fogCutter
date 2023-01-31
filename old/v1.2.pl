node(smartphone8).
    vmType(smartphone8, 8). % the hardware capabilities of the node 
    softwareCaps(smartphone8, [android, gcc, make]). % the software capabilities of the node
    iotCaps(smartphone8, [temperature, humidity, light]). % the IoT capabilities of the node (e.g. sensors, actuators
    energyProfile(smartphone8, 0.8). % a number between 0 and 1 that indicates how much energy for the node is provided by renewable sources
    availability(smartphone8, 0.9). % the availability of the node
    profit(smartphone8, 10). % the profit that the infrastructureCut provider gets for providing a node
    location(smartphone8, eu). % the location of the node
    security(smartphone8, [antimalware, encryptedStorage]). % the security of the node
node(smartphone9).
    vmType(smartphone9, 8). 
    vmType(smartphone9, 1). 
    vmType(smartphone9, 2). 
    softwareCaps(smartphone9, [android, gcc, make]).
    iotCaps(smartphone9, [temperature, humidity, light]).
    energyProfile(smartphone9, 0.99).
    availability(smartphone9, 0.95).
    profit(smartphone9, 4).
    location(smartphone9, us).
    security(smartphone9, [antimalware]).
node(accesspoint9).
    vmType(accesspoint9, 12).
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
% TODO: consider suggesting a cut that is not the best one, but that is "good enough", return all results when it is not possible to find a best one.
request(req42, accesspoint9, (5, 200, 1, 2, [antimalware], [eu,us]), [(sustainability,0.6),(availability, 0.8)]).

% Returns the cut with the highest profit for the provider.
bestCut(RequestId, BestCut) :- 
    fogCutter(RequestId, Cuts),
    cutsWithProfit(Cuts, PCs), % TODO: change profit into rank so to linearly combine all metrics into a single value :-)
    sort(PCs, Tmp), reverse(Tmp,  [(_,BestCut,_)|_]).

cutsWithProfit([], []).
cutsWithProfit([(C,Scores)|Cs], [(P,C,Scores)|PCs]) :-
    member((profit,P), Scores),
    cutsWithProfit(Cs, PCs).

fogCutter(RequestId, Cuts) :-
    request(RequestId, _, _, Targets),
    setof((Cut, Scores), (fogCutter(RequestId, Targets, Cut, Scores), suitableCut(Targets,Scores)), Cuts).

fogCutter(RequestId, Targets, Cut, Scores) :-
    request(RequestId, N, ReqsSpecs, Params),
    node(N), vmType(N, H),
    infrastructureCut(N, H, ReqsSpecs, [N], Nodes), sort(Nodes,Cut),
    append(Params, [(profit,0)], NewParams), % params from the infrastructure provider
    cutScores(Cut, NewParams, Scores).

infrastructureCut(_, H, (ReqHW, _, _, MaxNodes, _), Cut, Cut) :- 
    H >= ReqHW, length(Cut, L), L =< MaxNodes.
infrastructureCut(N, CurrHW, ReqsSpecs, Cut, NewCut) :-
    ReqsSpecs = (_, MaxLat, MinBW, _, Security, Locs), 
    goodNeighbour(N,M,H,MaxLat,MinBW,Security,Locs), \+ member(M,Cut),
    NewCurrHW is CurrHW + H,
    infrastructureCut(N, NewCurrHW, ReqsSpecs, [M|Cut], NewCut).

goodNeighbour(N, M, H, MaxLat, MinBW, SecReqs, Locs) :- 
    node(M), dif(N,M),
    location(M, Loc), member(Loc, Locs), vmType(M, H),
    security(M, SecCaps), subset(SecReqs, SecCaps),
    link(N,M,LNM,BNM), LNM < MaxLat, BNM > MinBW,
    link(M,N,LMN,BMN), LMN < MaxLat, BMN > MinBW.

%Params=[availability,cost]
cutScores(C,Params,Eval) :-
    initialize(Params,InitEval),
    cutScores(C,Params,InitEval,Eval).

initialize([P|Ps],[E|Es]) :-
    initializeOne(P,E),
    initialize(Ps,Es).
initialize([],[]).

initializeOne((availability,_),(availability,1)).
initializeOne((sustainability,_),(sustainability,1)).
initializeOne((profit,_),(profit,0)).

cutScores([N|Ns],Params,Eval,NewEval):-
    updateEval(N,Params,Eval,TempEval),
    cutScores(Ns,Params,TempEval,NewEval).
cutScores([],_,E,E).

updateEval(N,[P|Ps],[E|Es],[NewE|NewEs]) :-
    updateOne(N,P,E,NewE),
    updateEval(N,Ps,Es,NewEs).
updateEval(_,[],E,E).

updateOne(N,(availability,_),(availability,E),(availability,NewE)) :-
    availability(N,X), NewE is E*X.
updateOne(N,(sustainability,_),(sustainability,E),(sustainability,NewE)) :-
    energyProfile(N,X), NewE is E*X.
updateOne(N,(profit,_),(profit,E),(profit,NewE)) :-
    profit(N,X), NewE is E+X.

suitableCut([(P,T)|Ts], Scores) :-
   member((P, S), Scores), S >= T, suitableCut(Ts, Scores).
suitableCut([], _).