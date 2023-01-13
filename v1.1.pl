%:- consult('infra.pl').

% node(NodeId, SWCaps, HWCaps, IoTCaps).
node(smartphone8,[android, gcc, make], 8, []).
    energyProfile(smartphone8, 0.8). % a number between 0 and 1 that indicates how much energy for the node is provided by renewable sources
    availability(smartphone8, 0.9). % the availability of the node
    profit(smartphone8, 10). % the profit that the infrastructure provider gets for providing a node
    location(smartphone8, eu). % the location of the node
    security(smartphone8, [antimalware, encryptedStorage]). % the security of the node
node(smartphone9,[android, gcc, make], 8, [vrViewer]).
    energyProfile(smartphone9, 0.99).
    availability(smartphone9, 0.95).
    profit(smartphone9, 4).
    location(smartphone9, us).
    security(smartphone9, [antimalware]).
node(accesspoint9,[android, gcc, make], 12, [vrViewer]).
    energyProfile(accesspoint9, 0.8).
    availability(accesspoint9, 0.9999).
    profit(accesspoint9, 0).
    location(accesspoint9, eu).
    security(accesspoint9, [wpa2, firewall, antimalware]).

link(accesspoint9, smartphone9, 10, 15).
link(smartphone9, accesspoint9, 10, 15).
link(accesspoint9, smartphone8, 10, 15).
link(smartphone8, accesspoint9, 10, 15).

% request(RequestId, SourceNodeId, TotHardware, MaxLatency, MinBandwidth, MaxNodes, Security, Locations).
request(req42, accesspoint9, (5, 200, 1, 2, [antimalware], [eu,us])).

findCuts(RequestId, Continua) :-
    setof((Nodes,HW), fogcutter(RequestId, Nodes, HW), Continua).

fogcutter(RequestId, Nodes, Params) :-
    request(RequestId, N, ReqsSpecs),
    node(N,_,H,_),
    infrastructure(N, H, ReqsSpecs, [N], Nodes, Params).

infrastructure(_, H, (ReqHW, _, _, _, _), L, NewL, (H,R,A,P)) :- 
    H >= ReqHW, sort(L,NewL),
    renewableCut(NewL, R), 
    availableCut(NewL, A), 
    cutProfit(NewL, P).
infrastructure(N, CurrHW,(HW, MaxLat, MinBW, MaxNodes, Security, Locs), Nodes, NewNodes, Params) :-
    goodNeighbour(N,M,H,MaxLat,MinBW,Security,Locs), \+ member(M,Nodes),
    NewCurrHW is CurrHW + H,
    MaxNodes > 0, NewMax is MaxNodes - 1, 
    infrastructure(N, NewCurrHW, (HW, MaxLat, MinBW, NewMax, Security, Locs), [M|Nodes], NewNodes, Params).

goodNeighbour(N, M, H, MaxLat, MinBW, SecReqs, Locs) :- 
    node(M,_,H,_), dif(N,M),
    location(M, Loc), member(Loc, Locs),
    security(M, SecCaps), subset(SecReqs, SecCaps),
    link(N,M,LNM,BNM), LNM < MaxLat, BNM > MinBW,
    link(M,N,LMN,BMN), LMN < MaxLat, BMN > MinBW.

cutProfit([], 0).
cutProfit([N|Ns], Profit) :-
    cutProfit(Ns, Profit1),
    profit(N, P),
    Profit is Profit1 + P.

availableCut([], 1).
availableCut([N|Ns], A) :-
    availableCut(Ns, A1),
    availability(N, A2),
    A is A1 * A2.

renewableCu([], 1).
renewableCu([N|Ns], GScore) :-
    renewableCut(Ns, GScore1),
    energyProfile(N, E),
    GScore is GScore1 * E.

