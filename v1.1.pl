%:- consult('infra.pl').

% node(NodeId, SWCaps, HWCaps, IoTCaps).
node(smartphone8,[android, gcc, make], 8, []).
    energyProfile(smartphone8, 0.8). % a number between 0 and 1 that indicates how much energy for the node is provided by renewable sources
    availability(smartphone8, 0.9). % the availability of the node
    profit(smartphone8, 10). % the profit that the infrastructure provider gets for providing a node
    location(smartphone8, eu). % the location of the node
node(smartphone9,[android, gcc, make], 8, [vrViewer]).
    energyProfile(smartphone9, 0.99).
    availability(smartphone9, 0.95).
    profit(smartphone9, 4).
    location(smartphone9, us).
node(accesspoint9,[android, gcc, make], 12, [vrViewer]).
    energyProfile(accesspoint9, 0.8).
    availability(accesspoint9, 0.9999).
    profit(accesspoint9, 0).
    location(smartphone8, eu).

link(accesspoint9, smartphone9, 10, 15).
link(smartphone9, accesspoint9, 10, 15).
link(accesspoint9, smartphone8, 10, 15).
link(smartphone8, accesspoint9, 10, 15).

% request(RequestId, SourceNodeId, TotHardware, MaxLatency, MinBandwidth, MaxNodes, Locations).
request(req42, accesspoint9, 5, 25, 10, 1, [eu,us]).

findCuts(RequestId, Continua) :-
    setof((Nodes,HW), fogcutter(RequestId, Nodes, HW), Continua).

fogcutter(RequestId, Nodes, Params) :-
    request(RequestId, N, Hardware, MaxLatency, MinBandwidth, MaxNodes, Locs),
    node(N,_,H,_),
    infrastructure(N, H, Hardware, MaxLatency, MinBandwidth, MaxNodes, Locs, [N], Nodes, Params).

infrastructure(_, H, MissingHW, _, _, _, _, L, NewL, (H,R,A,P)) :- 
    MissingHW =< 0, sort(L,NewL),
    renewableScore(NewL, R),
    availableCut(NewL, A),
    cutProfit(NewL, P).
infrastructure(N, CurrHW, MissingHW, MaxLat, MinBW, MaxNodes, Locs, Nodes, NewNodes, Params) :-
    goodNeighbour(N,M,H,MaxLat,MinBW,Locs), \+ member(M,Nodes),
    NewMissingHW is MissingHW - H,
    NewCurrHW is CurrHW + H,
    MaxNodes > 0, NewMax is MaxNodes - 1, 
    infrastructure(N, NewCurrHW, NewMissingHW, MaxLat, MinBW, NewMax, Locs, [M|Nodes], NewNodes, Params).

goodNeighbour(N, M, H, MaxLat, MinBW, Locs) :- 
    node(M,_,H,_), dif(N,M),
    location(M, Loc), member(Loc, Locs),
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

renewableScore([], 1).
renewableScore([N|Ns], GScore) :-
    renewableScore(Ns, GScore1),
    energyProfile(N, E),
    GScore is GScore1 * E.

