:- consult('infra.pl').

% request(RequestId, SourceNodeId, TotHardware, MaxLatency, MinBandwidth, MaxNodes).
% DONE: 
%   - Energy sources (PLANET)
%   - Profit for the provider (PROFIT)
%   - Security (PEOPLE)
%   - Availability (PEOPLE)
%   - Geographical Location (PEOPLE)
% TODO: 
%   - Energy consumption (PLANET) (makes sense?)
%   - Variations with probabilities (PEOPLE) check whether/how fogcutter works in Problog
% request(n45, small, .95).
% How to compose different Cloud-IoT "cuts"? (Subset of the union)
% How to pick the "best" cut?
% How to compose a request based on application requirements?

request(req42, accesspoint9, 10, 25, 10, 3).

findCuts(RequestId, Continua) :-
    setof((Nodes,HW), fogcutter(RequestId, Nodes, HW), Continua).

fogcutter(RequestId, Nodes, TotHW) :-
    request(RequestId, NodeId, Hardware, MaxLatency, MinBandwidth, MaxNodes),
    infrastructure(NodeId, 0, Hardware, MaxLatency, MinBandwidth, MaxNodes, [], Nodes, TotHW).

infrastructure(_, H, MissingHW, _, _, _, P, NewP, H) :- MissingHW =< 0, sort(P,NewP).
infrastructure(N, CurrHW, MissingHW, MaxLat, MinBW, MaxNodes, Nodes, NewNodes, TotHW) :-
    goodNeighbour(N,M,H,MaxLat,MinBW), \+ member(M,Nodes),
    NewMissingHW is MissingHW - H,
    NewCurrHW is CurrHW + H,
    MaxNodes > 0, NewMax is MaxNodes - 1, 
    infrastructure(N, NewCurrHW, NewMissingHW, MaxLat, MinBW, NewMax, [M|Nodes], NewNodes, TotHW).

goodNeighbour(N, M, H, MaxLat, MinBW) :- 
    node(M,_,H,_), dif(N,M),
    link(N,M,LNM,BNM), LNM < MaxLat, BNM > MinBW,
    link(M,N,LMN,BMN), LMN < MaxLat, BMN > MinBW.
