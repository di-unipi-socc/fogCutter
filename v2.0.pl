:- discontiguous aggregatedListValue/3. 

fogCutter(RequestId, BestPortion) :- 
    setof(C, portionWithProfit(RequestId, C), Portions),
    bestPortion(Portions, BestPortion).

bestPortion(L, (P,Best)) :- 
    member((P,Best), L), \+ (member((P1,B1),L), dif(Best,B1), P < P1). 
    %sort(L, Tmp), reverse(Tmp, [(P,Best)|_]).

portionWithProfit(RequestId, (Profit, Portion)) :-
    request(RequestId, N, MaxNodes, Reqs),      
    splitRequests(Reqs, NodeReqs, LinkReqs, GlobalReqs),
    portion(N, MaxNodes, NodeReqs, LinkReqs, GlobalReqs, [N], Portion),
    portionProfit(Portion, Profit).

portionProfit([N|Ns], NewProfit) :-
    profit(N, Profit),
    portionProfit(Ns, TmpProfit),
    NewProfit is TmpProfit + Profit.
portionProfit([], 0).

splitRequests([],[],[],[]).
splitRequests([(P,V)|Rs],[(P,V,Op)|Ns],Ls,Gs) :- capType(P,node,Op), splitRequests(Rs,Ns,Ls,Gs).
splitRequests([(P,V)|Rs],Ns,[(P,V,Op)|Ls],Gs) :- capType(P,link,Op), splitRequests(Rs,Ns,Ls,Gs).
splitRequests([(P,V)|Rs],Ns,Ls,[(P,V,Op,Aggr)|Gs]) :- capType(P,node,Op,Aggr), splitRequests(Rs,Ns,Ls,Gs).

portion(_, _, _, _, GlobalReqs, I, I) :-
    satisfiesGlobalReqs(GlobalReqs, I).
portion(N, MaxNodes, NodeReqs, LinkReqs, GlobalReqs, I, NewI) :-
    MaxNodes > 0,
    \+ satisfiesGlobalReqs(GlobalReqs, I),
    node(M), \+ member(M,[N|I]),
    satisfiesNodeReqs(M,NodeReqs),
    satisfiesLinkReqs(M,N,LinkReqs),
    NewMaxNodes is MaxNodes-1,
    portion(N, NewMaxNodes, NodeReqs, LinkReqs, GlobalReqs, [M|I], NewI).

satisfiesGlobalReqs(Rs, [I|Is]) :-  satisfies(Rs, [I|Is]).

satisfies([], _).
satisfies([(P,V,Op,Ag)|Rs], I) :-
    findall(Vm, (member(M,I),nodeCap(M,P,Vm)), Vs),
    aggregatedListValue(Vs,Ag,GV), 
    compareValue(V,Op,GV),
    satisfies(Rs, I).

aggregatedListValue([V|Vs],Ag,AV):- aggregatedListValue(Vs,Ag,AV2), aggregatedValue(V,Ag,AV2,AV).

satisfiesNodeReqs(_,[]).
satisfiesNodeReqs(M,[(P,V,Op)|Rs]) :- 
    nodeCap(M,P,Vm), compareValue(Vm,Op,V),
    satisfiesNodeReqs(M,Rs).

satisfiesLinkReqs(_,_,[]).
satisfiesLinkReqs(M,N,[(P,V,Op)|Rs]) :- 
    linkCap(M,N,P,Vmn), compareValue(Vmn,Op,V),
    satisfiesLinkReqs(M,N,Rs).

%%%%%%% Default definition of requirement properties %%%%%%%%%%%%
capType(hardware,       node,   smaller,    sum).
capType(availability,   node,   smaller,    product).
capType(sustainability, node,   smaller,    product).
capType(security,       node,   supset).
capType(location,       node,   member).
capType(latency,        link,   smaller).
capType(bandwidth,      link,   greater).

aggregatedListValue([],sum,0).
aggregatedListValue([],product,1).

aggregatedValue(X,sum,Y,Z) :- Z is X+Y.
aggregatedValue(X,product,Y,Z) :- Z is X*Y.

compareValue(X,smaller,Y) :- X < Y. 
compareValue(X,greater,Y) :- X > Y. 
compareValue(X,supset, Y) :- subset(Y,X).       
compareValue(X,member,Y) :- member(X,Y).
compareValue(X,smaller,Y) :- X < Y.

%%%%%%% Example of request %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% request( RequestId, SourceNodeId, MaxNodes, HardReqs, SoftReqs)
request(req42, ap0, 3, [(hardware,12), (latency,200),(bandwidth,1),(security,[antimalware]), (location,([eu,us])), (availability, 0.7)]).

%%%%%%% Example of portionstructure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous node/1.  
:- discontiguous nodeCap/3.
:- discontiguous linkCap/3.

node(sp1).
nodeCap(sp1, hardware, 8).   
nodeCap(sp1, location, eu).   
nodeCap(sp1, security, [antimalware, encryptedStorage]). 
nodeCap(sp1, availability, 0.999).   
profit(sp1, 1).

node(sp2).
nodeCap(sp2, hardware, 8).   
nodeCap(sp2, location, us).   
nodeCap(sp2, security, [antimalware]). 
nodeCap(sp2, availability, 0.95).   
profit(sp2, 10).

node(ap0).
nodeCap(ap0, hardware, 12).   
nodeCap(ap0, location, eu).   
nodeCap(ap0, security, [pa2, firewall, antimalware]). 
nodeCap(ap0, availability, 0.99).  
profit(ap0, 9). 

linkCap(ap10, sp1, latency, 10).
linkCap(ap10, sp1, bandwidth, 15).
linkCap(sp1, ap0, latency, 10).
linkCap(sp1, ap0, bandwidth, 15).

linkCap(ap10, sp2, latency, 10).
linkCap(ap10, sp2, bandwidth, 15).
linkCap(sp2, ap0, latency, 10).
linkCap(sp2, ap0, bandwidth, 15).
