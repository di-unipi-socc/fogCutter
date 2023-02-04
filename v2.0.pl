:- discontiguous aggregatedListValue/3. 
:- use_module(library(random)).

start :- 
    once(generateLinks()),
    fogCutter(req42, BestPortion),
    write(BestPortion).

fogCutter(RequestId, BestPortion) :- 
    setof(C, portionWithProfit(RequestId, C), Portions),
    bestPortion(Portions, BestPortion).

bestPortion(L, (P,Best)) :- 
    member((P,Best), L), \+ (member((P1,B1),L), dif(Best,B1), P < P1). %sort(L, Tmp), reverse(Tmp, [(P,Best)|_]).

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
request(req42, ap5, 3, [(hardware,4), (latency,100),(bandwidth,1),(security,[antimalware]), (location,([eu,us])), (availability, 0.7)]).

%%%%%%% Example of portionstructure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous node/1.  
:- discontiguous nodeCap/3.
:- dynamic linkCap/4.

type(c1, cloud).
type(c2, cloud).
type(c3, cloud).

type(ap1, ap).
type(ap2, ap).
type(ap3, ap).
type(ap4, ap).
type(ap5, ap).
type(ap6, ap).
type(ap7, ap).
type(ap8, ap).
type(ap9, ap).
type(ap10, ap).

type(n1, edge).
type(n2, edge).
type(n3, edge).
type(n4, edge).
type(n5, edge).
type(n6, edge).
type(n7, edge).

% EU AP - EU AP
linkCap(A, B, latency, 10) :-
    type(A, ap), nodeCap(A, location, eu),
    type(B, ap), nodeCap(B, location, eu).
    dif(A,B).
linkCap(A, B, bandwidth, 50) :-
    type(A, ap), nodeCap(A, location, eu),
    type(B, ap), nodeCap(B, location, eu).
    dif(A,B).


profit(_, 10).

node(ap1).
nodeCap(ap1, hardware, 2).
nodeCap(ap1, location, us). 
nodeCap(ap1, security, [encryptedStorage, antimalware]).
nodeCap(ap1, availability, 0.9).   
nodeCap(ap1, sustainability, 1).

node(ap2).
nodeCap(ap2, hardware, 4).
nodeCap(ap2, location, us). 
nodeCap(ap2, security, []).   
nodeCap(ap2, availability, 0.8).
nodeCap(ap2, sustainability, 0.25). 

node(ap3).
nodeCap(ap3, hardware, 3).
nodeCap(ap3, location, eu).
nodeCap(ap3, security, [encryptedStorage]).
nodeCap(ap3, availability, 0.85).
nodeCap(ap3, sustainability, 1).

node(ap4).
nodeCap(ap4, hardware, 4).
nodeCap(ap4, location, eu).
nodeCap(ap4, security, [encryptedStorage, antimalware]).
node(ap4, availability, 0.99).
node(ap4, sustainability, 0.75).

node(ap5).
nodeCap(ap5, hardware, 2).
nodeCap(ap5, location, eu).
nodeCap(ap5, security, [encryptedStorage, antimalware]).
nodeCap(ap5, availability, 0.99).
nodeCap(ap5, sustainability, 0.25).

node(ap6).
nodeCap(ap6, hardware, 2).
nodeCap(ap6, location, eu).
nodeCap(ap6, security, [antimalware]).
nodeCap(ap6, availability, 0.9).
nodeCap(ap6, sustainability, 0.75).

node(ap7).
nodeCap(ap7, hardware, 4).
nodeCap(ap7, location, eu).
nodeCap(ap7, security, [encryptedStorage, antimalware]).
nodeCap(ap7, availability, 0.8).
nodeCap(ap7, sustainability, 0.75).

node(ap8).
nodeCap(ap8, hardware, 1).
nodeCap(ap8, location, eu).
nodeCap(ap8, security, [encryptedStorage, antimalware]).
nodeCap(ap8, availability, 0.99).
nodeCap(ap8, sustainability, 0.5).

node(ap9).
nodeCap(ap9, hardware, 3).
nodeCap(ap9, location, ch).
nodeCap(ap9, security, [encryptedStorage]).
nodeCap(ap9, availability, 0.999).
nodeCap(ap9, sustainability, 0.5).

node(ap10).
nodeCap(ap10, hardware, 8).
nodeCap(ap10, location, ch).
nodeCap(ap10, security, [encryptedStorage, antimalware]).
nodeCap(ap10, availability, 0.7).
nodeCap(ap10, sustainability, 1).

node(n1).
nodeCap(n1, hardware, 8).
nodeCap(n1, location, us).
nodeCap(n1, security, [antimalware, encryptedStorage]).
nodeCap(n1, availability, 0.9).
nodeCap(n1, sustainability, 0.25).

node(n2).
nodeCap(n2, hardware, 12).
nodeCap(n2, location, eu).
nodeCap(n2, security, [antimalware, encryptedStorage]).
nodeCap(n2, availability, 0.85).
nodeCap(n2, sustainability, 0.75).

node(n3).
nodeCap(n3, hardware, 4).
nodeCap(n3, location, eu).
nodeCap(n3, security, [audit, encryptedStorage]).
nodeCap(n3, availability, 0.9).
nodeCap(n3, sustainability, 1).

node(n4).
nodeCap(n4, hardware, 12).
nodeCap(n4, location, eu).
nodeCap(n4, security, [antimalware]).
nodeCap(n4, availability, 0.98).
nodeCap(n4, sustainability, 0.5).

node(n5).
nodeCap(n5, hardware, 2).
nodeCap(n5, location, eu).
nodeCap(n5, security, [antimalware, encryptedStorage]).
nodeCap(n5, availability, 0.99).
nodeCap(n5, sustainability, 0.25).

node(n6).
nodeCap(n6, hardware, 8).
nodeCap(n6, location, eu).
nodeCap(n6, security, [antimalware, encryptedStorage]).
nodeCap(n6, availability, 0.9).
nodeCap(n6, sustainability, 1).

node(n7).
nodeCap(n7, hardware, 8).
nodeCap(n7, location, ch).
nodeCap(n7, security, [audit, encryptedStorage]).
nodeCap(n7, availability, 0.9).
nodeCap(n7, sustainability, 0.5).

node(c1).
nodeCap(c1, hardware, 24).
nodeCap(c1, location, us).
nodeCap(c1, security, [antimalware, encryptedStorage, audit]).
nodeCap(c1, availability, 0.99).
nodeCap(c1, sustainability, 0.75).

node(c2).
nodeCap(c2, hardware, 16).
nodeCap(c2, location, eu).
nodeCap(c2, security, [antimalware, encryptedStorage, audit]).
nodeCap(c2, availability, 0.95).
nodeCap(c2, sustainability, 0.5).

node(c3).
nodeCap(c2, hardware, 12).
nodeCap(c3, location, ch).
nodeCap(c3, security, [antimalware, encryptedStorage, audit]). 
nodeCap(c3, availability, 0.999).
nodeCap(c3, sustainability, 1).