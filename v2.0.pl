:- discontiguous aggregatedListValue/3. 

myCutFor(RequestId, I) :-
    request(RequestId, N, MaxNodes, Reqs),      
    splitRequests(Reqs, NodeReqs, LinkReqs, GlobalReqs),
    infra(N, MaxNodes, NodeReqs, LinkReqs, GlobalReqs, [], I).

splitRequests([],[],[],[]).
splitRequests([(P,V)|Rs],[(P,V,Op)|Ns],Ls,Gs) :- reqType(P,node,Op), splitRequests(Rs,Ns,Ls,Gs).
splitRequests([(P,V)|Rs],Ns,[(P,V,Op)|Ls],Gs) :- reqType(P,link,Op), splitRequests(Rs,Ns,Ls,Gs).
splitRequests([(P,V)|Rs],Ns,Ls,[(P,V,Op,Aggr)|Gs]) :- reqType(P,node,Op,Aggr), splitRequests(Rs,Ns,Ls,Gs).

infra(_, _, _, _, GlobalReqs, I, I) :-
    satisfiesGlobalReqs(GlobalReqs, I).

infra(N, MaxNodes, NodeReqs, LinkReqs, GlobalReqs, I, NewI) :-
    MaxNodes > 0,
    \+ satisfiesGlobalReqs(GlobalReqs, I),
    node(M), \+member(M,[N|I]),
    satisfiesNodeReqs(M,NodeReqs),
    satisfiesLinkReqs(M,N,LinkReqs),
    NewMaxNodes is MaxNodes-1,
    infra(N, NewMaxNodes, NodeReqs, LinkReqs, GlobalReqs, [M|I], NewI).

satisfiesGlobalReqs(Rs, [I|Is]) :-   satisfiesGlobalReqs2(Rs, [I|Is]).

satisfiesGlobalReqs2([], _).
satisfiesGlobalReqs2([(P,V,Op,Ag)|Rs], I) :-
    findall(Vm, (member(M,I),nodecap(M,P,Vm)), Vs),
    aggregatedListValue(Vs,Ag,GV), 
    compareValue(V,Op,GV),
    satisfiesGlobalReqs2(Rs, I).

aggregatedListValue([V|Vs],Ag,AV):- aggregatedListValue(Vs,Ag,AV2), aggregatedValue(V,Ag,AV2,AV).

satisfiesNodeReqs(_,[]).
satisfiesNodeReqs(M,[(P,V,Op)|Rs]) :- 
    nodecap(M,P,Vm), compareValue(Vm,Op,V),
    satisfiesNodeReqs(M,Rs).

satisfiesLinkReqs(_,_,[]).
satisfiesLinkReqs(M,N,[(P,V,Op)|Rs]) :- 
    linkcap(M,N,P,Vmn), compareValue(Vmn,Op,V),
    satisfiesLinkReqs(M,N,Rs).

%%%%%%% Default definition of requirement properties %%%%%%%%%%%%
reqType(hardware,       node,   smaller,    sum).
reqType(availability,   node,   smaller,    product).
reqType(security,       node,   supset).
reqType(location,       node,   member).
reqType(latency,        link,   smaller).
reqType(bandwidth,      link,   greater).

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
request(req42, ap0, 3, [(hardware,12), (latency,200),(bandwidth,1),(security,[antimalware]), (location,([eu,us])), (availability, 0.8)]).

%%%%%%% Example of infrastructure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous node/1.  
:- discontiguous nodecap/3.
:- discontiguous linkcap/3.

node(sp1).
nodecap(sp1, hardware, 8).   
nodecap(sp1, location, eu).   
nodecap(sp1, security, [antimalware, encryptedStorage]). 
nodecap(sp1, availability, 0.9).   

node(sp2).
nodecap(sp2, hardware, 8).   
nodecap(sp2, location, us).   
nodecap(sp2, security, [antimalware]). 
nodecap(sp2, availability, 0.95).   

node(ap0).
nodecap(ap0, hardware, 12).   
nodecap(ap0, location, eu).   
nodecap(ap0, security, [pa2, firewall, antimalware]). 
nodecap(ap0, availability, 0.99).   

linkcap(ap10, sp1, latency, 10).
linkcap(ap10, sp1, bandwidth, 15).
linkcap(sp1, ap0, latency, 10).
linkcap(sp1, ap0, bandwidth, 15).

linkcap(ap10, sp2, latency, 10).
linkcap(ap10, sp2, bandwidth, 15).
linkcap(sp2, ap0, latency, 10).
linkcap(sp2, ap0, bandwidth, 15).