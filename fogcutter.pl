:- discontiguous aggregatedListValue/3. 
:- consult('scenarios/smarttraffic.pl').
:-set_prolog_flag(stack_limit, 256 000 000 000).
:-set_prolog_flag(last_call_optimisation, true).

fogCutter(RequestId, (Portion,Profit)) :- 
    setof(X, portion(RequestId, X), CandidatePortions), 
    bestPortion(CandidatePortions, (Portion,Profit)).

bestPortion(CandidatePortions, (Portion,Profit)) :- 
    member((Portion,Profit), CandidatePortions), 
    \+ (member((_,HigherProfit), CandidatePortions), Profit < HigherProfit). 

portion(RequestId, (Portion, Profit)) :-
    request(RequestId, N, MaxNodes, Reqs),      
    splitRequirements(Reqs, NodeReqs, LinkReqs, GlobalReqs),
    portion(N, MaxNodes, NodeReqs, LinkReqs, GlobalReqs, [N], Portion),
    portionProfit(Portion, Profit).

portionProfit([N|Ns], Profit) :-
    profit(N, N_Profit),
    portionProfit(Ns, Ns_Profit),
    Profit is N_Profit + Ns_Profit.
portionProfit([], 0).

splitRequirements([],[],[],[]).
splitRequirements([(P,V)|Rs],[(P,V,Op)|Ns],Ls,Gs) :- capType(P,node,Op), splitRequirements(Rs,Ns,Ls,Gs).
splitRequirements([(P,V)|Rs],Ns,[(P,V,Op)|Ls],Gs) :- capType(P,link,Op), splitRequirements(Rs,Ns,Ls,Gs).
splitRequirements([(P,V)|Rs],Ns,Ls,[(P,V,Op,Aggr)|Gs]) :- capType(P,node,Op,Aggr), splitRequirements(Rs,Ns,Ls,Gs).

portion(_, _, _, _, GlobalReqs, I, P) :-
    satisfiesGlobalReqs(GlobalReqs, I), sort(I, P).
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
