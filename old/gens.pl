findProfit() :-
    findall(profit(N,P), profit(N,P), Ps),
    printProfits(Ps).

printProfits([]).
printProfits([profit(N,P)|Ps]) :-
    write(profit(N,P)), write('.\n'),
    printProfits(Ps).

profit(N, P) :-
    nodeCap(N, hardware, H),
    nodeCap(N, security, L), length(L, S),
    nodeCap(N, location, Loc), locationProfit(Loc, LP),
    P is H/4 + S.

locationProfit(us, 1).
locationProfit(eu, 3).
locationProfit(ch, 1).


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

linkCap :- 
    findall(linkCap(A,B,C,D), linkCap(A,B,C,D), L),
    printLinkCaps(L).

printLinkCaps([]).
printLinkCaps([linkCap(A,B,C,D)|T]) :-
    write(linkCap(A,B,C,D)), write('.'), nl,
    printLinkCaps(T).

% AP -  AP (same location)
linkCap(A, B, latency, 10) :-
    type(A, ap), nodeCap(A, location, L),
    type(B, ap), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 100) :-
    type(A, ap), nodeCap(A, location, L),
    type(B, ap), nodeCap(B, location, L),
    dif(A,B).

% AP -  AP (different location)
linkCap(A, B, latency, 150) :-
    type(A, ap), nodeCap(A, location, L1),
    type(B, ap), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 25) :-
    type(A, ap), nodeCap(A, location, L1),
    type(B, ap), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).

% AP - Edge (same location)
linkCap(A, B, latency, 60) :-
    type(A, ap), nodeCap(A, location, L),
    type(B, edge), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, latency, 60) :-
    type(A, edge), nodeCap(A, location, L),
    type(B, ap), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 30) :-
    type(A, ap), nodeCap(A, location, L),
    type(B, edge), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 200) :-
    type(A, edge), nodeCap(A, location, L),
    type(B, ap), nodeCap(B, location, L),
    dif(A,B).
% AP - Cloud (same location)
linkCap(A, B, latency, 130) :-
    type(A, ap), nodeCap(A, location, L),
    type(B, cloud), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, latency, 130) :-
    type(A, cloud), nodeCap(A, location, L),
    type(B, ap), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 90) :-
    type(A, ap), nodeCap(A, location, L),
    type(B, cloud), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 15) :-
    type(A, cloud), nodeCap(A, location, L),
    type(B, ap), nodeCap(B, location, L),
    dif(A,B).

% AP - Cloud (different location)
linkCap(A, B, latency, 200) :-
    type(A, ap), nodeCap(A, location, L1),
    type(B, cloud), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, latency, 200) :-
    type(A, cloud), nodeCap(A, location, L1),
    type(B, ap), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 10) :-
    type(A, ap), nodeCap(A, location, L1),
    type(B, cloud), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 25) :-
    type(A, cloud), nodeCap(A, location, L1),
    type(B, ap), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).

% Edge - Edge (same location)
linkCap(A, B, latency, 30) :-
    type(A, edge), nodeCap(A, location, L),
    type(B, edge), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 200) :-
    type(A, edge), nodeCap(A, location, L),
    type(B, edge), nodeCap(B, location, L),
    dif(A,B).

% Edge - Edge (different location)
linkCap(A, B, latency, 130) :-
    type(A, edge), nodeCap(A, location, L1),
    type(B, edge), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 50) :-
    type(A, edge), nodeCap(A, location, L1),
    type(B, edge), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).

% Edge - Cloud (same location)
linkCap(A, B, latency, 100) :-
    type(A, edge), nodeCap(A, location, L),
    type(B, cloud), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, latency, 100) :-
    type(A, cloud), nodeCap(A, location, L),
    type(B, edge), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 100) :-
    type(A, edge), nodeCap(A, location, L),
    type(B, cloud), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 150) :-
    type(A, cloud), nodeCap(A, location, L),
    type(B, edge), nodeCap(B, location, L),
    dif(A,B).

% Edge - Cloud (different location)
linkCap(A, B, latency, 150) :-
    type(A, edge), nodeCap(A, location, L1),
    type(B, cloud), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, latency, 150) :- 
    type(A, cloud), nodeCap(A, location, L1),
    type(B, edge), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 50) :- 
    type(A, edge), nodeCap(A, location, L1),
    type(B, cloud), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 100) :-
    type(A, cloud), nodeCap(A, location, L1),
    type(B, edge), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).

% Cloud - Cloud (same location)
linkCap(A, B, latency, 20) :-
    type(A, cloud), nodeCap(A, location, L),
    type(B, cloud), nodeCap(B, location, L),
    dif(A,B).
linkCap(A, B, bandwidth, 1000) :-
    type(A, cloud), nodeCap(A, location, L),
    type(B, cloud), nodeCap(B, location, L),
    dif(A,B).

% Cloud - Cloud (different location)
linkCap(A, B, latency, 100) :-
    type(A, cloud), nodeCap(A, location, L1),
    type(B, cloud), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).
linkCap(A, B, bandwidth, 500) :-
    type(A, cloud), nodeCap(A, location, L1),
    type(B, cloud), nodeCap(B, location, L2),
    dif(A,B), dif(L1,L2).