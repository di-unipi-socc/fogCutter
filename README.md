<p><img align="left"  src="https://github.com/di-unipi-socc/fogCutter/blob/main/logo/png/logo-no-background.png?raw=true " width="300"> </p>
<br>


_A logic programming solution to resource selection in the Cloud-Edge continuum_

FogCutter solves the following problem with a declarative approach:

> Let `N` be a set of heterogeneous nodes of a Cloud-Edge infrastructure managed by an infrastructure provider.
  Let `p: N -> R` be the function that defines the profit of the infrastructure provider for leasing a node.
  Let `Req` be a request of resources of an application operator.
  A solution to the considered resource selection problem is a portion `C = {n1, n2, ... nM}` in `N` that guarantees all requirements in `Req` while maximising the infrastructure provider profit.

# Quick guide

## Prerequisites

To run FogCutter, simply download and install [SWI-Prolog](https://www.swi-prolog.org/Download.html).

## Knowledge base

The FogCutter prototype inputs a knowledge base made of:

-  *nodes* with their (hardware, location, security, availability, etc.) capabilities and their profit for the infrastructure provider. For instance:
```prolog
node(ap1).
nodeCap(ap1, hardware, 2).
nodeCap(ap1, location, us). 
nodeCap(ap1, security, [encryptedStorage, antimalware]).
nodeCap(ap1, availability, 0.9).   
nodeCap(ap1, sustainability, 1). 
```

- end-to-end links with their (latency, bandwidth, etc.) capabilities. For instance:
```prolog
linkCap(ap1,ap2,latency,10). # 10 ms latency
linkCap(ap2,ap1,latency,10).
linkCap(ap1,ap2,bandwidth,100). # 100 Mbps
linkCap(ap2,ap1,bandwidth,100). 
```
- a request from the application operator specifying the requirements for a portion of Cloud-Edge infrastructure made of at most `Max` nodes:
```prolog
% request(ReqId, SourceNode, Max, ReqsList).
request(req40, ap3, 7, [(hardware,20), (latency,250), (bandwidth,10)]).
request(req41, ap3, 10, [(hardware,20), (latency,250),(bandwidth,10),(security,[antimalware, encryptedStorage]), (location,[eu])]).
request(req42, ap3, 3, [(hardware,20), (latency,250),(bandwidth,10),(security,[antimalware, encryptedStorage]), (location,[eu]), (availability, 0.85), (sustainability, 0.3)]).
```

## Query

To determine a Cloud-Edge portion that satisfy a request, simply query the predicate:

```prolog
fogCutter(req42,Portion).
```

which, over the knowledge base in `scenarios/smarttraffic.pl`, returns:

```prolog
Portion = ([ap3, ap4, c2], 11.75).
```