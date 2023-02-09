# fogCutter
_A logic programming solution to resource selection in the Cloud-Edge continuum_

# Quick guide

The fogCutter prototype inputs a knowledge base made of:

-  *nodes* with their (hardware, location, security, availability, etc.) capabilities. For instance:
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