%%%%%%% Example of request %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% request( RequestId, SourceNodeId, MaxNodes, HardReqs, SoftReqs)
request(req42, ap5, 7, [(hardware,20), (latency,150),(bandwidth,10),(security,[antimalware]), (location,([eu,us])), (availability, 0.8), (sustainability, 0.0)]).


%%%%%%% Example of infrastructure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous node/1.  
:- discontiguous nodeCap/3.
:- dynamic linkCap/4.

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

linkCap(ap1,ap2,latency,10).
linkCap(ap2,ap1,latency,10).
linkCap(ap3,ap4,latency,10).
linkCap(ap3,ap5,latency,10).
linkCap(ap3,ap6,latency,10).
linkCap(ap3,ap7,latency,10).
linkCap(ap3,ap8,latency,10).
linkCap(ap4,ap3,latency,10).
linkCap(ap4,ap5,latency,10).
linkCap(ap4,ap6,latency,10).
linkCap(ap4,ap7,latency,10).
linkCap(ap4,ap8,latency,10).
linkCap(ap5,ap3,latency,10).
linkCap(ap5,ap4,latency,10).
linkCap(ap5,ap6,latency,10).
linkCap(ap5,ap7,latency,10).
linkCap(ap5,ap8,latency,10).
linkCap(ap6,ap3,latency,10).
linkCap(ap6,ap4,latency,10).
linkCap(ap6,ap5,latency,10).
linkCap(ap6,ap7,latency,10).
linkCap(ap6,ap8,latency,10).
linkCap(ap7,ap3,latency,10).
linkCap(ap7,ap4,latency,10).
linkCap(ap7,ap5,latency,10).
linkCap(ap7,ap6,latency,10).
linkCap(ap7,ap8,latency,10).
linkCap(ap8,ap3,latency,10).
linkCap(ap8,ap4,latency,10).
linkCap(ap8,ap5,latency,10).
linkCap(ap8,ap6,latency,10).
linkCap(ap8,ap7,latency,10).
linkCap(ap9,ap10,latency,10).
linkCap(ap10,ap9,latency,10).
linkCap(ap1,ap2,bandwidth,100).
linkCap(ap2,ap1,bandwidth,100).
linkCap(ap3,ap4,bandwidth,100).
linkCap(ap3,ap5,bandwidth,100).
linkCap(ap3,ap6,bandwidth,100).
linkCap(ap3,ap7,bandwidth,100).
linkCap(ap3,ap8,bandwidth,100).
linkCap(ap4,ap3,bandwidth,100).
linkCap(ap4,ap5,bandwidth,100).
linkCap(ap4,ap6,bandwidth,100).
linkCap(ap4,ap7,bandwidth,100).
linkCap(ap4,ap8,bandwidth,100).
linkCap(ap5,ap3,bandwidth,100).
linkCap(ap5,ap4,bandwidth,100).
linkCap(ap5,ap6,bandwidth,100).
linkCap(ap5,ap7,bandwidth,100).
linkCap(ap5,ap8,bandwidth,100).
linkCap(ap6,ap3,bandwidth,100).
linkCap(ap6,ap4,bandwidth,100).
linkCap(ap6,ap5,bandwidth,100).
linkCap(ap6,ap7,bandwidth,100).
linkCap(ap6,ap8,bandwidth,100).
linkCap(ap7,ap3,bandwidth,100).
linkCap(ap7,ap4,bandwidth,100).
linkCap(ap7,ap5,bandwidth,100).
linkCap(ap7,ap6,bandwidth,100).
linkCap(ap7,ap8,bandwidth,100).
linkCap(ap8,ap3,bandwidth,100).
linkCap(ap8,ap4,bandwidth,100).
linkCap(ap8,ap5,bandwidth,100).
linkCap(ap8,ap6,bandwidth,100).
linkCap(ap8,ap7,bandwidth,100).
linkCap(ap9,ap10,bandwidth,100).
linkCap(ap10,ap9,bandwidth,100).
linkCap(ap1,ap3,latency,150).
linkCap(ap1,ap4,latency,150).
linkCap(ap1,ap5,latency,150).
linkCap(ap1,ap6,latency,150).
linkCap(ap1,ap7,latency,150).
linkCap(ap1,ap8,latency,150).
linkCap(ap1,ap9,latency,150).
linkCap(ap1,ap10,latency,150).
linkCap(ap2,ap3,latency,150).
linkCap(ap2,ap4,latency,150).
linkCap(ap2,ap5,latency,150).
linkCap(ap2,ap6,latency,150).
linkCap(ap2,ap7,latency,150).
linkCap(ap2,ap8,latency,150).
linkCap(ap2,ap9,latency,150).
linkCap(ap2,ap10,latency,150).
linkCap(ap3,ap1,latency,150).
linkCap(ap3,ap2,latency,150).
linkCap(ap3,ap9,latency,150).
linkCap(ap3,ap10,latency,150).
linkCap(ap4,ap1,latency,150).
linkCap(ap4,ap2,latency,150).
linkCap(ap4,ap9,latency,150).
linkCap(ap4,ap10,latency,150).
linkCap(ap5,ap1,latency,150).
linkCap(ap5,ap2,latency,150).
linkCap(ap5,ap9,latency,150).
linkCap(ap5,ap10,latency,150).
linkCap(ap6,ap1,latency,150).
linkCap(ap6,ap2,latency,150).
linkCap(ap6,ap9,latency,150).
linkCap(ap6,ap10,latency,150).
linkCap(ap7,ap1,latency,150).
linkCap(ap7,ap2,latency,150).
linkCap(ap7,ap9,latency,150).
linkCap(ap7,ap10,latency,150).
linkCap(ap8,ap1,latency,150).
linkCap(ap8,ap2,latency,150).
linkCap(ap8,ap9,latency,150).
linkCap(ap8,ap10,latency,150).
linkCap(ap9,ap1,latency,150).
linkCap(ap9,ap2,latency,150).
linkCap(ap9,ap3,latency,150).
linkCap(ap9,ap4,latency,150).
linkCap(ap9,ap5,latency,150).
linkCap(ap9,ap6,latency,150).
linkCap(ap9,ap7,latency,150).
linkCap(ap9,ap8,latency,150).
linkCap(ap10,ap1,latency,150).
linkCap(ap10,ap2,latency,150).
linkCap(ap10,ap3,latency,150).
linkCap(ap10,ap4,latency,150).
linkCap(ap10,ap5,latency,150).
linkCap(ap10,ap6,latency,150).
linkCap(ap10,ap7,latency,150).
linkCap(ap10,ap8,latency,150).
linkCap(ap1,ap3,bandwidth,25).
linkCap(ap1,ap4,bandwidth,25).
linkCap(ap1,ap5,bandwidth,25).
linkCap(ap1,ap6,bandwidth,25).
linkCap(ap1,ap7,bandwidth,25).
linkCap(ap1,ap8,bandwidth,25).
linkCap(ap1,ap9,bandwidth,25).
linkCap(ap1,ap10,bandwidth,25).
linkCap(ap2,ap3,bandwidth,25).
linkCap(ap2,ap4,bandwidth,25).
linkCap(ap2,ap5,bandwidth,25).
linkCap(ap2,ap6,bandwidth,25).
linkCap(ap2,ap7,bandwidth,25).
linkCap(ap2,ap8,bandwidth,25).
linkCap(ap2,ap9,bandwidth,25).
linkCap(ap2,ap10,bandwidth,25).
linkCap(ap3,ap1,bandwidth,25).
linkCap(ap3,ap2,bandwidth,25).
linkCap(ap3,ap9,bandwidth,25).
linkCap(ap3,ap10,bandwidth,25).
linkCap(ap4,ap1,bandwidth,25).
linkCap(ap4,ap2,bandwidth,25).
linkCap(ap4,ap9,bandwidth,25).
linkCap(ap4,ap10,bandwidth,25).
linkCap(ap5,ap1,bandwidth,25).
linkCap(ap5,ap2,bandwidth,25).
linkCap(ap5,ap9,bandwidth,25).
linkCap(ap5,ap10,bandwidth,25).
linkCap(ap6,ap1,bandwidth,25).
linkCap(ap6,ap2,bandwidth,25).
linkCap(ap6,ap9,bandwidth,25).
linkCap(ap6,ap10,bandwidth,25).
linkCap(ap7,ap1,bandwidth,25).
linkCap(ap7,ap2,bandwidth,25).
linkCap(ap7,ap9,bandwidth,25).
linkCap(ap7,ap10,bandwidth,25).
linkCap(ap8,ap1,bandwidth,25).
linkCap(ap8,ap2,bandwidth,25).
linkCap(ap8,ap9,bandwidth,25).
linkCap(ap8,ap10,bandwidth,25).
linkCap(ap9,ap1,bandwidth,25).
linkCap(ap9,ap2,bandwidth,25).
linkCap(ap9,ap3,bandwidth,25).
linkCap(ap9,ap4,bandwidth,25).
linkCap(ap9,ap5,bandwidth,25).
linkCap(ap9,ap6,bandwidth,25).
linkCap(ap9,ap7,bandwidth,25).
linkCap(ap9,ap8,bandwidth,25).
linkCap(ap10,ap1,bandwidth,25).
linkCap(ap10,ap2,bandwidth,25).
linkCap(ap10,ap3,bandwidth,25).
linkCap(ap10,ap4,bandwidth,25).
linkCap(ap10,ap5,bandwidth,25).
linkCap(ap10,ap6,bandwidth,25).
linkCap(ap10,ap7,bandwidth,25).
linkCap(ap10,ap8,bandwidth,25).
linkCap(ap1,n1,latency,60).
linkCap(ap2,n1,latency,60).
linkCap(ap3,n2,latency,60).
linkCap(ap3,n3,latency,60).
linkCap(ap3,n4,latency,60).
linkCap(ap3,n5,latency,60).
linkCap(ap3,n6,latency,60).
linkCap(ap4,n2,latency,60).
linkCap(ap4,n3,latency,60).
linkCap(ap4,n4,latency,60).
linkCap(ap4,n5,latency,60).
linkCap(ap4,n6,latency,60).
linkCap(ap5,n2,latency,60).
linkCap(ap5,n3,latency,60).
linkCap(ap5,n4,latency,60).
linkCap(ap5,n5,latency,60).
linkCap(ap5,n6,latency,60).
linkCap(ap6,n2,latency,60).
linkCap(ap6,n3,latency,60).
linkCap(ap6,n4,latency,60).
linkCap(ap6,n5,latency,60).
linkCap(ap6,n6,latency,60).
linkCap(ap7,n2,latency,60).
linkCap(ap7,n3,latency,60).
linkCap(ap7,n4,latency,60).
linkCap(ap7,n5,latency,60).
linkCap(ap7,n6,latency,60).
linkCap(ap8,n2,latency,60).
linkCap(ap8,n3,latency,60).
linkCap(ap8,n4,latency,60).
linkCap(ap8,n5,latency,60).
linkCap(ap8,n6,latency,60).
linkCap(ap9,n7,latency,60).
linkCap(ap10,n7,latency,60).
linkCap(n1,ap1,latency,60).
linkCap(n1,ap2,latency,60).
linkCap(n2,ap3,latency,60).
linkCap(n2,ap4,latency,60).
linkCap(n2,ap5,latency,60).
linkCap(n2,ap6,latency,60).
linkCap(n2,ap7,latency,60).
linkCap(n2,ap8,latency,60).
linkCap(n3,ap3,latency,60).
linkCap(n3,ap4,latency,60).
linkCap(n3,ap5,latency,60).
linkCap(n3,ap6,latency,60).
linkCap(n3,ap7,latency,60).
linkCap(n3,ap8,latency,60).
linkCap(n4,ap3,latency,60).
linkCap(n4,ap4,latency,60).
linkCap(n4,ap5,latency,60).
linkCap(n4,ap6,latency,60).
linkCap(n4,ap7,latency,60).
linkCap(n4,ap8,latency,60).
linkCap(n5,ap3,latency,60).
linkCap(n5,ap4,latency,60).
linkCap(n5,ap5,latency,60).
linkCap(n5,ap6,latency,60).
linkCap(n5,ap7,latency,60).
linkCap(n5,ap8,latency,60).
linkCap(n6,ap3,latency,60).
linkCap(n6,ap4,latency,60).
linkCap(n6,ap5,latency,60).
linkCap(n6,ap6,latency,60).
linkCap(n6,ap7,latency,60).
linkCap(n6,ap8,latency,60).
linkCap(n7,ap9,latency,60).
linkCap(n7,ap10,latency,60).
linkCap(ap1,n1,bandwidth,30).
linkCap(ap2,n1,bandwidth,30).
linkCap(ap3,n2,bandwidth,30).
linkCap(ap3,n3,bandwidth,30).
linkCap(ap3,n4,bandwidth,30).
linkCap(ap3,n5,bandwidth,30).
linkCap(ap3,n6,bandwidth,30).
linkCap(ap4,n2,bandwidth,30).
linkCap(ap4,n3,bandwidth,30).
linkCap(ap4,n4,bandwidth,30).
linkCap(ap4,n5,bandwidth,30).
linkCap(ap4,n6,bandwidth,30).
linkCap(ap5,n2,bandwidth,30).
linkCap(ap5,n3,bandwidth,30).
linkCap(ap5,n4,bandwidth,30).
linkCap(ap5,n5,bandwidth,30).
linkCap(ap5,n6,bandwidth,30).
linkCap(ap6,n2,bandwidth,30).
linkCap(ap6,n3,bandwidth,30).
linkCap(ap6,n4,bandwidth,30).
linkCap(ap6,n5,bandwidth,30).
linkCap(ap6,n6,bandwidth,30).
linkCap(ap7,n2,bandwidth,30).
linkCap(ap7,n3,bandwidth,30).
linkCap(ap7,n4,bandwidth,30).
linkCap(ap7,n5,bandwidth,30).
linkCap(ap7,n6,bandwidth,30).
linkCap(ap8,n2,bandwidth,30).
linkCap(ap8,n3,bandwidth,30).
linkCap(ap8,n4,bandwidth,30).
linkCap(ap8,n5,bandwidth,30).
linkCap(ap8,n6,bandwidth,30).
linkCap(ap9,n7,bandwidth,30).
linkCap(ap10,n7,bandwidth,30).
linkCap(n1,ap1,bandwidth,200).
linkCap(n1,ap2,bandwidth,200).
linkCap(n2,ap3,bandwidth,200).
linkCap(n2,ap4,bandwidth,200).
linkCap(n2,ap5,bandwidth,200).
linkCap(n2,ap6,bandwidth,200).
linkCap(n2,ap7,bandwidth,200).
linkCap(n2,ap8,bandwidth,200).
linkCap(n3,ap3,bandwidth,200).
linkCap(n3,ap4,bandwidth,200).
linkCap(n3,ap5,bandwidth,200).
linkCap(n3,ap6,bandwidth,200).
linkCap(n3,ap7,bandwidth,200).
linkCap(n3,ap8,bandwidth,200).
linkCap(n4,ap3,bandwidth,200).
linkCap(n4,ap4,bandwidth,200).
linkCap(n4,ap5,bandwidth,200).
linkCap(n4,ap6,bandwidth,200).
linkCap(n4,ap7,bandwidth,200).
linkCap(n4,ap8,bandwidth,200).
linkCap(n5,ap3,bandwidth,200).
linkCap(n5,ap4,bandwidth,200).
linkCap(n5,ap5,bandwidth,200).
linkCap(n5,ap6,bandwidth,200).
linkCap(n5,ap7,bandwidth,200).
linkCap(n5,ap8,bandwidth,200).
linkCap(n6,ap3,bandwidth,200).
linkCap(n6,ap4,bandwidth,200).
linkCap(n6,ap5,bandwidth,200).
linkCap(n6,ap6,bandwidth,200).
linkCap(n6,ap7,bandwidth,200).
linkCap(n6,ap8,bandwidth,200).
linkCap(n7,ap9,bandwidth,200).
linkCap(n7,ap10,bandwidth,200).
linkCap(ap1,c1,latency,130).
linkCap(ap2,c1,latency,130).
linkCap(ap3,c2,latency,130).
linkCap(ap4,c2,latency,130).
linkCap(ap5,c2,latency,130).
linkCap(ap6,c2,latency,130).
linkCap(ap7,c2,latency,130).
linkCap(ap8,c2,latency,130).
linkCap(ap9,c3,latency,130).
linkCap(ap10,c3,latency,130).
linkCap(c1,ap1,latency,130).
linkCap(c1,ap2,latency,130).
linkCap(c2,ap3,latency,130).
linkCap(c2,ap4,latency,130).
linkCap(c2,ap5,latency,130).
linkCap(c2,ap6,latency,130).
linkCap(c2,ap7,latency,130).
linkCap(c2,ap8,latency,130).
linkCap(c3,ap9,latency,130).
linkCap(c3,ap10,latency,130).
linkCap(ap1,c1,bandwidth,90).
linkCap(ap2,c1,bandwidth,90).
linkCap(ap3,c2,bandwidth,90).
linkCap(ap4,c2,bandwidth,90).
linkCap(ap5,c2,bandwidth,90).
linkCap(ap6,c2,bandwidth,90).
linkCap(ap7,c2,bandwidth,90).
linkCap(ap8,c2,bandwidth,90).
linkCap(ap9,c3,bandwidth,90).
linkCap(ap10,c3,bandwidth,90).
linkCap(c1,ap1,bandwidth,15).
linkCap(c1,ap2,bandwidth,15).
linkCap(c2,ap3,bandwidth,15).
linkCap(c2,ap4,bandwidth,15).
linkCap(c2,ap5,bandwidth,15).
linkCap(c2,ap6,bandwidth,15).
linkCap(c2,ap7,bandwidth,15).
linkCap(c2,ap8,bandwidth,15).
linkCap(c3,ap9,bandwidth,15).
linkCap(c3,ap10,bandwidth,15).
linkCap(ap1,c2,latency,200).
linkCap(ap1,c3,latency,200).
linkCap(ap2,c2,latency,200).
linkCap(ap2,c3,latency,200).
linkCap(ap3,c1,latency,200).
linkCap(ap3,c3,latency,200).
linkCap(ap4,c1,latency,200).
linkCap(ap4,c3,latency,200).
linkCap(ap5,c1,latency,200).
linkCap(ap5,c3,latency,200).
linkCap(ap6,c1,latency,200).
linkCap(ap6,c3,latency,200).
linkCap(ap7,c1,latency,200).
linkCap(ap7,c3,latency,200).
linkCap(ap8,c1,latency,200).
linkCap(ap8,c3,latency,200).
linkCap(ap9,c1,latency,200).
linkCap(ap9,c2,latency,200).
linkCap(ap10,c1,latency,200).
linkCap(ap10,c2,latency,200).
linkCap(c1,ap3,latency,200).
linkCap(c1,ap4,latency,200).
linkCap(c1,ap5,latency,200).
linkCap(c1,ap6,latency,200).
linkCap(c1,ap7,latency,200).
linkCap(c1,ap8,latency,200).
linkCap(c1,ap9,latency,200).
linkCap(c1,ap10,latency,200).
linkCap(c2,ap1,latency,200).
linkCap(c2,ap2,latency,200).
linkCap(c2,ap9,latency,200).
linkCap(c2,ap10,latency,200).
linkCap(c3,ap1,latency,200).
linkCap(c3,ap2,latency,200).
linkCap(c3,ap3,latency,200).
linkCap(c3,ap4,latency,200).
linkCap(c3,ap5,latency,200).
linkCap(c3,ap6,latency,200).
linkCap(c3,ap7,latency,200).
linkCap(c3,ap8,latency,200).
linkCap(ap1,c2,bandwidth,10).
linkCap(ap1,c3,bandwidth,10).
linkCap(ap2,c2,bandwidth,10).
linkCap(ap2,c3,bandwidth,10).
linkCap(ap3,c1,bandwidth,10).
linkCap(ap3,c3,bandwidth,10).
linkCap(ap4,c1,bandwidth,10).
linkCap(ap4,c3,bandwidth,10).
linkCap(ap5,c1,bandwidth,10).
linkCap(ap5,c3,bandwidth,10).
linkCap(ap6,c1,bandwidth,10).
linkCap(ap6,c3,bandwidth,10).
linkCap(ap7,c1,bandwidth,10).
linkCap(ap7,c3,bandwidth,10).
linkCap(ap8,c1,bandwidth,10).
linkCap(ap8,c3,bandwidth,10).
linkCap(ap9,c1,bandwidth,10).
linkCap(ap9,c2,bandwidth,10).
linkCap(ap10,c1,bandwidth,10).
linkCap(ap10,c2,bandwidth,10).
linkCap(c1,ap3,bandwidth,25).
linkCap(c1,ap4,bandwidth,25).
linkCap(c1,ap5,bandwidth,25).
linkCap(c1,ap6,bandwidth,25).
linkCap(c1,ap7,bandwidth,25).
linkCap(c1,ap8,bandwidth,25).
linkCap(c1,ap9,bandwidth,25).
linkCap(c1,ap10,bandwidth,25).
linkCap(c2,ap1,bandwidth,25).
linkCap(c2,ap2,bandwidth,25).
linkCap(c2,ap9,bandwidth,25).
linkCap(c2,ap10,bandwidth,25).
linkCap(c3,ap1,bandwidth,25).
linkCap(c3,ap2,bandwidth,25).
linkCap(c3,ap3,bandwidth,25).
linkCap(c3,ap4,bandwidth,25).
linkCap(c3,ap5,bandwidth,25).
linkCap(c3,ap6,bandwidth,25).
linkCap(c3,ap7,bandwidth,25).
linkCap(c3,ap8,bandwidth,25).
linkCap(n2,n3,latency,30).
linkCap(n2,n4,latency,30).
linkCap(n2,n5,latency,30).
linkCap(n2,n6,latency,30).
linkCap(n3,n2,latency,30).
linkCap(n3,n4,latency,30).
linkCap(n3,n5,latency,30).
linkCap(n3,n6,latency,30).
linkCap(n4,n2,latency,30).
linkCap(n4,n3,latency,30).
linkCap(n4,n5,latency,30).
linkCap(n4,n6,latency,30).
linkCap(n5,n2,latency,30).
linkCap(n5,n3,latency,30).
linkCap(n5,n4,latency,30).
linkCap(n5,n6,latency,30).
linkCap(n6,n2,latency,30).
linkCap(n6,n3,latency,30).
linkCap(n6,n4,latency,30).
linkCap(n6,n5,latency,30).
linkCap(n2,n3,bandwidth,200).
linkCap(n2,n4,bandwidth,200).
linkCap(n2,n5,bandwidth,200).
linkCap(n2,n6,bandwidth,200).
linkCap(n3,n2,bandwidth,200).
linkCap(n3,n4,bandwidth,200).
linkCap(n3,n5,bandwidth,200).
linkCap(n3,n6,bandwidth,200).
linkCap(n4,n2,bandwidth,200).
linkCap(n4,n3,bandwidth,200).
linkCap(n4,n5,bandwidth,200).
linkCap(n4,n6,bandwidth,200).
linkCap(n5,n2,bandwidth,200).
linkCap(n5,n3,bandwidth,200).
linkCap(n5,n4,bandwidth,200).
linkCap(n5,n6,bandwidth,200).
linkCap(n6,n2,bandwidth,200).
linkCap(n6,n3,bandwidth,200).
linkCap(n6,n4,bandwidth,200).
linkCap(n6,n5,bandwidth,200).
linkCap(n1,n2,latency,130).
linkCap(n1,n3,latency,130).
linkCap(n1,n4,latency,130).
linkCap(n1,n5,latency,130).
linkCap(n1,n6,latency,130).
linkCap(n1,n7,latency,130).
linkCap(n2,n1,latency,130).
linkCap(n2,n7,latency,130).
linkCap(n3,n1,latency,130).
linkCap(n3,n7,latency,130).
linkCap(n4,n1,latency,130).
linkCap(n4,n7,latency,130).
linkCap(n5,n1,latency,130).
linkCap(n5,n7,latency,130).
linkCap(n6,n1,latency,130).
linkCap(n6,n7,latency,130).
linkCap(n7,n1,latency,130).
linkCap(n7,n2,latency,130).
linkCap(n7,n3,latency,130).
linkCap(n7,n4,latency,130).
linkCap(n7,n5,latency,130).
linkCap(n7,n6,latency,130).
linkCap(n1,n2,bandwidth,50).
linkCap(n1,n3,bandwidth,50).
linkCap(n1,n4,bandwidth,50).
linkCap(n1,n5,bandwidth,50).
linkCap(n1,n6,bandwidth,50).
linkCap(n1,n7,bandwidth,50).
linkCap(n2,n1,bandwidth,50).
linkCap(n2,n7,bandwidth,50).
linkCap(n3,n1,bandwidth,50).
linkCap(n3,n7,bandwidth,50).
linkCap(n4,n1,bandwidth,50).
linkCap(n4,n7,bandwidth,50).
linkCap(n5,n1,bandwidth,50).
linkCap(n5,n7,bandwidth,50).
linkCap(n6,n1,bandwidth,50).
linkCap(n6,n7,bandwidth,50).
linkCap(n7,n1,bandwidth,50).
linkCap(n7,n2,bandwidth,50).
linkCap(n7,n3,bandwidth,50).
linkCap(n7,n4,bandwidth,50).
linkCap(n7,n5,bandwidth,50).
linkCap(n7,n6,bandwidth,50).
linkCap(n1,c1,latency,100).
linkCap(n2,c2,latency,100).
linkCap(n3,c2,latency,100).
linkCap(n4,c2,latency,100).
linkCap(n5,c2,latency,100).
linkCap(n6,c2,latency,100).
linkCap(n7,c3,latency,100).
linkCap(c1,n1,latency,100).
linkCap(c2,n2,latency,100).
linkCap(c2,n3,latency,100).
linkCap(c2,n4,latency,100).
linkCap(c2,n5,latency,100).
linkCap(c2,n6,latency,100).
linkCap(c3,n7,latency,100).
linkCap(n1,c1,bandwidth,100).
linkCap(n2,c2,bandwidth,100).
linkCap(n3,c2,bandwidth,100).
linkCap(n4,c2,bandwidth,100).
linkCap(n5,c2,bandwidth,100).
linkCap(n6,c2,bandwidth,100).
linkCap(n7,c3,bandwidth,100).
linkCap(c1,n1,bandwidth,150).
linkCap(c2,n2,bandwidth,150).
linkCap(c2,n3,bandwidth,150).
linkCap(c2,n4,bandwidth,150).
linkCap(c2,n5,bandwidth,150).
linkCap(c2,n6,bandwidth,150).
linkCap(c3,n7,bandwidth,150).
linkCap(n1,c2,latency,150).
linkCap(n1,c3,latency,150).
linkCap(n2,c1,latency,150).
linkCap(n2,c3,latency,150).
linkCap(n3,c1,latency,150).
linkCap(n3,c3,latency,150).
linkCap(n4,c1,latency,150).
linkCap(n4,c3,latency,150).
linkCap(n5,c1,latency,150).
linkCap(n5,c3,latency,150).
linkCap(n6,c1,latency,150).
linkCap(n6,c3,latency,150).
linkCap(n7,c1,latency,150).
linkCap(n7,c2,latency,150).
linkCap(c1,n2,latency,150).
linkCap(c1,n3,latency,150).
linkCap(c1,n4,latency,150).
linkCap(c1,n5,latency,150).
linkCap(c1,n6,latency,150).
linkCap(c1,n7,latency,150).
linkCap(c2,n1,latency,150).
linkCap(c2,n7,latency,150).
linkCap(c3,n1,latency,150).
linkCap(c3,n2,latency,150).
linkCap(c3,n3,latency,150).
linkCap(c3,n4,latency,150).
linkCap(c3,n5,latency,150).
linkCap(c3,n6,latency,150).
linkCap(n1,c2,bandwidth,50).
linkCap(n1,c3,bandwidth,50).
linkCap(n2,c1,bandwidth,50).
linkCap(n2,c3,bandwidth,50).
linkCap(n3,c1,bandwidth,50).
linkCap(n3,c3,bandwidth,50).
linkCap(n4,c1,bandwidth,50).
linkCap(n4,c3,bandwidth,50).
linkCap(n5,c1,bandwidth,50).
linkCap(n5,c3,bandwidth,50).
linkCap(n6,c1,bandwidth,50).
linkCap(n6,c3,bandwidth,50).
linkCap(n7,c1,bandwidth,50).
linkCap(n7,c2,bandwidth,50).
linkCap(c1,n2,bandwidth,100).
linkCap(c1,n3,bandwidth,100).
linkCap(c1,n4,bandwidth,100).
linkCap(c1,n5,bandwidth,100).
linkCap(c1,n6,bandwidth,100).
linkCap(c1,n7,bandwidth,100).
linkCap(c2,n1,bandwidth,100).
linkCap(c2,n7,bandwidth,100).
linkCap(c3,n1,bandwidth,100).
linkCap(c3,n2,bandwidth,100).
linkCap(c3,n3,bandwidth,100).
linkCap(c3,n4,bandwidth,100).
linkCap(c3,n5,bandwidth,100).
linkCap(c3,n6,bandwidth,100).
linkCap(c1,c2,latency,100).
linkCap(c1,c3,latency,100).
linkCap(c2,c1,latency,100).
linkCap(c2,c3,latency,100).
linkCap(c3,c1,latency,100).
linkCap(c3,c2,latency,100).
linkCap(c1,c2,bandwidth,500).
linkCap(c1,c3,bandwidth,500).
linkCap(c2,c1,bandwidth,500).
linkCap(c2,c3,bandwidth,500).
linkCap(c3,c1,bandwidth,500).
linkCap(c3,c2,bandwidth,500).

profit(ap1,2.5).
profit(ap2,1).
profit(ap3,1.75).
profit(ap4,3).
profit(ap5,2.5).
profit(ap6,1.5).
profit(ap7,3).
profit(ap8,2.25).
profit(ap9,1.75).
profit(ap10,4).
profit(n1,4).
profit(n2,5).
profit(n3,3).
profit(n4,4).
profit(n5,2.5).
profit(n6,4).
profit(n7,4).
profit(c1,9).
profit(c2,7).
profit(c2,6).