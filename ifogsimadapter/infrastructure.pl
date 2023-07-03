node(motor-1).
nodeCap(motor-1, type, actuator).
nodeCap(motor-1, mips, 0).
nodeCap(motor-1, ram, 0).
nodeCap(motor-1, storage, 0).
nodeCap(motor-1, downBw, 1000).
nodeCap(motor-1, upBw, 1000).

node(router).
nodeCap(router, type, fog_device).
nodeCap(router, mips, 1000).
nodeCap(router, ram, 1024).
nodeCap(router, ratePerMips, 0.0).
nodeCap(router, upBw, 1000).
nodeCap(router, downBw, 1000).
nodeCap(router, level, 1).

node(temp-1).
nodeCap(temp-1, type, sensor).
nodeCap(temp-1, mips, 0).
nodeCap(temp-1, ram, 0).
nodeCap(temp-1, storage, 0).
nodeCap(temp-1, downBw, 1000).
nodeCap(temp-1, upBw, 1000).

node(temp-0).
nodeCap(temp-0, type, sensor).
nodeCap(temp-0, mips, 0).
nodeCap(temp-0, ram, 0).
nodeCap(temp-0, storage, 0).
nodeCap(temp-0, downBw, 1000).
nodeCap(temp-0, upBw, 1000).

node(mobile-0).
nodeCap(mobile-0, type, fog_device).
nodeCap(mobile-0, mips, 1000).
nodeCap(mobile-0, ram, 1024).
nodeCap(mobile-0, ratePerMips, 0.0).
nodeCap(mobile-0, upBw, 1000).
nodeCap(mobile-0, downBw, 1000).
nodeCap(mobile-0, level, 2).

node(mobile-1).
nodeCap(mobile-1, type, fog_device).
nodeCap(mobile-1, mips, 1000).
nodeCap(mobile-1, ram, 1024).
nodeCap(mobile-1, ratePerMips, 0.0).
nodeCap(mobile-1, upBw, 1000).
nodeCap(mobile-1, downBw, 1000).
nodeCap(mobile-1, level, 2).

node(motor-0).
nodeCap(motor-0, type, actuator).
nodeCap(motor-0, mips, 0).
nodeCap(motor-0, ram, 0).
nodeCap(motor-0, storage, 0).
nodeCap(motor-0, downBw, 1000).
nodeCap(motor-0, upBw, 1000).

node(cloud).
nodeCap(cloud, type, fog_device).
nodeCap(cloud, mips, 100000).
nodeCap(cloud, ram, 10240).
nodeCap(cloud, ratePerMips, 10.0).
nodeCap(cloud, upBw, 100).
nodeCap(cloud, downBw, 1000).
nodeCap(cloud, level, 0).

linkCap(motor-1, mobile-1, latency, 2.0).
linkCap(mobile-1, motor-1, latency, 2.0).
linkCap(router, cloud, latency, 50.0).
linkCap(cloud, router, latency, 50.0).
linkCap(temp-1, mobile-1, latency, 2.0).
linkCap(mobile-1, temp-1, latency, 2.0).
linkCap(temp-0, mobile-0, latency, 2.0).
linkCap(mobile-0, temp-0, latency, 2.0).
linkCap(mobile-0, router, latency, 10.0).
linkCap(router, mobile-0, latency, 10.0).
linkCap(mobile-1, router, latency, 10.0).
linkCap(router, mobile-1, latency, 10.0).
linkCap(motor-0, mobile-0, latency, 2.0).
linkCap(mobile-0, motor-0, latency, 2.0).
