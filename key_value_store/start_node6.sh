#!/bin/bash
app="kvs:4.0"

docker rm node6
docker build -t kvs:4.0 .

docker run -p 13805:13800                                                            \
             --net=kv_subnet --ip=10.10.0.7 --name="node6"                             \
             -e ADDRESS="10.10.0.7:13800"                                              \
             -e VIEW="10.10.0.2:13800,10.10.0.3:13800,10.10.0.4:13800,10.10.0.5:13800,10.10.0.6:13800,10.10.0.7:13800" \
             -e REPL_FACTOR=2                                                          \
             kvs:4.0