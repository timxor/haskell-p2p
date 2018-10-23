#!/bin/bash

clear

cd bin

for i in $(seq -f "%05g" 1 64); do
    echo "Spawned client ID:" $i
    ./client > "../log/$i.txt" &
done
