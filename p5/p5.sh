#!/bin/bash

h=uqwqemis

for i in $(seq 1 10000); do
        h=$(echo $h | md5)
        if (echo $h | egrep -q '^00000'); then
                echo $h
        fi
done
