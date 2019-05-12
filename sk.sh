#!/bin/bash

for i in `seq -w 10`
do
    if [ $(diff <(cat "good$i.in" | ./main) good$i.ps) ]; then
        echo "OPST! Test $i not passed"
    else
        echo "Test $i passed"
    fi
done
