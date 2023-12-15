#!/bin/bash

failed=0
counter=0

echo "Testing for Demo"
for f in ./*.uc;
do
    if [ $f == "./trie.uc" ]
    then
        dune exec ucurry -- -b < $f | lli
        continue
    fi
    echo "Testing $f"

    dune exec ucurry -- -s < $f | lli
done

