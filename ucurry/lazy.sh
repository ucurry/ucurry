#! /bin/bash

for f in ./tests/lazy/*.uc 
do 
    echo "Testing $f"
    dune exec ucurry -- -s < $f | lli

done
