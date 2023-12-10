#! /bin/bash
cd ..
for f in ./tests/lazy/*.uc 
do 
    echo "Testing .$f"
    dune exec ucurry -- -s < $f | lli
done
