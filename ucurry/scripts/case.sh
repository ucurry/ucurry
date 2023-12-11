#! /bin/bash
cd ..
for f in ./tests/case/*.uc 
do 
    echo "Testing .$f"
    dune exec ucurry -- -s < $f | lli > $f.out
    diff $f.out $f.gold
done
