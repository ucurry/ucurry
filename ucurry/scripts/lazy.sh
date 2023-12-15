#!/bin/bash

failed=0
counter=0

echo "Testing for lazy cases"
cd ..
for f in ./tests/lazy/*.uc
do 
    echo "Testing $f"

    dune exec ucurry -- -s < $f | lli > $f.out
    diff $f.out $f.gold
    
    ecode=$?
    ((counter++))

    if [ $ecode -ne 0 ]
    then 
        echo "Test $f failed"
        ((failed++))
    fi 
done

if [ $failed -eq 0 ]
then 
    echo "All $counter positive tests passed"
else 
    echo "$failed out of $counter tests failed"
fi
