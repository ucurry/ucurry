#! /bin/bash

failed=0
counter=0

echo "Testing for postivie cases"
cd ..;
for f in ./tests/test_suite/positive_test/*.uc
do 
    echo "Testing .$f with lazy evaluation"
    dune exec ucurry -- -s < $f | lli > $f.out
    diff $f.out $f.gold
    
    ecode=$?
    ((counter++))
    
    if [ $ecode -ne 0 ]
    then 
        echo "Test $f failed"
        ((failed++))
    fi 

    echo "Testing .$f without lazy evaluation"
    dune exec ucurry -- -b < $f | lli > $f.out
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


echo "Testing for negative cases"
failed=0
counter=0
for f in ./tests/test_suite/negative_test/*.uc
do 
    echo "Testing .$f"

    dune exec ucurry -- -s < $f 2> $f.out
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
