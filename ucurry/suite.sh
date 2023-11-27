#! /bin/sh

anyfailed=0
for f in tests/test_suite/positive_test/*.uc
do 
    echo "Testing $f"

    dune exec ucurry -- -s < $f | lli > $f.out
    diff $f.out $f.gold
    
    ecode=$?

    if [ $ecode -ne 0 ]
    then 
        echo "Test $f failed"
        anyfailed+=1
    fi 
done 

if [ $anyfailed -ne 1 ]
then 
    echo "All tests passed"
fi
