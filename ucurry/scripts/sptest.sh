#! /bin/bash
echo "Testing for scanparse and preliminary semant check" 

echo "Tests for success cases \n"
failed=0
total=0
cd ..
for f in ./tests/scanparse/*test-*.uc
do
    echo "Testing $f"
    dune exec ucurry -- -a < $f > $f.trimmed
    dune exec ucurry -- -a < $f.trimmed > $f.out
    diff $f.trimmed $f.out
    
    # Get the PID of the last background command
    ecode=$?
    ((total++))
    
    if [ $ecode -ne 0 ]
    then
        echo "Test $f failed with exit code $ecode\n"
        ((failed++))
        # exit $ecode
    fi
done

if [ $failed -eq 0 ]
then
    echo "All $total tests passed"
else 
    echo "$failed out of $total tests failed"
fi

failed=0
total=0
echo "Tests for fail cases \n"
for f in ./tests/scanparse/*fail-*.uc
do
    echo "Testing $f"
    dune exec ucurry -- -a < $f 2> /dev/null 1> /dev/null
    ecode=$?
    ((total++))
    
    if [ $ecode -eq 0 ]
    then 
        echo "Test $f succeeded unexpectedly\n"
        ((failed++))
    fi
done

if [ $failed -eq 0 ]
then
    echo "All $total tests passed"
else 
    echo "$failed out of $total tests failed"
fi