#! /bin/sh
echo "Tests for success cases \n"
for f in tests/scanparse/*test-*.uc
do
    echo "Testing $f"
    dune exec ucurry -- -a < $f > $f.trimmed
    dune exec ucurry -- -a < $f.trimmed > $f.out
    diff $f.trimmed $f.out
    
    # Get the PID of the last background command
    ecode=$?
    
    if [ $ecode -ne 0 ]
    then
        echo "Test $f failed with exit code $ecode\n"
        # exit $ecode
    fi
done

echo "Tests for fail cases \n"
for f in tests/scanparse/*fail-*.uc
do
    echo "Testing $f which is supposed to fail"
    dune exec ucurry -- -a < $f 2> /dev/null 1> /dev/null
    ecode=$?
    
    if [ $ecode -eq 0 ]
    then 
        echo "Test $f succeeded unexpectedly\n"
    else 
        echo "Test $f failed as expected\n"
    fi

done
