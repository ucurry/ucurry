#! /bin/sh

for f in tests/scanparse/*.uc
do
    echo "Testing $f"
    dune exec ucurry -- -a < $f > $f.out
    # diff $f.out $f.expected
    
    # Get the PID of the last background command
    ecode=$?
    
    if [ $ecode -ne 0 ]
    then
        echo "Test $f failed with exit code $ecode\n"
        # exit $ecode
    fi
done
