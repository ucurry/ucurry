#! /bin/bash

anyfailed=0

for f in ./tests/tuple/*.uc
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
        anyfailed=1
        # exit $ecode
    fi
done


if [ $anyfailed -ne 1 ]
then
    echo "All tests passed"
fi
