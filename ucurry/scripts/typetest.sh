#! /bin/sh
echo "Tests for success cases \n"
for f in ../tests/type/*.uc
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

