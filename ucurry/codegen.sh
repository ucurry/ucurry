#! /bin/sh 
make clean
echo "Running codegen tests"

anyfailed=0
for f in tests/codegen/*.uc 
do 
    echo "Testing $f"
    dune exec ucurry -- -s < $f > $f.ll 
    lli $f.ll > $f.out
    diff $f.out $f.gold 

    ecode=$? 

    if [ $ecode -ne 0 ] 
    then 
        echo "Test $f failed"
        anyfailed=1
    fi 
done 

if [ $anyfailed -ne 1 ]
then
    echo "All tests passed"
fi