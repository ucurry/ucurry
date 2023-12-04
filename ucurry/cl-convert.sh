
#! /bin/sh 
make clean
echo "Running cl-convert tests"

anyfailed=0
for f in tests/cl-convert/*.uc 
do 
    echo "Testing $f"
    # dune exec ucurry -- -s < $f > $f.ll
    # llc -relocation-model=pic $f.ll > $f.s
    # cc -o $f.exe $f.s
    # ./$f.exe > $f.out
    dune exec ucurry -- -s < $f | lli > $f.out
    dune exec ucurry -- -s < $f | lli > $f.gold
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
