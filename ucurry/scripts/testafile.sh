f=$1 

gcc -c nomatch.c

echo "Generating LLVM code to $f.ll"
dune exec ucurry -- -s < $f > $f.ll 

echo "Generating the executable to $f.exe"
llc -relocation-model=pic $f.ll > $f.s
cc -o $f.exe $f.s nomatch.o
./$f.exe