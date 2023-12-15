#! /bin/sh
f=$1 

echo "Generating LLVM code to $f.ll"
dune exec ucurry -- -s < $f > $f.ll 

echo "Generating the executable to $f.exe"
llc -relocation-model=pic $f.ll > $f.s
./$f.exe