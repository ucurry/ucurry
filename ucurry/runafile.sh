#! /bin/bash
flag=$1
f=$2 

echo "Generating LLVM code to $f.ll"
dune exec ucurry -- $flag < $f > $f.ll 

echo "Generating the executable to $f.exe"
llc -relocation-model=pic $f.ll > $f.s
cc -o $f.exe $f.s
./$f.exe