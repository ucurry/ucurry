f=$1 

echo "Generating Ast $f.ast"
dune exec ucurry -- -a < $f > $f.ast 

echo "Generating Last to $f.cast"
dune exec ucurry -- -l < $f > $f.last

echo "Generating renamed Cast to $f.cast"
dune exec ucurry -- -c < $f > $f.cast 

echo "Generating LLVM code to $f.ll"
dune exec ucurry -- -s < $f > $f.ll 

echo "Generating the executable to $f.exe"
llc -relocation-model=pic $f.ll > $f.s
cc -o $f.exe $f.s 
