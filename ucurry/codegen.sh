dune exec ucurry -- -s < tests/codegen/print-hello.uc;
echo "--------------- Executing hello world program ----------------"
dune exec ucurry -- -s < tests/codegen/print-hello.uc | lli;
