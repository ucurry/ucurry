for f in tests/codegen/*.uc
do 
	cat $f >> tests/codegen/all.uc
	echo -e "\n" >> tests/codegen/all.uc
done
