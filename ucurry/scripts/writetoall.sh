src=$1
target="all.uc"
cd $src;
rm $target;
for f in ./*.uc
do 
	echo "--$f" >> $target
	cat $f >> $target
	echo -e "\n" >> $target
done
