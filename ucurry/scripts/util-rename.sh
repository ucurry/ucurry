src=$1
prefix=$2
cd $src;
for f in *
do 
    echo $f
    mv $f "$prefix$f"
done 
