#!/bin/sh
# rename path results to be just iter1.RData, iter2.RData etc once they're in the right folder

# arguments: name of folder containing the set of 100 path runs

cd ../results

mkdir $1

mv $1.tgz $1

cd $1

tar xzvf *.tgz

for i in {1..50..1}
do
	mv *iter${i}.RData iter${i}.RData
done

<<<<<<< HEAD
#rm *.tgz   # learnt from mistakes: don't do this until you've checked that the untarred files are ok
=======
#rm *.tgz # learnt from mistakes - don't do this until you've checked the .RData files
>>>>>>> master

#counter=0
#for file in *.RData; do
#counter=counter+1
#    mv "$file" "iter${counter}.RData"
#done




