#!/bin/sh
# rename path results to be just iter1.RData, iter2.RData etc once after putting them in the right folder

# arguments: name of folder containing the set of 100 path runs

mkdir $1

mv $1.tgz $1

cd $1

tar xzvf *.tgz

files=(*.RData) # so that can more easily index out elements in the folder

for i in {0..19..1} # for each file (there are 20) in the folder
do
	mv ${files[i]} iter${i}.RData
done

<<<<<<< HEAD

#rm *.tgz   # learnt from mistakes: don't do this until you've checked that the untarred files are ok

#counter=0
#for file in *.RData; do
#counter=counter+1
#    mv "$file" "iter${counter}.RData"
#done
=======
mv iter0.RData iter20.RData # bc bash indexes from 0 but I prefer indexing from 1

>>>>>>> master




