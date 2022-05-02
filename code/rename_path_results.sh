#!/bin/sh
# rename path results to be just iter1.RData, iter2.RData etc once they're in the right folder

# arguments: name of folder containing the set of 100 path runs

cd path_results

mkdir $1

mv $1.tgz $1

cd $1

tar xzvf *.tgz

for i in {1..100..1}
do
	mv *iter${i}.RData iter${i}.RData
done

rm *.tgz

#counter=0
#for file in *.RData; do
#counter=counter+1
#    mv "$file" "iter${counter}.RData"
#done




