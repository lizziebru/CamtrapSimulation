#!/bin/sh
# rename incorrectly named paths

# arguments:
# $1 = original (incorrect) speed parameter 
# $2 = new (correct) speed parameter

for i in {51..83..1}
do
        mv sp$1iter${i}.RData sp$2iter${i}.RData
done

