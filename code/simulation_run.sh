#!/bin/bash
#PBS -l walltime=70:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
cd $HOME
echo "R is about to run"
which R
Rscript --vanilla $HOME/CamtrapSimulation/pipeline_2CTs_1.R
echo "R has finished running"
# end of script
