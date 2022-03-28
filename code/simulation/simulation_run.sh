#!/bin/bash
#PBS -l walltime=00:04:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
cd $HOME
echo "R is about to run"
which R
Rscript --vanilla $HOME/CamtrapSimulation/pipeline_cluster.R
mv $HOME/CamtrapSimulation/*.RData $HOME/CamtrapSimulation
echo "R has finished running"
# end of script
