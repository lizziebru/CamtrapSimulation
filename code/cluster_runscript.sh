#!/bin/bash
#PBS -lwalltime=70:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
cd $HOME/Mb_camtrapsimulation
echo "R is about to run"
which R
Rscript --vanilla $HOME/Mb_camtrapsimulation/pipeline_cluster_path.R
echo "R has finished running"
