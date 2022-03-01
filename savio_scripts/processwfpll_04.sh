#!/bin/bash
#SBATCH --job-name=dcdc4
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio2_bigmem
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --time=08:00:00

## Command(s) to run (example):
module load r r-packages r-spatial
R CMD BATCH --no-save \
~/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/01_process_all_waveforms_04.R \
run-reports/processwf.Rout
