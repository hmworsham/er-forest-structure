#!/bin/bash
#SBATCH --job-name=bprocesswf
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=12:00:00

## Command(s) to run

module load r r-packages r-spatial

R CMD BATCH --no-save \
~/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/01_process_all_waveforms_03.R \
/global/scratch/users/worsham/logs/batchprocesswf_03.Rout
