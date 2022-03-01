#!/bin/bash
#SBATCH --job-name=bigdcdc
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3_bigmem
#SBATCH --nodes=3
#SBATCH --ntasks-per-node=32
#SBATCH --time=06:00:00

## Command(s) to run
module load r r-packages r-spatial
R CMD BATCH --no-save \
~/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/01_process_all_waveforms_batch.R \
run-reports/batchprocesswf.Rout
