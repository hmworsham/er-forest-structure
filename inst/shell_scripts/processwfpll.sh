#!/bin/bash
#SBATCH --job-name=dcdc1
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3_bigmem
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=08:00:00

## Command(s) to run (example):
module load r r-packages r-spatial
R CMD BATCH --no-save \
../notebooks/LiDAR/01_process_all_waveforms.R \
run-reports/processwf.Rout
