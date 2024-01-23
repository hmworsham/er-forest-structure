#!/bin/bash

#SBATCH --job-name=bprocesswf
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3_bigmem
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=08:00:00

## Command(s) to run
module load r r-packages r-spatial
while true
do
R CMD BATCH --no-save \
../notebooks/LiDAR/03_hyperpointcloud.R \
/global/scratch/users/worsham/logs/Rout_hpc.Rout
done
