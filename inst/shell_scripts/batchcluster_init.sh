#!/bin/bash

#SBATCH --job-name=normalize
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
../notebooks/LiDAR/03.00_normalize_points.R \
/global/scratch/users/worsham/logs/Rout_normalize.Rout
done
