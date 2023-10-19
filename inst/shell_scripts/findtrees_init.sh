#!/bin/bash

#SBATCH --job-name=findtrees
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3_bigmem
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=02:00:00

## Command(s) to run
module load r r-packages r-spatial
while true
do
R CMD BATCH --no-save \
../notebooks/LiDAR/05.11_detect_trees_full_watershed.R \
/global/scratch/users/worsham/logs/Rout_findtrees.Rout
done
