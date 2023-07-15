#!/bin/bash

#SBATCH --job-name=itc_traintest
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=06:00:00

## Command(s) to run
module load r r-packages r-spatial
while true
do
R CMD BATCH --no-save \
../notebooks/LiDAR/05.03_itc_ls.R \
/global/scratch/users/worsham/logs/Rout_itc_ls.Rout
done
