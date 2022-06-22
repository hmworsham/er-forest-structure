#!/bin/bash

#SBATCH --job-name=resample
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3_bigmem
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=04:00:00

## Command(s) to run
module load r r-packages r-spatial
while true
do
R CMD BATCH --no-save \
~/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/02_regrid_LAScatalog.R \
/global/scratch/users/worsham/logs/Rout_resampleLAS.Rout
done
