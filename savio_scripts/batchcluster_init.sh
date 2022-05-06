#!/bin/bash

#SBATCH --job-name=bprocesswf
#SBATCH --account=fc_lmklab
#SBATCH --partition=savio3_bigmem
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=10:00:00

## Command(s) to run
module load r r-packages r-spatial
while true
do
R CMD BATCH --no-save \
~/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/02_points_to_las.R \
/global/scratch/users/worsham/logs/Rout_pts2las.Rout
done
