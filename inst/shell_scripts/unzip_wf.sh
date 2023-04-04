#!/bin/bash
#Job name:
#SBATCH --job-name=unzip_wfs

#Account:
#SBATCH --account=fc_lmklab

#Partition:
#SBATCH --partition=savio3    

# Wall clock limit:
#SBATCH --time=02:00:00

# Nodes
#SBATCH --nodes=1

# Command(s) to run:
module load python/3.7
python ../notebooks/LiDAR/00_unzipwf.py > unzip.pyout
