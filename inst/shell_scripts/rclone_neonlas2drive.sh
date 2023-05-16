#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

# Raw NEON LAS for gaps

priority=15

scratch=/global/scratch/users/worsham/neon_las_gaps/

dest=er-drive:Data/LiDAR/neon_las_gaps/

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &

# Regridded NEON LAS (deprecated)

scratch=/global/scratch/users/worsham/neon_las_regridded/

dest=er-drive:Data/LiDAR/neon_las_regridded/

