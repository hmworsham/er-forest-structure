#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/las_regridded/

dest=er-drive:/Data/LiDAR/las_regridded/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &
