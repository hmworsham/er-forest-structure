#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

# Copy PNGs

scratch=/global/scratch/users/worsham/itc_results/

dest=er-drive:Data/LiDAR/itc_results/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &
