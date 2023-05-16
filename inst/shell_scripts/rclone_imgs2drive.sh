#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

# Copy PNGs

scratch=/global/scratch/users/worsham/pngs/

dest=er-drive:Data/LiDAR/pngs/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &


# Copy TIFs
scratch=/global/scratch/users/worsham/tifs/

dest=er-drive:Data/LiDAR/tifs/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &




