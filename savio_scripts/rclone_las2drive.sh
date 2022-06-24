#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/las_ungridded/

dest=er-drive:/Data/LiDAR/las_ungridded/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.1M $scratch $dest &
