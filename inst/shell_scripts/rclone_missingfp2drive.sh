#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/missing_flightpath/

dest=er-drive:Data/LiDAR/missing_flightpath/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &
