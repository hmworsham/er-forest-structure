#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/logs/

dest=er-drive:Data/LiDAR/logs/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &
