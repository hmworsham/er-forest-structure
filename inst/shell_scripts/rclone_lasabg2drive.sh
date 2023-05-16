#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/las_abg/

dest=er-drive:Data/LiDAR/las_abg/

priority=18

nohup nice -n $priority rclone copy --bwlimit 8.15M --log-file=/global/scratch/users/worsham/logs/rclone_las2drive_logfile.txt $scratch $dest &
