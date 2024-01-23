#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/trees_ls_lmffill/

dest=er-drive:Data/LiDAR/trees_ls_lmffill/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.15M $scratch $dest &
