#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/hyperpointcloud/

dest=eastriver:/Data/LiDAR/hyperpointcloud/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.1M $scratch $dest &
