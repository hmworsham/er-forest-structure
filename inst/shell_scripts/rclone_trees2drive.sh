#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/trees_100K/

dest=er-drive:/EastRiver/trees_100K/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.2M $scratch $dest &
