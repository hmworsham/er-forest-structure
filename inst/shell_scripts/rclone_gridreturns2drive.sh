#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/gridded_returns/

dest=er-drive:/EastRiver/gridded_returns/

priority=12

nohup nice -n $priority rclone copy --bwlimit 8.2M $scratch $dest &
