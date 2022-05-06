#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/geolocated_returns/

dest=er-drive:/EastRiver/geolocated_returns/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.2M $scratch $dest &
