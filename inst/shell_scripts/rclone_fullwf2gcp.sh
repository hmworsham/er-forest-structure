#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/las_normalized.tar.gz

dest=eastriver-gcp:/eastriver

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.2M --progress $scratch $dest &
