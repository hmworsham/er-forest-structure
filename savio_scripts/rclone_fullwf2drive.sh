#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/waveformbinarychunks/

dest=er-drive:/EastRiver/waveformlidarchunks/

priority=16

nohup nice -n $priority rclone copy --bwlimit 8.2M $scratch $dest &
