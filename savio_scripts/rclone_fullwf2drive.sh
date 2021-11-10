#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/waveform_binary/

dest=er-drive:/EastRiver/waveformlidar/

priority=16

nohup nice -n $priority rclone copy --bwlimit 8.2M $scratch $dest &
