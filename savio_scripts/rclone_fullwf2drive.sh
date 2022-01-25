#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

scratch=/global/scratch/users/worsham/waveform_binary_chunks/

dest=er-drive:/EastRiver/waveformlidarchunks/

priority=19

nohup nice -n $priority rclone copy --bwlimit 8.2M $scratch $dest &
