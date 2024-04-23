#!/bin/bash

# Rclone data from Savio Scratch to watched Drive directory

src=worsham:/LiDAR/waveform_lidar_chunks_1e5/

scratch=/global/scratch/users/worsham/waveform_lidar_chunks_1e5/

dest=eastriver-gcp:/eastriver/waveform_lidar_chunks_1e5/

priority=15

nohup nice -n $priority rclone copy --bwlimit 8.2M --progress $src $scratch &

find $scratch -maxdepth 1 -mindepth 1 -type d -exec tar czvf {}.tar.gz {} --remove-files \;

nohup nice -n $priority rclone copy --bwlimit 8.2M --progress $scratch $dest &
