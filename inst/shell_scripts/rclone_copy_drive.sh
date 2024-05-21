#!/bin/bash
# Rclone data from watched remote drive to Savio Scratch

src=worsham:/LiDAR/las_downsampled
scratch=/global/scratch/users/worsham/las_downsampled
priority=16

nohup nice -n $priority rclone copy --bwlimit 8.15M $src $scratch &
