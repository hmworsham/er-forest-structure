#!/bin/bash
# Rclone data from watched remote drive to Savio Scratch

src=worsham:/LiDAR/hyperpointcloud
scratch=/global/scratch/users/worsham/hyperpointcloud
priority=16

nohup nice -n $priority rclone copy --bwlimit 8.15M $src $scratch &
