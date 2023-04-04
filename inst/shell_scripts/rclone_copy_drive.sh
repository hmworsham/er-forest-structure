#!/bin/bash
# Rclone data from watched remote drive to Savio Scratch
src=er-lbl-drive:/LiDAR/NEON_2018_SDP/WaveformLidar/NeonBinary/
scratch=/global/scratch/users/worsham/LiDAR
priority=16

nohup nice -n $priority rclone copy --bwlimit 8.2M $src $scratch &

