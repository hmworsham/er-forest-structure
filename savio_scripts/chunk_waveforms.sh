#!/bin/bash

# Command(s) to run:
ipcluster start -n 20 &
sleep 120
ipython ~/eastriver/Watershed_Spatial_Dataset/LiDAR/00_chunkwf.py > chunkwf.pyout
