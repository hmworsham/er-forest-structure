# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

import os
import sys
from ipyparallel import Client
from waveform import chunk_waveforms as cw

# Connect to ipyparallel
c = Client()
c.ids

lview = c.load_balanced_view()
# cause execution on main process to wait while tasks sent to workers finish
lview.block = True

# Define wrapper function for parallel chunking
def wrapper(i):
    import sys
    sys.path.append(
        '/global/home/users/worsham/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/')
    from waveform import chunk_waveforms as cw
    idir = '/global/scratch/users/worsham/waveform_binary'
    odir = '/global/scratch/users/worsham/waveform_binary_chunks'
    return(cw.chunk_wfbinary_loc(i, idir, odir))


# Name flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(
    os.path.join(indir, d))]  # Lists all flightpaths

##############
# Parallel
##############

# Split and save to scratch
lview.map(wrapper, fps)
