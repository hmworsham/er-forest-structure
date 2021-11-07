# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

import importlib
import os
import functools
from waveform import chunk_waveforms as cw

importlib.reload(cw)

# Define the directory where large files are stored
#indir = '/global/scratch/users/worsham/waveform_binary'
#outdir = '/global/scratch/users/worsham/waveform_binary_split'
indir = '/home/ubuntu/waveform_binary/'
outdir = '/home/ubuntu/waveform_binary_chunked'

# # Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Process and upload to GCS
for fp in fps[:3]:
    cw.envi_chunk(fp, indir)
    