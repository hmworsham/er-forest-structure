# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

import os
import functools
from waveform import chunk_waveforms as cw

# Define the directory where large files are stored
indir = '/global/scratch/users/worsham/waveform_binary'
outdir = '/global/scratch/users/worsham/waveform_binary_split'

# # Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Process and upload to GCS
for fp in fps:
    waveform.process_wfbinary(fp)

# Process and copy to Brain10 storage volume    
for fp in fps:
    waveform.process_wfbinary_b10(fp)
