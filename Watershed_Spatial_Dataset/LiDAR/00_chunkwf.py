# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

import dask.multiprocessing
import importlib
import os
import functools

from waveform import chunk_waveforms as cw
from dask.distributed import Client

importlib.reload(cw)

# Connect to dask
c = Client(address=os.getenv('SCHED') + ':8786')
dask.config.set(scheduler='processes', num_workers=20)

# Define the directory where large files are stored
indir = '/global/scratch/users/worsham/waveform_binary'
outdir = '/global/scratch/users/worsham/waveform_binary_split'

# # Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Process and upload to GCS
futures = []
for fp in fps[34:37]:
    futures.append(dask.delayed(cw.envi_chunk)(fp, indir))

dask.compute(futures)