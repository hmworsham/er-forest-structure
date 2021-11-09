# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

#import importlib
import os
from waveform import chunk_waveforms as cw
from ipyparallel import Client

#importlib.reload(cw)

# Connect to dask
c = Client()
c.ids

lview = c.load_balanced_view()
lview.block = True # cause execution on main process to wait while tasks sent to workers finish 

#lview.execute('import os')
#lview.execute('from waveform import chunk_waveforms as cw')

def wrapper(i):
    return(cw.envi_chunk(i, indir))

# Define the directory where large files are stored
indir = '/global/scratch/users/worsham/waveform_binary'
outdir = '/global/scratch/users/worsham/waveform_binary_split'

# # Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Process and upload to GCS
# for fp in fps[34:37]:
#     futures.append(dask.delayed(cw.envi_chunk)(fp, indir))

lview.map(wrapper, fps[34:37])
