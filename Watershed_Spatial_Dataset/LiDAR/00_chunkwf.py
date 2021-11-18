# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

#import importlib
import sys
sys.path.append('/global/home/users/worsham/eastriver/Watershed_Spatial_Dataset/LiDAR/')
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

# Define directories
indir = '/global/scratch/users/worsham/waveformbinary'
outdir = '/global/scratch/users/worsham/waveformbinarychunk'

def wrapper(i):
    import sys
    sys.path.append('/global/home/users/worsham/eastriver/Watershed_Spatial_Dataset/LiDAR/')
    from waveform import chunk_waveforms as cw 
    idir = '/global/scratch/users/worsham/waveformbinary'
    odir = '/global/scratch/users/worsham/waveformbinarychunks'
    return(cw.process_wfbinary_loc(i, idir, odir))

# Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Split and save to scratch
lview.map(wrapper, fps[:4])
