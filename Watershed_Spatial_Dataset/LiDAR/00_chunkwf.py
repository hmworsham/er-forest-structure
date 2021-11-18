# Chunk large LiDAR binary files
# Author: Marshall Worsham
# Created 21-03-29
# Revised: 21-11-06

# Script to split large binary files into chunks of 1M lines each for parallel processing

#import importlib
import sys
sys.path.append('/global/home/users/worsham/eastriver/Watershed_Spatial_Dataset/LiDAR/')
import importlib
import os
from waveform import chunk_waveforms as cw
from ipyparallel import Client

# Connect to ipyparallel
c = Client()
c.ids

lview = c.load_balanced_view()
lview.block = True # cause execution on main process to wait while tasks sent to workers finish

# Define directories
indir = '/global/scratch/users/worsham/waveformbinary'
outdir = '/global/scratch/users/worsham/waveformbinarychunk'
# indir = '/Users/hmworsham/waveformbinary'
# outdir = '/Users/hmworsham/waveformbinarychunks'

# fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir, d))]

# importlib.reload(cw)
# tst = cw.envi_chunk(fps[0], indir, outdir)
# tst2 = cw.cp_files(fps[0], indir, outdir)
# tst3 = cw.chunk_wfbinary_loc(fps[0], indir, outdir)

def wrapper(i):
    import sys
    sys.path.append('/global/home/users/worsham/eastriver/Watershed_Spatial_Dataset/LiDAR/')
    from waveform import chunk_waveforms as cw 
    idir = '/global/scratch/users/worsham/waveformbinary'
    odir = '/global/scratch/users/worsham/waveformbinarychunks'
    return(cw.chunk_wfbinary_loc(i, idir, odir))

# Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Split and save to scratch
lview.map(wrapper, fps[8:10])
