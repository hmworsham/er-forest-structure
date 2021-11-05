# Unzip LiDAR binary files
# Author: Marshall Worsham
# Revised: 21-03-29

# Script to unzip zipfiles
import os
import functools
from waveform import chunk_waveforms as cw
#rom ipyparallel import Client

# Define client
#c = Client()
#c.ids

# Define the directory where the large files are stored
indir = '/global/scratch/users/worsham/LiDAR'
files=os.listdir(indir)
paths=[os.path.join(indir, i) for i in files]
outdir = '/global/scratch/users/worsham/LiDAR/waveform_binary'

#dview = c[:]
#dview.block = True 
#dview.map_sync(functools.partial(cw.unzip_wf(outdir=outdir),paths) 

map(functools.partial(cw.unzip_wf(outdir=outdir),paths))

# # Define flightpaths to ingest
# fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# # Process and upload to GCS
# for fp in fps:
#     process_wfbinary(fp)

# # Process and copy to Brain10 storage volume    
# for fp in fps:
#     process_wfbinary_b10(fp)