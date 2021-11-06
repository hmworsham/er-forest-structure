# Unzip LiDAR binary files
# Author: Marshall Worsham
# Revised: 21-03-29

# Script to unzip zipfiles
import os
import functools
from waveform import chunk_waveforms as cw

# Define the directory where the large files are stored
indir = '/global/scratch/users/worsham/LiDAR'
files=os.listdir(indir)
paths=[os.path.join(indir, i) for i in files]
outdir = '/global/scratch/users/worsham/waveform_binary'

<<<<<<< HEAD
for p in paths:
=======
#dview = c[:]
#dview.block = True 
#dview.map_sync(functools.partial(cw.unzip_wf(outdir=outdir),paths) 

for p in paths[46:]:
>>>>>>> 306ba750a2d72ca1555530d5c00de9852130dc3f
    cw.unzip_wf(p, outdir)

# # Define flightpaths to ingest
# fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# # Process and upload to GCS
# for fp in fps:
#     process_wfbinary(fp)

# # Process and copy to Brain10 storage volume    
# for fp in fps:
#     process_wfbinary_b10(fp)
