# Unzip LiDAR binary files
# Author: Marshall Worsham
# Created: 03-29-21
# Revised: 03-29-21

# Script to unzip zipfiles
import os
import functools
from waveform import chunk_waveforms as cw

# Define the directory where the large files are stored
indir = '/global/scratch/users/worsham/LiDAR'
files=os.listdir(indir)
paths=[os.path.join(indir, i) for i in files]
outdir = '/global/scratch/users/worsham/waveform_binary'

for p in paths:
    cw.unzip_wf(p, outdir)
