# Chunk NEON waveform LiDAR binary files
# Author: Marshall Worsham
# Revised: 21-03-29

# This script contains functions to ingest large (20+GB) files from NEON waveform LiDAR acquisition and split them into more manageable chunks of ~1GB in size. The script defines several functions, which can be combined into a single workflow. The script allows for writing either to a local storage device or to a Google Cloud Storage bucket.

# Import libraries
import importlib
import os
import numpy as np
import shutil
import spectral.io.envi as envi
import waveform

from waveform import chunk_waveforms as cw
from google.cloud import storage
from os.path import isfile, join
from spectral import *

# Quick reload modified modules
importlib.reload(cw)

# Define the directory where the large files are stored
indir = ('/home/ubuntu/neon')

# os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = '/Users/hmworsham/.ssh/eastriver-308601-65de0446573f.json'

testzip=os.listdir(indir)[0]
testzip=os.path.join(indir,testzip)
xx = cw.unzip_wf(testzip, '/tmp')


# Define flightpaths to ingest
fps = [d for d in os.listdir(indir) if os.path.isdir(os.path.join(indir,d))] # Lists all flightpaths

# Process and upload to GCS
for fp in fps:
    process_wfbinary(fp)

# Process and copy to Brain10 storage volume    
for fp in fps:
    process_wfbinary_b10(fp)