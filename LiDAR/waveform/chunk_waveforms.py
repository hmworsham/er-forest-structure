# Chunk NEON waveform LiDAR binary files
# Author: Marshall Worsham
# Revised: 21-03-29

# This script contains functions to ingest large (20+GB) files from NEON waveform LiDAR acquisition and split them into more manageable chunks of ~1GB in size. The script defines several functions, which can be combined into a single workflow. The functions allow for writing either to a local storage device or to a Google Cloud Storage bucket.

# Import libraries
import os
import numpy as np
import py7zr
import shutil
import spectral.io.envi as envi

from google.cloud import storage
from os.path import isfile, join
from spectral import *

# Function to chunk a large set of waveform files and write to temporary subdirectories in sequence


def unzip_wf(inzip, outdir):
    '''
    Ingests a zipfile containing ENVI binary files and unzips into constituents

    Args: zipfile - a path to a given zipfile
    Returns: constituent files in outdir
    '''

    with py7zr.SevenZipFile(inzip, 'r') as z:
        z.extractall(path=outdir)

# Function to chunk a large set of waveform files and write to temporary subdirectories in sequence


def envi_chunk(fp, indir, outdir):
    ''' Ingests a set of ENVI binary files (bil) and splits them into 1M-line chunks, then writes chunks out to local directory. Also copies one-line impulse response and impulse response at T0 files to the local directory. Each directory corresponds to a flightpath named by flight date and number in series flown that day. The set of binary files and their naming conventions should be consistent across directories. 

    Args:
        fp: string indicating unique flightpath name from source

    '''

    chunk_strings = ['return_pulse', 'geolocation',
                     'outgoing_pulse', 'observation', 'ephemeris']
    full_list = os.listdir(os.path.join(indir, fp))
    chunk_files = [
        nm for ps in chunk_strings for nm in full_list if ps in nm and 'hdr' not in nm]

    for f in chunk_files:
        img = envi.open(os.path.join(indir, fp, f + '.hdr'))

        # pull metadata
        nobs = np.int(img.nrows)
        dt = img.dtype
        if img.interleave == 1: il = 'bil'
        bo = img.byte_order
        ss = img.sample_size

        beg = np.int(0)
        sub = np.int(1e5)
        n = np.int(1)
        print(f + ' loaded')

        while sub < nobs:
            subset = img[beg:sub, :]
            #print(f'{n}: {len(subset)} lines')
            beg = sub
            sub = np.int(sub + 1e5)

            subsetname = '-' + str.zfill(f'{n}', 3)
            fpsub = f.split('_waveform')[0] + \
                subsetname + '_' + f.split('_', 5)[-1]
            #print('fpsubsetname:', fpsub)

            newdir = os.path.join(outdir, fp + subsetname)
            #print('newdirname:', newdir)
            if not os.path.isdir(newdir):
                os.mkdir(newdir)
            else:
                print(fp + subsetname + ' exists')

            outpath = os.path.join(newdir, fpsub + '.hdr')
            # print(outpath)

            n = n+1

            envi.save_image(outpath, subset, force=True, dtype=dt,
                            ext='', sample_size=ss, interleave=il, 
byte_order=bo)

        else:
            subset = img[beg:nobs, :]
            # print(len(subset))

            subsetname = '-' + str.zfill(f'{n}', 3)
            fpsub = f.split('_waveform')[0] + \
                subsetname + '_' + f.split('_', 5)[-1]
            #print('fpsubsetname:', fpsub)

            newdir = os.path.join(outdir, fp + subsetname)
            if not os.path.isdir(newdir):
                os.mkdir(newdir)
            else:
                print(fp + subsetname + ' exists')

            outpath = os.path.join(newdir, fpsub + '.hdr')
            #print('outpath:', outpath)

            n = n+1

            envi.save_image(outpath, subset, dtype=dt, force=True,
                            ext='', interleave=il, byte_order=bo)

# Function to copy impulse response files from original directory to new directory


def cp_files(fp, indir, outdir):
    alldirs = os.listdir(outdir)
    copy_dirs = [d for d in alldirs if fp in d and '-' in d]

    full_list = os.listdir(os.path.join(indir, fp))
    copy_strings = ['impulse']
    copy_files = [nm for ps in copy_strings for nm in full_list if ps in nm]

    for d in copy_dirs:
        for f in copy_files:
            f2copy = os.path.join(indir, fp, f)
            copypath = os.path.join(outdir, d, f)
            shutil.copyfile(f2copy, copypath)

# Function to write to GCS


def upload_dir(bucket_name, fp):
    '''Uploads files to GCS bucket.

    Retry params override default retry params for this file handle.

    Args:
        bucket_name: destination bucket name
        fp: flightpath
    '''

    # Set some parameters for uploading to Google Cloud Storage to avoid timeout errors
    storage.blob._DEFAULT_CHUNKSIZE = 5 * 1024 * 1024  # 5 MB
    storage.blob._MAX_MULTIPART_SIZE = 5 * 1024 * 1024  # 5 MB

    # Instantiate a client
    storage_client = storage.Client()  # Uses credentials referred in os.environ

    # Call bucket
    bucket = storage_client.get_bucket(bucket_name)

    # Write files to specified directory in bucket
    alldirs = os.listdir(indir)
    dirs2ul = [os.path.join(indir, d) for d in alldirs if fp in d and '-' in d]

    for d in dirs2ul:
        files = [f for f in os.listdir(d) if isfile(os.path.join(d, f))]
        for file in files:
            local_file = os.path.join(d, file)
            blob = bucket.blob(os.path.join(d.split('/')[-1], file))
            if not blob.exists():
                blob.upload_from_filename(
                    local_file, num_retries=4, timeout=60)

# Function to remove temporary directories after uploading


def rm_dirs(fp, indir):
    alldirs = os.listdir(indir)
    dirs2rm = [os.path.join(indir, d) for d in alldirs if fp in d]
    # print(dirs2rm)
    for d in dirs2rm:
        shutil.rmtree(d)

# Function to copy to local storage and remove temporary directories after uploading


def cp_dir(fp, indir, destdir):
    alldirs = os.listdir(indir)
    dirs2cp = [os.path.join(indir, d) for d in alldirs if fp in d and '-' in d]

    for dc in dirs2cp:
        dest = os.path.join(destdir, dc.split('/')[8])
        shutil.copytree(dc, dest)
        shutil.rmtree(dc)

# Function to process all waveforms and copy to directory


def chunk_wfbinary_loc(fp, indir, destdir):

    # Ingest files from one flightpath directory, chunk them, and write chunks to destination subdirs
    envi_chunk(fp, indir, destdir)
    # Copy impulse response files to the new directory
    cp_files(fp, indir, destdir)
    print('filepath {} processed'.format(fp))

    return errors

# Function to process all waveforms and upload to Google Cloud Storage


def process_wfbinary_gc(fp, indir):
    # Ingest files from one flightpath directory, chunk them, and write chunks to temporary subdirs
    envi_chunk(fp, indir)
    cp_files(fp)  # Copy impulse response files to the new directory
    # Write the directory and files to GCS
    upload_dir('neon_waveform_binary', fp)
    rm_dirs(fp)  # Delete temporary chunk directories from source
