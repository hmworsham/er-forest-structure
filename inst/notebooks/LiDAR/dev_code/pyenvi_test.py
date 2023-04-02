# Testing ENVI binary data manipulation

# author = ['Marshall Worsham']
# data = 11-02-2020

# installs
!pip3 install spectral
!pip install liblas

# libraries
from spectral import *
import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import laspy
#from liblas import file


# local path to sample binary files
neon_binary_path = '/Users/hmworsham/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Source/NEON_Binary_2018/2018_CRBU_1_2018061214_FL001/'

# open geolocation array
geolocation_path = os.sep.join([neon_binary_path, '2018_CRBU_1_2018061214_FL001_waveform_geolocation_array_img.hdr'])
geo_pulse = open_image(geolocation_path)
geo_open = geo_pulse.open_memmap(writeable=True)
geo_open

## check shape of geolocation array
geo_open.shape

# open return array
return_path =os.sep.join([neon_binary_path, '2018_CRBU_1_2018061214_FL001_waveform_return_pulse_array_img.hdr'])
return_pulse = open_image(return_path)
return_open = return_pulse.open_memmap(writeable=True)

## check shape of return array
len(return_open)
return_open.shape

# plot return array
plt.plot(return_open[0], np.arange(500), label = 'Return')


# open outgoing pulse array
out_path = os.sep.join([neon_binary_path, '2018_CRBU_1_2018061214_FL001_waveform_outgoing_pulse_array_img.hdr'])
out_pulse = open_image(out_path)
out_open = out_pulse.open_memmap(writeable=True)
out_open
out_open.shape

plt.plot(out_open[0], np.arange(100), label = 'Outgoing')
plt.legend()

# open observation array
obs_path = os.sep.join([neon_binary_path, '2018_CRBU_1_2018061214_FL001_waveform_observation_array_img.hdr'])
obs_array = open_image(obs_path)
obs_open = obs_array.open_memmap(writeable = True)
obs_open[0]

# column titles



################
# LAS Processing
################

# view first-return las
fig = plt.figure()
las_file = os.sep.join([neon_binary_path, '2018_CRBU_1_2018061214_FL001_waveform_first_return.las'])
input_las = laspy.file.File(las_file, mode="r")
point_records = input_las.points.copy()

# getting scaling and offset parameters
las_scaleX = input_las.header.scale[0]
las_offsetX = input_las.header.offset[0]
las_scaleY = input_las.header.scale[1]
las_offsetY = input_las.header.offset[1]
las_scaleZ = input_las.header.scale[2]
las_offsetZ = input_las.header.offset[2]

# calculating coordinates
p_X = np.array((point_records['point']['X'] * las_scaleX) + las_offsetX)
p_Y = np.array((point_records['point']['Y'] * las_scaleY) + las_offsetY)
p_Z = np.array((point_records['point']['Z'] * las_scaleZ) + las_offsetZ)

print(p_X[:100][0], p_Y[:100][0], p_Z[:100][0])

# plotting points in 3D
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
#ax.scatter(p_X, p_Y, p_Z, cmap = 'viridis', c='red', marker = 'o')
ax.scatter(p_X[:28500], p_Y[:28500], p_Z[:28500], c = p_Z[:28500], cmap = 'plasma', marker='o', alpha = 0.7)
plt.show()

# plotting points in 2D
fig = plt.figure(figsize=[30,30])
ax = fig.add_subplot(111)
ax.scatter(p_X, p_Y, c = p_Z, cmap = 'plasma', marker = 'o', alpha = 0.7)
plt.show()


# save an image
envi.save_image('new_image.bsq', ext='bsq', interleave='BSQ')

import numpy
from numba import jit
from pylidar import lidarprocessor
from pylidar.toolbox import spatial
from pylidar.lidarformats import generic

def readFunc(data):
    # returns 2d masked structured array with info about waveforms
    # first axis is waveform number, second is pulse
    waveinfo = data.input1.getWaveformInfo()

    # returns masked 3d radiance array
    # first axis is waveform bin, second is waveform number, third is pulse
    recv = data.input1.getReceived()
    trans = data.input1.getTransmitted()

dataFiles = lidarprocessor.DataFiles()
dataFiles.input1 = lidarprocessor.LidarFile(infile, lidarprocessor.READ)
lidarprocessor.doProcessing(readFunc, dataFiles)