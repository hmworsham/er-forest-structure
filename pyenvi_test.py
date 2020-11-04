# Testing ENVI binary data manipulation

# author = ['Marshall Worsham']
# data = 11-02-2020

# installs
!pip3 install spectral
!pip3 install liblas

# libraries
from spectral import *
import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import laspy
from liblas import file


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
plt.plot(return_open[0], np.arange(500))


# open outgoing pulse array
out_path = os.sep.join([neon_binary_path, '2018_CRBU_1_2018061214_FL001_waveform_outgoing_pulse_array_img.hdr'])
out_pulse = open_image(out_path)
out_open = out_pulse.open_memmap(writeable=True)
out_open
out_open.shape

plt.plot(out_open[0], np.arange(100))


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

p_X[:100]
p_Y[:100]
p_Z[:100]

# plotting points
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
#ax.scatter(p_X, p_Y, p_Z, cmap = 'viridis', c='red', marker = 'o')
ax.scatter(p_X[:28500], p_Y[:28500], p_Z[:28500], c = p_Z[:28500], cmap = 'plasma', marker='o')
plt.show()

# save an image
envi.save_image('new_image.bsq', ext='bsq', interleave='BSQ')
