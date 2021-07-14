# Topographic Position Index Algorithm

# Import file
import gdal as gdal
import numpy as np
import os as os

# Set import and export paths
os.getcwd()
wd = os.getcwd() + '/Desktop/RMBL/Projects/Watershed_Spatial_Dataset'
dem10 = wd + '/Output/SDP_DEM_Resampled/sdp_dem10.tif'
dem100 = wd + '/Output/SDP_DEM_Resampled/sdp_dem100.tif'
tpi1k = wd + '/Output/TPI/TPI1000.tif'

# Create the moving window
r = 5
window = np.ones((2 * r + 1, 2 * r + 1))

# Determine window radius
r_y, r_x = window.shape[0]//2, window.shape[1]//2 
window[r_y, r_x] = 0  # let's remove the central cell

# Define function to return two matching numpy views for a moving window routine
def view(offset_y, offset_x, shape, step=1):
    size_x, size_y = shape
    x, y = abs(offset_x), abs(offset_y)
    x_in = slice(x, size_x, step)
    x_out = slice(0, size_x - x, step)
    y_in = slice(y, size_y, step)
    y_out = slice(0, size_y - y, step)

    # swap in and out values
    if offset_x < 0:
        x_in, x_out = x_out, x_in
    if offset_y < 0:
        y_in, y_out = y_out, y_in

    # return window view (in) and main view (out)
    return np.s_[y_in, x_in], np.s_[y_out, x_out] 

view(500, 500, 220,)


# Calculate TPI
dem = gdal.Open(dem100)
zmatx = dem.ReadAsArray()

#matrices for temporary data
matx_temp = np.zeros(zmatx.shape)
matx_count = np.zeros(zmatx.shape)

# loop through window and accumulate values
for (y, x), weight in np.ndenumerate(window):

    if weight == 0:
        continue  # skip zero values !
    # determine views to extract data
    view_in, view_out = view(y - r_y, x - r_x, zmatx.shape)
    # using window weights (eg. for a Gaussian function)
    matx_temp[view_out] += zmatx[view_in] * weight

   # track the number of neighbours
   # (this is used for weighted mean : Σ weights*val / Σ weights)
    matx_count[view_out] += weight

# this is TPI (spot height – average neighbourhood height)
out = zmatx - matx_temp / matx_count

# writing output
driver = gdal.GetDriverByName('GTiff')
ds = driver.Create(
    tpi1k, zmatx.shape[1], zmatx.shape[0], 1, gdal.GDT_Float32)
ds.SetProjection(dem100.GetProjection())
ds.SetGeoTransform(dem100.GetGeoTransform())
ds.GetRasterBand(1).WriteArray(out)
ds = None
