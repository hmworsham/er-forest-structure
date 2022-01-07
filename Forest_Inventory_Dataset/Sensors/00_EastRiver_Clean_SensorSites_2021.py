# Script to generate points, lines, and polygons demarcating Kueppers-Powell forest inventory plot boundaries

#%%
# Load libraries
import pandas as pd
import geopandas as gpd
import math
import numpy as np
import os
import shapely
import re

#%%
# Set arcpy workspace as output
indir = '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Inventory Plots/Inventory_Plots_GPS_Data/2021'

sensordirs = [
    'KATZJ060810B',
    'KATZJ062410A',
    'KATZJ062510A',
    'KATZJ071909A',
    'KATZJ072311B',
    'KATZJ072812A',
    'KATZ080512A',
    'KATZ080517A',
    'WORSHAMM081308A',
    'WORSHAMM081812B',
    'WORSHAMM082008A',]

pd.set_option('display.max_rows', 200)

#%%
sensorfiles = []
for root, dirs, files in os.walk(indir):
    for d in dirs:
        if d in sensordirs:
            sensfile = os.path.join(root, d, 'Student__Project.shp')
            sensorfiles.append(sensfile)
#%%
allshots=[]
for f in sensorfiles:
    shots = gpd.read_file(f)
    allshots.append(shots)

allshots_gdf = gpd.GeoDataFrame(pd.concat(allshots, ignore_index=True))

#%%
allshots_gdf['Other'] = allshots_gdf['Other'].str.replace('GTH1', 'GT1')
allshots_gdf['Other'] = allshots_gdf['Other'].str.replace('ER-PVG1', 'SR-PVG1')

allshots_gdf['Site'] = allshots_gdf['Other'].str[:7].str.strip().str.replace(' ', '-')

allshots_gdf['Site'] = allshots_gdf['Site'].str.upper()

#%%
allshots_gdf = allshots_gdf[~allshots_gdf['Site'].str.contains('5')]
allshots_gdf = allshots_gdf[~allshots_gdf['Site'].str.contains('6')]

#%%
# Add inverse horizontal precision field for mean center weighting
allshots_gdf['Inv_Horz_Prec'] = 1/allshots_gdf['Horz_Prec']

#%%
# Calculate mean center of point clusters, grouping by corner direction
def cleansites(gpdf):
    
    meanpoints = []

    sites = list(gpdf['Site'].unique())
    
    def geomeancenter(pnt):
        lon = np.sum(pnt.geometry.x * pnt.Inv_Horz_Prec)/np.sum(pnt.Inv_Horz_Prec)
        lat = np.sum(pnt.geometry.y * pnt.Inv_Horz_Prec)/np.sum(pnt.Inv_Horz_Prec)
        geomcenter = shapely.geometry.Point([lon,lat])
        return geomcenter
    
    def meanhorzprec(pnt):
        mhp = np.mean(pnt['Horz_Prec'])
        return mhp

    for s in sites:
        ploti = gpdf[gpdf['Site'] == s]
        cgroups = geomeancenter(ploti)
        mhp = meanhorzprec(ploti)
        gs = {'SITE_ID':s, 'HORZ_PREC':mhp, 'geometry':cgroups}
        meanpoints.append(gs)
    
    cgroups_gs = gpd.GeoDataFrame(meanpoints, crs='EPSG:32613').reset_index(drop=True)
    cgroups_gs['X_COORD'] = cgroups_gs.geometry.x
    cgroups_gs['Y_COORD'] = cgroups_gs.geometry.y
        
    return cgroups_gs[['SITE_ID', 'X_COORD', 'Y_COORD', 'HORZ_PREC', 'geometry']]

#%%
cleaned_sensites = cleansites(allshots_gdf)

#%%
# Write out all sensor sites
outdir = '/Users/hmworsham/Desktop/SensorSites/'
filename = os.path.join(outdir, 'Kueppers_EastRiver_SensorSites_2021_WGS84UTM13N.shp')
cleaned_sensites.to_file(filename)

#%%
grouped = cleaned_sensites.groupby('SITE_ID')
gdfs=[]
for key, obj in grouped:
    gdfs.append(obj)

#%%
for key, obj in grouped:
    outdir='/Users/hmworsham/Desktop/SensorSites/'
    plotid = obj['SITE_ID'].values[0]
    filename = os.path.join(outdir, plotid+'_SensorSite_WGS84UTM13N')
    obj.to_file(filename)
#%%
# Split into linestrings
all_lines = all_poly.copy()
all_lines.geometry = all_poly.boundary

linegroups = all_lines.groupby('PLOT_ID')

for key, obj in linegroups:
    outdir='/Users/hmworsham/Desktop/Lines/'
    plotid = obj['PLOT_ID'].values[0]
    filename = os.path.join(outdir, plotid+'_Bound_lines_WGS84UTM13N')
    obj.to_file(filename)

#%%
all_centers = all_poly.copy()
all_centers.geometry = all_poly.centroid
all_centers = all_centers[['PLOT_ID', 'CENTROID_X', 'CENTROID_Y', 'geometry']]
all_centers.rename(columns={'CENTROID_X':'X_COORD', 'CENTROID_Y':'Y_COORD'},inplace=True)
centgroups = all_centers.groupby('PLOT_ID')

#%%
for key, obj in centgroups:
    outdir='/Users/hmworsham/Desktop/Center_Points/'
    plotid = obj['PLOT_ID'].values[0]
    filename = os.path.join(outdir, plotid+'_PlotCenter_WGS84UTM13N')
    obj.to_file(filename)

#%%
# NEED TO MATCH FORMATS OF EXISTING FILES
allplots_format = gpd.read_file('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N/AllPlots/Kueppers_EastRiver_AllPlots_2020_WGS84UTM13N.shp')

cornerpts_format = gpd.read_file('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N/Corner_Points/CC-UC1_Bound_pts_WGS84UTM13N.shp')

lines_format = gpd.read_file('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N/Lines/CC-UC1_Bound_lines_WGS84UTM13N.shp')

centers_format = gpd.read_file('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N/Center_Points/CC-UC1_PlotCenter_WGS84UTM13N.shp')

##################################
#%%


# Generate lines from mean center points
pt2lnin = ap.env.workspace + "\\" + i[0:-12] + 'MeanCtr.shp'
pt2lnout = pt2lnin[0:-4] + '_line.shp'
ap.PointsToLine_management(pt2lnin, pt2lnout, '', 'Corners', 'CLOSE')

# Generate polygon from lines
ln2plin = pt2lnout
ln2plout = pt2lnout[0:-9] + '_poly.shp'
ap.FeatureToPolygon_management(ln2plin, ln2plout, '', 'ATTRIBUTES')

# Calculate geometries
ap.AddGeometryAttributes_management(ap.env.workspace + "\\PL1_Corners_MeanCtr_poly.shp", 'AREA; AREA_GEODESIC; CENTROID; EXTENT; PERIMETER_LENGTH; PERIMETER_LENGTH_GEODESIC', 'METERS', 'SQUARE_METERS')

# Gather the points, lines, and polys into separate groups
allpoints = ap.ListFeatureClasses(wild_card = '*MeanCtr.shp') + ap.ListFeatureClasses(wild_card = '*pts*')
alllines = ap.ListFeatureClasses(wild_card = '*line*')
allpolys = ap.ListFeatureClasses(wild_card = '*poly*')

# Reproject shapefiles into WGS 1984 UTM Zone 13N spatial reference (used in RMBL SDP)
outdir = 'Y:\\Desktop\\RMBL\\Projects\\Watershed_Spatial_Dataset\\Output\\Kueppers_Plot_Bnd_2020_WGS84UTM13N\\'

for sf in allpoints:
    ap.Project_management(sf, outdir + sf.split('_')[0] + '_Bound_pts_WGS84UTM13N.shp', ap.SpatialReference('WGS 1984 UTM Zone 13N'), 'NAD_1983_to_WGS_1984_5')

for sf in alllines:
    ap.Project_management(sf, outdir + sf.split('_')[0] + '_Bound_lines_WGS84UTM13N.shp', ap.SpatialReference('WGS 1984 UTM Zone 13N'), 'NAD_1983_to_WGS_1984_5')

for sf in allpolys:
    ap.Project_management(sf, outdir + sf.split('_')[0] + '_Bound_poly_WGS84UTM13N.shp', ap.SpatialReference('WGS 1984 UTM Zone 13N'), 'NAD_1983_to_WGS_1984_5')

# add fields to hold geometric center lat/long in decimal degrees
ap.env.workspace = outdir
wgsutm_allpolys = ap.ListFeatureClasses(wild_card = '*poly*')
wgsutm_allpolys

for sf in wgsutm_allpolys:
    ap.DeleteField_management(sf, 'GEOMCTR_X')
    ap.DeleteField_management(sf, 'GEOMCTR_Y')
    ap.AddField_management(sf, 'GEOMCTR_X', 'LONG', 12, 7, field_is_nullable = 'NULLABLE', field_is_required = 'NON_REQUIRED')
    ap.AddField_management(sf, 'GEOMCTR_Y', 'LONG', 12, 7, field_is_nullable = 'NULLABLE', field_is_required = 'NON_REQUIRED')

# calculate geometric center of plot polygons
#wgsutm_allpolys[0].split('_')[0]
for sf in wgsutm_allpolys:
    ap.MeanCenter_stats(sf, outdir + "\\" + sf.split('_')[0] + '_PlotCenter_WGS84UTM13N.shp')

# Merge polygons into one 2020 plots shapefile
ap.env.workspace = outdir
wgsutm_allpolys = ap.ListFeatureClasses(wild_card = '*poly*')
wgsutm_allpolys

allplotsin = wgsutm_allpolys
allplotsout = outdir + 'Kueppers_AllPlots_2020_WGS84UTM13N.shp'
ap.Merge_management(allplotsin, allplotsout)
#ap.Project_management(allplotsout, allplotsout[:-15] + 'WGS84UTM13N.shp', ap.SpatialReference('WGS 1984 UTM Zone 13N'))
# %%
