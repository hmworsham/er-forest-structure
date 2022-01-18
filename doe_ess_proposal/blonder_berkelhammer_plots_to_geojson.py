import geopandas as gpd
import os
import pandas as pd
import numpy as np
import shapely

datadir = '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/'

# Blonder aspen sites
aspensites = gpd.read_file(os.path.join(
    datadir,
    'Blonder_Aspen_Plots_2020/aspen data site-level processed 30 Mar 2020.csv'))

aspensites.geometry = gpd.points_from_xy(
    aspensites.Longitude, aspensites.Latitude)

aspensites.crs = 'EPSG:4326'

aspensites.plot()

aspensites.to_file(
    './blonder_aspen_sites_2020.geojson', driver='GeoJSON')

# Join aspen sites with dendro sampled list
aspencored = pd.read_csv(os.path.join(
    datadir,
    'Blonder_Aspen_Plots_2020/aspen dendro 2021 - completed sites.csv'))

aspencored.head()
aspensites.head()

aspensites = aspensites.merge(aspencored, on='Site_Code', how='left')

aspensites.to_file(
    '../../../Desktop/blonder_aspen_sites_cored_2021.geojson',
    driver='GeoJSON')

# Berkelhammer sapflux sites
sapfluxsites = gpd.read_file(
    '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Berkelhammer_Still_Sapflux_Sites_2021/SapSites.csv')

sapfluxsites.geometry = gpd.points_from_xy(
    sapfluxsites.longitude, sapfluxsites.latitude)

sapfluxsites.plot()

sapfluxsites.to_file(
    './berkelhammer_still_sapflux_sites_2021.geojson', driver='GeoJSON')
