import pandas as pd
import geopandas
import numpy as np
import alphashape
import os

os.chdir(r"C:\\Users\\WAI045\\OneDrive - CSIRO\\ALA\\Labs")

fungi_data = pd.read_csv("honeyeater_data.csv")

species = list(set(fungi_data["species"]))

# GeoPandas data frame to contain all alpha shapes
alpha_shape_gdf = geopandas.GeoDataFrame()
for i, s in enumerate(species):
    species_points = fungi_data[["decimalLongitude", "decimalLatitude"]] [fungi_data["species"] == s]
    if len(species_points) <= 3: 
        continue
    alpha_shape = alphashape.alphashape(species_points, 1.3)
    d = {"species": s, "geometry": [alpha_shape.buffer(0.2)]}
    tmp_gdf = geopandas.GeoDataFrame(d, crs="GDA94")
    alpha_shape_gdf = pd.concat([alpha_shape_gdf, tmp_gdf])
    
alpha_shape_gdf.to_file('Shapefiles\\honeyeater_alphahulls\\honeyeater_alphahulls.shp', driver='ESRI Shapefile')