import sys
import os
from shapely.geometry import Point
import geopandas as gpd
import pandas as pd
import rasterio
from rasterstats import zonal_stats
import numpy as np


sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
    import basic_utils as bu
except FileNotFoundError:
    print("Wrong file or file path")

project_dir: str = (
    r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/"
)
geosan_db_dir: str = r"/mnt/data/GEOSAN/GEOSAN DB/data"
ces_dir: str = (
    r"/mnt/data/GEOSAN/RESEARCH PROJECTS/COMMUNE EN SANTE @ UNISANTE/commune-en-sante")

def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    # Hectares (VD) 
    ha_vd = gpd.read_postgis(
        "SELECT reli, geometry FROM vd_reli_polygon", conn, geom_col="geometry")
    
    # Buffer of 500m around each hectare's centroid
    ha_buff = gpd.read_postgis("SELECT reli, ST_Buffer(ST_Centroid(geometry),500) as geometry FROM vd_reli_polygon r WHERE ST_Intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))", conn, geom_col="geometry")
    ha_buff["area_buff"] = ha_buff.geometry.area


    # HA - LAUSANNE EXTENT
    ha = gpd.read_postgis("SELECT reli, geometry FROM vd_reli_polygon r WHERE ST_Intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))", conn, geom_col="geometry")


    # NOISE AND AIR POLLUTION

    pm25_path = os.sep.join([geosan_db_dir, "AIR POLLUTION SWITZERLAND/pm25/pm25_2015/pm25_2015.tif"])
    pm25 = get_airpol_value(pm25_path, ha_buff)[["reli", "mean"]]
    pm25.columns = ["reli", "PM25"]
    ha_environment = ha.merge(
        pm25[["reli", "PM25"]], how="left", on="reli")

    no2_path = os.sep.join([geosan_db_dir, "AIR POLLUTION SWITZERLAND/no2/no2_2015/no2_2015.tif"])
    no2 = get_airpol_value(no2_path, ha_buff)[["reli", "mean"]]
    no2.columns = ["reli", "NO2"]
    ha_environment = ha_environment.merge(
        no2[["reli", "NO2"]], how="left", on="reli")

    noise = zonal_stats(
        ha_buff,
        os.sep.join(
            [
                ces_dir,
                "processed_data/environmental_exposures/total_night_noise_vd.tif",
            ]
        ),
        band_num=1,
        nodata=255,
        geojson_out=True,
    )
    noise = gpd.GeoDataFrame.from_features(noise, crs=2056)[["reli", "mean"]]
    noise.columns = ["reli", "NOISE"]
    ha_environment = ha_environment.merge(
        noise[["reli", "NOISE"]], how="left", on="reli")

    # GREENSPACES

    cadastre = gpd.read_file(
        os.sep.join([geosan_db_dir, "LAND COVER VD/2015/LC_merged_fixed.geojson"]), driver="GeoJSON"
    )
    # Create a single geodaframe with green and wood (measure of greeness)
    greenspace = cadastre[cadastre.Genre.str.startswith(("boisee.", "verte."))]
    intersect_greenspace = gpd.sjoin(
        ha_buff, greenspace[["id", "geometry"]], how="left", predicate="intersects"
    )

    # Define greenspace availability as the percentage of greenspace area over the buffer area
    ha_buff["green_sp"] = ha_buff.reli.apply(
        lambda x: compute_area(x, ha_buff, greenspace, intersect_greenspace)
    )

    ha_environment = ha_environment.merge(
        ha_buff[["reli", "green_sp"]], how="left", on="reli")

    
    # INTERSECTION DENSITY (STREET CONNECTIVITY)

    roads = gpd.read_file(os.sep.join(
        [project_dir, "processed_data/walkable_roads/walkable_roads_lausanne_01jan2016_intersections_2056.geojson"]))
    roads = roads[["id", "geometry"]]
    roads.reset_index(drop=False, inplace=True)
    roads['x'] = roads.geometry.x
    roads['y'] = roads.geometry.y

    nb_intersect = gpd.sjoin(ha_buff, roads[["index", "geometry"]],
                             how='inner', predicate='contains')
    nb_intersect = nb_intersect.groupby(
        "reli").size().reset_index(name='count')

    intersect_buff = ha_buff.merge(nb_intersect, how="left", on="reli")
    intersect_buff['count'] = intersect_buff['count'].fillna(0)
    # Define intersection density as the number of intersections divided by the buffer area (De Courreges et al., 2021)
    intersect_buff['intden'] = intersect_buff['count'] / \
        intersect_buff['area_buff']

    ha_environment = ha_environment.merge(
        intersect_buff[["reli", "intden"]], how="left", on="reli")

    ha_environment.to_file(os.sep.join(
        [
            project_dir,
            "processed_data/environmental_measures.gpkg",
        ]
    ))



# # Almost same function than build_ha_characteristics.py
# def calculate_weighted_avg(reli, df, var):

#     # Filter the DataFrame based on the target RELI value
#     filtered_df = df[df['reli_buff'] == reli]

#     # Calculate the total area of all polygons
#     total_area = filtered_df['geometry'].area.sum()

#     # Calculate the weighted mean by surface area
#     weighted_mean = (
#         filtered_df[var] * filtered_df['geometry'].area).sum() / total_area

#     return weighted_mean



def intersection_area(landcov_polys, reli_buff):

    intersection = reli_buff.intersection(landcov_polys)

    return intersection.area


def compute_area(reli, ha_buff, landcov_df, joined_df):

    subs = joined_df[joined_df.reli == reli]
    polys = landcov_df[landcov_df.id.isin(subs.id.unique())]

    try:
        area_intersect = (
            polys.geometry.apply(
                intersection_area, args=(ha_buff[ha_buff.reli == reli],)
            )
            .sum()
            .values[0]
        )
    except:
        area_intersect = 0
    perc_landcov = (
        100 * (area_intersect /
               ha_buff[ha_buff.reli == reli].area_buff).values[0]
    )

    return perc_landcov


# def extract_env_exposure(var_name):

#     if var_name=="NOISE":

#         df = gpd.read_file(
#             os.sep.join(
#                 [ces_dir, 
#                 "processed_data/environmental_exposures/total_night_noise_vd.gpkg"]
#                 ),
#                 driver="GPKG",
#                 )

#     else:

#         filename = var_name + ".gpkg"
#         df = gpd.read_file(
#             os.sep.join(
#                 [
#                     ces_dir,
#                     "processed_data/environmental_exposures/airpol_2015/" + filename,
#                 ]
#             ),
#             driver="GPKG",
#         )

#     df = df[["reli", "mean"]]
#     df = df[~df["mean"].isna()]
#     df.columns = ["reli", var_name]
#     return df


def get_airpol_value(raster_path, vector):

    raster = rasterio.open(raster_path)
    print("no data value (will be converted to 255): ", raster.nodata)
    filter_arr = raster.read() < raster.nodata
    max_value = raster.read()[filter_arr].max()
    print("max value (used to filter nodata at the end): ", max_value)

    stats = zonal_stats(vector, raster_path, band_num=1, nodata=255, geojson_out=True)

    geostats = gpd.GeoDataFrame.from_features(stats, crs=2056)
    # the data provider forgot to delimitate decimals
    geostats.loc[:, ["min", "max", "mean"]] = (
        geostats.loc[:, ["min", "max", "mean"]] / 10
    )

    geostats.loc[geostats["max"] * 10 > max_value, "mean"] = np.nan
    geostats.loc[geostats["max"] * 10 == raster.nodata, "max"] = np.nan

    return geostats



if __name__ == "__main__":
    main()

