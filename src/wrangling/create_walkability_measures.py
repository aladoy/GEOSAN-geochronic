import sys
import os
from shapely.geometry import Point
import geopandas as gpd
import pandas as pd


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


def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    # HA INDICATORS FROM COMMUNE EN SANTE (ALREADY FILTERED PTOT > 3)

    ha = gpd.read_postgis(
        "SELECT reli, geometry FROM ha_indicators h WHERE ST_Intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))", conn, geom_col="geometry")
    ha_buff = ha.copy()
    buffer_distance = 500  # Create buffer
    ha_buff["geometry"] = ha.geometry.centroid.buffer(buffer_distance)
    # ha_buff = ha_buff.clip(vd) # Clip with Vaud (for hectares at the border of the lake for example)
    # Buffer's area
    ha_buff["area_buff"] = ha_buff.geometry.area

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

    ha_walkability = ha.merge(
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

    ha_walkability = ha_walkability.merge(
        intersect_buff[["reli", "intden"]], how="left", on="reli")

    ha_walkability.to_file(os.sep.join(
        [
            project_dir,
            "processed_data/walkability_measures.gpkg",
        ]
    ))


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


if __name__ == "__main__":
    main()
