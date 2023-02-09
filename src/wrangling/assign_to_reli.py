'''This code assign Follow-up participants to the RELI (inhabited hectare) where they live.'''


import numpy as np
from shapely.ops import nearest_points
import pandas as pd
import geopandas as gpd
import sys
import os


sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
    import basic_utils as bu
except FileNotFoundError:
    print("Wrong file or file path")


project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/COLAUS NCDS @ LASIG (EPFL)/GEOSAN-colaus-ncds/"

engine, conn, cursor = db.connect_db("geosan", "aladoy")

# Extract F2 participants and inhabited hectares (2021)
indiv = gpd.read_postgis("SELECT * FROM syndemic.colaus_f2_complete",
                         conn, geom_col="geometry")
reli = gpd.read_postgis("SELECT * FROM vd_reli_polygon",
                        conn, geom_col="geometry")


indiv['reli'] = indiv.apply(find_nearest_reli, args=(reli, 500), axis=1)

indiv[indiv.reli.isna()]

def find_nearest_reli(row, ha_poly, radius=None):
    """
    This function retrieves the nearest neighbor (Point) from a given Point.
    To speed-up the process, the user can restrict the set of destination
    points to a given neighborhood (radius around the point of origin).
    Outputs: nearest RELI index (NaN for isolated point)
    """
    try:
        nearest_reli = ha_poly[ha_poly.geometry.contains(
            row.geometry)]['reli'].values[0]

    except IndexError:

        try:

            if radius is not None:

                ha_poly = ha_poly.loc[
                    ha_poly.intersects(row.geometry.buffer(radius))
                ]

            ha_poly['distance'] = ha_poly.geometry.distance(row.geometry)
            ha_poly = ha_poly.sort_values(by='distance')
            nearest_reli = ha_poly.iloc[0]['reli']

        except IndexError:
            nearest_reli = np.nan  # no hectares within this distance

    return nearest_reli
