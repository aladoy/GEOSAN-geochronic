'''
This code builds the residential history of CoLaus F2 participants.
More specifically, it adds a new column in the database to indicate if a participant moved since last examination (F1 / baseline if missing) and at which distance.
'''

import pandas as pd
from geopandas import read_postgis
import shapely.geometry
import sys
import os
import numpy as np

sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
except FileNotFoundError:
    print("Wrong file or file path")


def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    baseline = extract_dataset("geochronic.colaus_b", conn)
    f1 = extract_dataset("geochronic.colaus_f1", conn)
    # extract VD geocoded indiv. only
    f2 = extract_dataset("geochronic.f2_geo_vaud", conn)

    for i in f2.pt:

        current_coord = f2.loc[f2.pt == i, 'geometry'].values[0]

        last_coord = get_last_coord(i, f1, baseline)

        if last_coord is None:

            f2.loc[f2.pt == i, ['has_moved_dist', 'has_moved']] = np.nan, False

        else:

            results = check_if_moved(current_coord, last_coord, dist=300)

            f2.loc[f2.pt == i, ['has_moved_dist', 'has_moved']
                   ] = results[0], results[1]

    conn.close()

    db.insert_attribute("geosan", "aladoy", "geochronic.f2_geo_vaud",
                        "has_moved", "boolean", "pt", f2)

    db.insert_attribute("geosan", "aladoy", "geochronic.f2_geo_vaud",
                        "has_moved_dist", "numeric", "pt", f2)


def extract_dataset(table_name, conn):

    dataset = read_postgis(
        f"SELECT pt, init_addr, gkode, gkodn, geometry FROM {table_name} WHERE NOT ST_IsEmpty(geometry)", conn, geom_col="geometry")

    return dataset


def get_last_coord(pt, f1, baseline):

    try:
        last_coord = f1.loc[f1.pt == pt, 'geometry'].values[0]

    except IndexError:

        try:
            last_coord = baseline.loc[baseline.pt == pt, 'geometry'].values[0]

        except IndexError:
            last_coord = None

    return last_coord


def check_if_moved(current_coord, last_coord, dist=300):

    distance = current_coord.distance(last_coord)

    if (distance >= dist):
        has_moved = True
    else:
        has_moved = False

    return distance, has_moved


if __name__ == "__main__":
    main()
