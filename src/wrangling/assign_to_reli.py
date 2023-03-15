'''This code assign Follow-up participants to the RELI (inhabited hectare) where they live.'''

import pandas as pd
import geopandas as gpd
import sys
import os


sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db # commit: bf78b2d
    import basic_utils as bu # commit: bf78b2d
except FileNotFoundError:
    print("Wrong file or file path")


def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    # Extract F2 participants (geocoded and in VD) and inhabited hectares (2021)
    indiv = gpd.read_postgis("SELECT * FROM geochronic.f2_geo_vaud",
                             conn, geom_col="geometry")
    reli = gpd.read_postgis("SELECT * FROM vd_reli_polygon",
                            conn, geom_col="geometry")

    # Restrict searching radius to 300 meters
    indiv['reli'] = indiv.apply(bu.find_nearest_reli, args=(reli, 300), axis=1)

    print("Do all individuals have a RELI (check from DataFrame)? " +
          str(indiv[indiv.reli.isna()].shape[0] == 0))
    print("If some individuals have not be assigned to a RELI (searching radius too small), the UPDATE query will throw an error because the attribute RELI will be float and not integer.")

    conn.close()

    db.insert_attribute("geosan", "aladoy", "geochronic.f2_geo_vaud",
                        "reli", "integer", "pt", indiv)


if __name__ == "__main__":
    main()
