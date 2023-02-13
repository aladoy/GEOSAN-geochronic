'''This code assign Follow-up participants to the RELI (inhabited hectare) where they live.'''

import pandas as pd
import geopandas as gpd
import sys
import os


sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
    import basic_utils as bu  # commit ede2be490aa3b2cabeb82bd514b9376c80ea573a
except FileNotFoundError:
    print("Wrong file or file path")


def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    # Extract F2 participants and inhabited hectares (2021)
    indiv = gpd.read_postgis("SELECT * FROM geochronic.f2_study_dataset",
                             conn, geom_col="geometry")
    reli = gpd.read_postgis("SELECT * FROM vd_reli_polygon",
                            conn, geom_col="geometry")

    # Restrict searching radius to 300 meters
    indiv['reli'] = indiv.apply(bu.find_nearest_reli, args=(reli, 300), axis=1)

    print("Do all individuals have a RELI (check from DataFrame)? " +
          str(indiv[indiv.reli.isna()].shape[0] == 0))
    print("If some individuals have not be assigned to a RELI (searching radius too small), the UPDATE query will throw an error because the attribute RELI will be float and not integer.")

    # Add this to GEOSAN DB table
    table_name = "geochronic.f2_study_dataset"
    cursor.execute(f"ALTER TABLE {table_name} DROP COLUMN reli;")
    conn.commit()
    cursor.execute(f"ALTER TABLE {table_name} ADD COLUMN reli integer;")
    conn.commit()

    for i, value in enumerate(indiv["reli"]):

        pt = indiv.loc[i, "pt"]
        cursor.execute(
            f"UPDATE {table_name} SET reli = '{value}' WHERE pt = {pt}")

    conn.commit()

    cursor.execute(f"SELECT * FROM {table_name} WHERE reli IS NULL")
    print("Check that everything was correctly inserted into DB " +
          str(len(cursor.fetchall()) == 0))

    conn.close()


if __name__ == "__main__":
    main()
