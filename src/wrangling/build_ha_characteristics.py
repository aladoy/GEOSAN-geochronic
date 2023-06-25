'''
Extract indicators from Commune en Santé project or GEOSAN DB, which are pertinent for chronic diseases.
'''

import pandas as pd
import geopandas as gpd
import sys
import os

sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import basic_utils as bu
    import db_utils as db
except FileNotFoundError:
    print("Wrong file or file path")

project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/"
ces_dir: str = (
    r"/mnt/data/GEOSAN/RESEARCH PROJECTS/COMMUNE EN SANTE @ UNISANTE/commune-en-sante")
data_dir: str = r"/mnt/data/GEOSAN/GEOSAN DB/data"


def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    # STATPOP 2017
    statpop = pd.read_csv(
        os.sep.join([data_dir, "STATPOP/2017/STATPOP2017.csv"]), sep=","
    )

    # Build demographic categories
    statpop["M_45_54"] = statpop["B17BM10"] + statpop["B17BM11"]
    statpop["M_55_64"] = statpop["B17BM12"] + statpop["B17BM13"]
    statpop["M_65_74"] = statpop["B17BM14"] + statpop["B17BM15"]
    statpop["M_75_84"] = statpop["B17BM16"] + statpop["B17BM17"]
    statpop["M_85_M"] = statpop["B17BM18"] + statpop["B17BM19"]

    statpop["F_45_54"] = statpop["B17BW10"] + statpop["B17BW11"]
    statpop["F_55_64"] = statpop["B17BW12"] + statpop["B17BW13"]
    statpop["F_65_74"] = statpop["B17BW14"] + statpop["B17BW15"]
    statpop["F_75_84"] = statpop["B17BW16"] + statpop["B17BW17"]
    statpop["F_85_M"] = statpop["B17BW18"] + statpop["B17BW19"]

    # Rename some variables
    statpop.rename(columns={"B17BTOT": "PTOT", "H17PTOT": "HTOT",
                            "B17BWTOT": "FTOT", "B17BMTOT": "MTOT"}, inplace=True)

    statpop = statpop[["RELI", "PTOT", "HTOT", "FTOT", "MTOT", "M_45_54", "M_55_64",
                       "M_65_74", "M_75_84", "M_85_M", "F_45_54", "F_55_64", "F_65_74", "F_75_84", "F_85_M"]]

    # Compute ratio instead of absolute number for demographics
    statpop.iloc[:, 6:] = statpop.iloc[:, 6:].apply(
        compute_ratio_demo, arg=statpop.PTOT)

    # HA INDICATORS FROM COMMUNE EN SANTE (ALREADY FILTERED PTOT > 3)
    ha = gpd.read_postgis(
        "SELECT reli, e_koord, n_koord, mun_index, noise, medrev, r_ffb, r_nn_fra, r_nn_pobl, r_unemp, geometry FROM ha_indicators WHERE ST_Intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))", conn, geom_col="geometry")

    # Read pedestrian / cyclists accidents
    accidents = pd.read_sql(
        "SELECT reli, SUM(nb_acdnt) as nb_acdnt FROM pedestrian_cyclist_accidents WHERE year <=2017 GROUP BY reli", conn)

    ha = pd.merge(ha, accidents, how='left', on='reli')

    new_columns = [col.upper() if col !=
                   "geometry" else col for col in ha.columns]
    ha.columns = new_columns

    # AIR POLLUTION
    pm25 = extract_airpol("PM25")
    pm10 = extract_airpol("PM10")
    no2 = extract_airpol("NO2")

    # WALKABILITY
    walkability = gpd.read_file(
        os.sep.join(
            [
                project_dir,
                "processed_data/walkability_measures.gpkg"
            ]
        ),
        driver="GPKG",
    )
    walkability = walkability.rename(
        columns=lambda x: x.upper()).drop("GEOMETRY", axis=1)

    # CONCATENATE ALL VARIABLES IN A SINGLE INDEX
    indicators = pd.merge(ha, statpop, on="RELI", how="inner")
    indicators = pd.merge(indicators, pm25, on="RELI", how="inner")
    indicators = pd.merge(indicators, pm10, on="RELI", how="inner")
    indicators = pd.merge(indicators, no2, on="RELI", how="inner")
    indicators = pd.merge(indicators, walkability, on="RELI", how="inner")

    print("Number of hectares with data: ", indicators.shape[0])

    # SAVE INDICATORS TO FILE
    bu.save_gdf(os.sep.join([project_dir, "processed_data"]),
                "ha_characteristics.gpkg", indicators, driver="GPKG", del_exist=True)

    # IMPORT TO DATABASE
    db.import_data('geosan', 'aladoy', indicators, 'ha_characteristics', pk='reli',
                   schema='geochronic', idx_geom=True, ifexists='replace')

    conn.close()


def compute_ratio_demo(var, arg):
    ratio = 100 * (var / arg)
    return ratio


def extract_airpol(var_name):
    filename = var_name + ".gpkg"
    df = gpd.read_file(
        os.sep.join(
            [
                ces_dir,
                "processed_data/environmental_exposures/airpol_2015/" + filename,
            ]
        ),
        driver="GPKG",
    )
    df = df[["reli", "mean"]]
    df = df[~df["mean"].isna()]
    df.columns = ["RELI", var_name.upper()]
    return df


if __name__ == "__main__":
    main()
