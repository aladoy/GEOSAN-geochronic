'''
Extract indicators from Commune en Santé project or GEOSAN DB, which are pertinent for chronic diseases.
'''

import pandas as pd
import geopandas as gpd
import sys
import os
import numpy as np
import matplotlib.pyplot as plt

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

    statpop = statpop[["RELI", "PTOT", "FTOT", "MTOT", "M_45_54", "M_55_64",
                       "M_65_74", "M_75_84", "M_85_M", "F_45_54", "F_55_64", "F_65_74", "F_75_84", "F_85_M"]]

    # Compute ratio instead of absolute number for demographics
    statpop.iloc[:, 6:] = statpop.iloc[:, 6:].apply(
        compute_ratio_demo, arg=statpop.PTOT)

    # HA INDICATORS FROM COMMUNE EN SANTE (ALREADY FILTERED PTOT > 3)
    ha = gpd.read_postgis(
        "SELECT reli, e_koord, n_koord, mun_index, noise, geometry FROM ha_indicators WHERE ST_Intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))", conn, geom_col="geometry")

    print("Number of hectares in Lausanne: ", ha.shape[0])

    # SOCIAL VARIABLES (MICROREGIONS 2017)
    #   Extract microregions polygons (and associated characteristics) that intersect each hectare
    microregions = gpd.read_postgis(
        """SELECT reli, nbid, r_nn_pobl, r_nn_ch, r_unemp, medrev, p15m, ptot, ST_Intersection(h.geometry, m.geometry) as geometry
    FROM (SELECT reli, geometry FROM ha_indicators WHERE ST_Intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))) h,
    (SELECT nbid, 100*(pfnone + pfobl) as R_NN_POBL, (100-rpnch) as R_NN_CH, 100*adune as R_UNEMP, ciqmd as MEDREV, (ptot-(p0004+p0509+p1014)) as P15M, ptot as PTOT, geometry
    FROM microgis_microreg) m
    WHERE ST_Intersects(h.geometry, m.geometry)""",
        conn, geom_col="geometry")

    microregions.columns = [
        col.upper() if col != "geometry" else col for col in microregions.columns]
    microregions["R_NN_POBL"] = np.where(
        microregions["P15M"] != 0, microregions["R_NN_POBL"] / microregions["P15M"], 0)
    microregions["R_UNEMP"] = np.where(
        microregions["P15M"] != 0, microregions["R_UNEMP"] / microregions["P15M"], 0)

    # Initialize new columns
    ha['R_NN_POBL'] = 0.0
    ha['R_UNEMP'] = 0.0
    ha['R_NN_CH'] = 0.0
    ha['MEDREV'] = 0.0

    # Iterate over each row in the DataFrame
    for index, row in ha.iterrows():

        r_nn_pobl_avg = calculate_weighted_avg(
            row.reli, microregions, "R_NN_POBL", type="surface")
        ha.at[index, 'R_NN_POBL'] = r_nn_pobl_avg

        r_unemp_avg = calculate_weighted_avg(
            row.reli, microregions, "R_UNEMP", type="surface")
        ha.at[index, 'R_UNEMP'] = r_unemp_avg

        r_nn_ch_avg = calculate_weighted_avg(
            row.reli, microregions, "R_NN_CH", type="surface")
        ha.at[index, 'R_NN_CH'] = r_nn_ch_avg

        medrev_avg = calculate_weighted_avg(
            row.reli, microregions, "MEDREV", type="surface")
        ha.at[index, 'MEDREV'] = medrev_avg

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


def calculate_weighted_avg(reli, df, var, type="surface"):

    # Filter the DataFrame based on the target RELI value
    filtered_df = df[df['RELI'] == reli]

    if type == "count":
        # Calculate the total count of polygons
        total_count = filtered_df.shape[0]

        # Calculate the weighted mean by polygon count
        weighted_mean = filtered_df[var].sum() / total_count

    elif type == "surface":
        # Calculate the total area of all polygons
        total_area = filtered_df['geometry'].area.sum()

        # Calculate the weighted mean by surface area
        weighted_mean = (
            filtered_df[var] * filtered_df['geometry'].area).sum() / total_area
    else:
        raise ValueError(
            "Invalid type value. Please choose either 'count' or 'surface'.")

    return weighted_mean


if __name__ == "__main__":
    main()
