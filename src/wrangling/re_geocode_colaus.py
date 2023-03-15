'''This code (re)geocodes CoLaus dataset to fix errors in M. Vieira algorithm'''

import pandas as pd
import geopandas as gpd
import sys
import os
from pandarallel import pandarallel
from shapely.geometry import Point
import warnings
import psycopg2 as ps

# TO USE THE FOLLOWING FUNCTIONS, YOU SHOULD GO BACK TO THE TAG
# ??? IN THE GIRAPH-functions REPOSITORY (git checkout geocoding_v2)
sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import geocoding_utils as g  # commit: bf78b2d
    import db_utils as db  # commit: bf78b2d
    import basic_utils as bu  # commit: bf78b2d
except FileNotFoundError:
    print("Wrong file or file path")

project_dir: str = (
    r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/"
)
geosan_db_dir: str = r"/mnt/data/GEOSAN/GEOSAN DB/data"

# REDIRECT STDOUT INTO TEXT FILE
output_print_file = os.sep.join(
    [project_dir, "results", "code_outputs/re_geocode_colaus.txt"]
)
sys.stdout = open(output_print_file, "w")


def main():

    warnings.simplefilter(action='ignore', category=FutureWarning)

    print()
    print("BASELINE")
    print()

    # Import dataset
    baseline = pd.read_csv(os.sep.join(
        [geosan_db_dir, "COLAUS/geodata_marco/BASELINE/F0_encod_mapgeo.csv"]), delimiter=',')  # Geocode on Marco's dataset because the locality is not present in initial_file_address
    baseline.rename(columns={"F1Code": "pt", "Rue": "strname", "No": "deinr",
                             "CP": "npa", "Ville": "ville"}, inplace=True)
    # Geocode & Save
    geocode(baseline, "baseline")

    print()
    print("FOLLOW-UP 1")
    print()

    # Import dataset
    f1 = pd.read_csv(os.sep.join(
        [geosan_db_dir, "COLAUS/geodata_marco/F1/F1_encod_mapgeo.csv"]), delimiter=',', encoding='iso-8859-1')
    f1.rename(columns={"F1Code": "pt", "Rue": "strname", "No": "deinr",
                       "CP": "npa", "Ville": "ville"}, inplace=True)
    # Geocode & Save
    geocode(f1, "f1")

    print()
    print("FOLLOW-UP 2")
    print()

    # Import dataset
    f2 = pd.read_csv(os.sep.join(
        [geosan_db_dir, "COLAUS/geodata_marco/F2/F2_encod_mapgeo.csv"]), delimiter=',', encoding='iso-8859-1')
    f2.rename(columns={"F1Code": "pt", "Rue": "strname", "No": "deinr",
                       "CP": "npa", "Ville": "ville"}, inplace=True)
    # Geocode & Save
    geocode(f2, "f2")

    # CLOSE OUTPUT FILE
    sys.stdout.close()


def geocode(df, period_name):

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    subdir = {'baseline': 0, 'f1': 1, 'f2': 2, 'f3': 3}
    cohort_period = subdir[period_name]
    init_size = extract_request_dataset(cohort_period).shape[0]

    print('Number of participants: ', str(init_size))

    df, remi = remove_addr(df, period_name, init_size, step='initial')
    df = prepare_addr(df)
    df = correct_addr(df, period_name)

    # Unique addresses
    addr = df.drop_duplicates(subset="init_addr", keep="first")[
        ["init_addr", "npa", "ville", "strname", "deinr"]
    ].reset_index(drop=True)
    addr['index'] = addr.index
    print("Number of unique addresses: ", addr.shape[0])

    # Geocoding
    addr = launch_geocoding(addr, 0.8, conn)
    addr = save_corresponding_addr(addr, period_name, conn)

    gdf = assign_coord_indiv(df, addr)
    gdf, remf = remove_addr(gdf, period_name, init_size, step='final')

    print('Number of geocoded participants: ', str(gdf.shape[0]))

    # Save
    filename = period_name + "_geocoded.gpkg"
    bu.save_gdf(os.sep.join(
        [project_dir, "processed_data/geocoding/"]), filename, gdf, driver="GPKG")

    filename = period_name + "_pt_removed.csv"
    remi.append(remf, ignore_index=True).to_csv(os.sep.join(
        [project_dir, "processed_data/geocoding", filename]), index=False)

    # CLOSE CONNECTION WITH GEOSAN DB
    conn.close()


def extract_regbl(conn):

    vd_addr = gpd.read_postgis("SELECT * FROM regbl2021",
                               conn, geom_col="geometry")
    vd_addr = g.regbl_wrangling(vd_addr)

    return vd_addr


def prepare_addr(df):

    # Create an attribute with full initial address
    df["init_addr"] = df["strname"] + " " + df["deinr"] + \
        " " + df["npa"].astype(str) + " " + df["ville"]

    df.loc[df.init_addr.isna(), 'init_addr'] = df["strname"] + " " + \
        df["npa"].astype(str) + " " + df["ville"]

    # Strip accents in addresses
    df["strname"] = df.strname.map(g.strip_accents)
    df["strname"] = df.strname.map(str.upper)
    # Remove space in deinr
    df["deinr"] = df.deinr.str.replace(" ", "").str.lower()

    print("Init address was build correctly? ", str(
        df[df.init_addr.isna()].shape[0] == 0))

    return df


def launch_geocoding(df, fuzzy_threshold, conn):

    # Import addr & npa dataset
    npa = pd.read_csv(os.sep.join(
        [geosan_db_dir, "NPA/2021/PLZO_CSV_LV95.csv"]), delimiter=";", encoding='iso-8859-1')
    vd_addr = extract_regbl(conn)

    pandarallel.initialize(nb_workers=13)
    (
        df["gkode"],
        df["gkodn"],
        df["egid"],
        df["note_geocoding"],
        df["scenario"],
    ) = zip(
        *df.parallel_apply(lambda row: g.get_coords(row, vd_addr, npa, 0.8), axis=1)
    )

    print(df.groupby("note_geocoding").size())

    return df


def save_corresponding_addr(df, period_name, conn):

    vd_addr = extract_regbl(conn).drop_duplicates(
        subset=["egid"], keep="first")

    df_tojoin = df[['init_addr', 'egid', 'strname',
                    'deinr', 'npa', 'ville', 'note_geocoding']]
    vd_addr = vd_addr[['egid', 'strname', 'deinr', 'dplz4', 'gdename']]
    vd_addr['matching_addr'] = vd_addr["strname"] + " " + vd_addr["deinr"] + \
        " " + vd_addr["dplz4"].astype(str) + " " + vd_addr["gdename"]

    nodat = df_tojoin[df_tojoin.note_geocoding.isin(
        ["To remove from the dataset", "Geocoded at NPA centroid. No street number.", "Geocoded at NPA centroid."])]
    nodat = nodat.drop_duplicates(
        subset=["strname", "npa", "ville", "note_geocoding"], keep="first")

    joined = df_tojoin.merge(vd_addr, on='egid', suffixes=['', '_regbl'])
    joined = joined.drop_duplicates(
        subset=["strname", "npa", "ville", "note_geocoding"], keep="first")
    joined = joined[['note_geocoding', 'strname',
                     'strname_regbl', 'npa', 'dplz4', 'ville', 'gdename']]

    df = df.merge(vd_addr[['egid', 'matching_addr']], on='egid', how='left')

    filename_joined = period_name + "_geocoding_check_wegid.csv"
    filename_nodat = period_name + "_geocoding_check_wo_egid.csv"
    joined.sort_values("note_geocoding", ascending=False).to_csv(os.sep.join(
        [project_dir, "processed_data/geocoding", filename_joined]), index=False)
    nodat.sort_values("note_geocoding", ascending=False).to_csv(os.sep.join(
        [project_dir, "processed_data/geocoding", filename_nodat]), index=False)

    return df


def assign_coord_indiv(indiv, addr_geo):

    indiv_geo = indiv[['pt', 'init_addr']].merge(
        addr_geo, on='init_addr', how='left')
    print("Number of geocoded addresses: ", indiv_geo.shape[0])
    print(indiv_geo.groupby("note_geocoding").size())

    indiv_geo = indiv_geo.assign(
        geometry=indiv_geo.apply(
            lambda row: Point(row.gkode, row.gkodn), axis=1)
    )
    indiv_geo = gpd.GeoDataFrame(
        indiv_geo, geometry=indiv_geo.geometry, crs={"init": "epsg:2056"}
    )

    return indiv_geo


def extract_request_dataset(cohort_period):
    # dataset geocoded by Marco Vieira

    filename = "Ladoy" + str(cohort_period + 1) + ".csv"
    data_req = pd.read_csv(os.sep.join(
        [geosan_db_dir, "COLAUS/request_12dec22", filename]), delimiter=';')

    return data_req


def extract_init_dataset(cohort_period):
    # initial dataset with addresses sent by Pedro Marques Vidal to Marco Vieira

    if cohort_period == 0:
        addr_init = pd.read_csv(os.sep.join(
            [geosan_db_dir, "COLAUS/geodata_marco/initial_file_address/AdressesF0.csv"]), delimiter=',')
        addr_init = addr_init[['uid', 'street', 'number', 'zipcode']]

    elif cohort_period == 1:
        addr_init = pd.read_excel(os.sep.join(
            [geosan_db_dir, "COLAUS/geodata_marco/initial_file_address/AdressesFU1.xlsx"]))
        addr_init = addr_init[['F1Code', 'Rue', 'No', 'CP']]

    elif cohort_period == 2:
        addr_init = pd.read_csv(os.sep.join(
            [geosan_db_dir, "COLAUS/geodata_marco/initial_file_address/AdressesFU2.csv"]), delimiter=';')
        addr_init = addr_init[['pt', 'Rue', 'No', 'CP']]

    elif cohort_period == 3:
        addr_init = pd.read_excel(os.sep.join(
            [geosan_db_dir, "COLAUS/geodata_marco/initial_file_address/AdressesFU3.xlsx"]))
        addr_init = addr_init[['F3pt', 'Rue', 'No', 'CP']]

    addr_init.columns = ['pt', 'strname', 'deinr', 'npa']

    return addr_init


def remove_addr(df, period_name, init_size, step='initial'):

    subdir = {'baseline': 0, 'f1': 1, 'f2': 2, 'f3': 3}
    cohort_period = subdir[period_name]

    if step == 'initial':

        req = extract_request_dataset(cohort_period)
        init = extract_init_dataset(cohort_period)

        # remove addr. that are not in request dataset (do not print anything)
        df.drop(df[~df.pt.isin(req.pt)].index, inplace=True)
        init.drop(init[~init.pt.isin(req.pt)].index, inplace=True)

        reason_nocoord = "addresses that were not given to M. Vieira"
        cond_nocoord = req[~req.pt.isin(init.pt)]
        print_deleted_addr(cond_nocoord, reason_nocoord, init_size)

        reason_dropped = "addresses that were dropped by M. Viera"
        cond_dropped = init[~init.pt.isin(df.pt)]
        print_deleted_addr(cond_dropped, reason_dropped, init_size)
        df = df[~df.pt.isin(cond_dropped.pt)]  # delete rows

        pt_removed = cond_nocoord['pt'].append(
            cond_dropped['pt'], ignore_index=True)

    elif step == 'final':

        reason_postal = "addresses that correspond to a postal box"
        cond_postal = df[(df.strname.str.contains(
            "CP ")) | (df.strname.str.contains("Case ")) | (df.strname.str.contains("cp "))]
        print_deleted_addr(cond_postal, reason_postal, init_size)
        df = df[~df.pt.isin(cond_postal.pt)]  # delete rows

        reason_to_remove = "addresses that were labelled 'To remove'"
        cond_to_remove = df[df.note_geocoding == "To remove from the dataset"]
        print_deleted_addr(cond_to_remove, reason_to_remove, init_size)
        df = df[~df.pt.isin(cond_to_remove.pt)]  # delete rows

        reason_npa = "Addresses that were geocoded at a NPA centroid"
        cond_npa = df[df.note_geocoding.isin(
            ["Geocoded at NPA centroid.", "Geocoded at NPA centroid. No street number."])]
        print_deleted_addr(cond_npa, reason_npa, init_size)
        df = df[~df.pt.isin(cond_npa.pt)]  # delete rows

        pt_removed = cond_postal['pt'].append(
            cond_to_remove['pt'], ignore_index=True).append(cond_npa['pt'], ignore_index=True)

    return df, pt_removed


def print_deleted_addr(df, reason, init_size):

    if reason == "addresses that were not given to M. Vieira":
        df = df[['pt']]
    else:
        df = df[['pt', 'strname', 'deinr', 'npa']]
    print(reason.upper())
    print("Number of participants removed: " +
          str(df.shape[0]) + " (" + str(round((100*df.shape[0]/init_size), 2)) + "%)")
    print(df)
    print("")


def correct_addr(df, period_name):

    df.loc[(df.strname.str.contains("CHAMPRILLY")) & (
        df.npa == 1008), ['npa', 'ville']] = [1004, "Lausanne"]
    df.loc[(df.strname.str.contains("BENJAMIN-DUMUR")) & (
        df.npa == 1008), ['npa', 'ville']] = [1004, "Lausanne"]
    df.loc[(df.strname.str.contains("AV. DU 24-JANVIER")) & (
        df.npa == 1004), ['npa', 'ville']] = [1020, "Renens (VD)"]
    df.loc[(df.strname.str.contains("GUILLAUME-DE-PIERREFLEUR")) & (
        df.npa == 1018), ['npa']] = [1004]
    df.loc[(df.strname.str.contains("AV. DE RHODANIE")) & (
        df.npa == 1004), ['npa']] = [1007]
    df.loc[(df.strname.str.contains("CH. DU MUGU")) & (
        df.npa == 1007), ['strname', 'deinr']] = ["CHEMIN DU MUGUET", None]  # assign deinr to None because no deinr in VD addr
    df.loc[(df.strname.str.contains("AV.DE FRANCE")) & (
        df.npa == 1004), ['strname']] = ["AVENUE DE FRANCE"]
    df.loc[(df.strname.str.contains("DE LA METAIRIE")) & (
        df.npa == 1006), ['npa', 'ville']] = [1009, "Pully"]
    df.loc[df.strname.str.contains("BD DE GRANCY"), ['strname']] = [
        "BOULEVARD DE GRANCY"]
    df.loc[(df.strname.str.contains("DE-LA-HARPE")) & (
        df.npa == 1007), ['strname']] = ["AVENUE FREDERIC-CESAR-DE-LA-HARPE"]
    df.loc[df.strname.str.contains("AV. DES FIGUIERS"), ['strname']] = [
        "AVENUE DES FIGUIERS"]
    df.loc[df.strname.str.contains("AV. DE LA SALLAZ"), ['strname']] = [
        "AVENUE DE LA SALLAZ"]
    df.loc[df.strname.str.contains("PASS. DES SAUGETTES"), ['strname']] = [
        "PASSAGE DES SAUGETTES"]
    df.loc[(df.strname.str.contains("RTE DE PRILLY")) & (
        df.npa == 1008), ['strname']] = "ROUTE DE CERY"  # we checked with given coordinates - must be an old address
    df.loc[((df.strname.str.contains("DE LA FAUVETTE")) & (df.npa == 1012)), ['strname']] = [
        "CHEMIN DE LA FAUVETTE"]

    df.loc[(df.strname.str.contains("DE LA BORDE")) & (
        df.ville == "Lausanne"), ['strname', 'npa']] = ['RUE DE LA BORDE', 1018]
    df.loc[(df.strname == "RTE DE CHAVANNES 20") & (
        df.deinr == "3"), ['strname', 'deinr']] = ['ROUTE DE CHAVANNES', "203"]  # Look in Baseline
    df.loc[df.strname == "10, CH. DU MURIER", ['strname', 'deinr']] = [  # Look in F2
        "CHEMIN DU MURIER", "10"]
    df.loc[df.strname == "AV. DE COUR 12", ['strname', 'deinr']] = [  # Look in F2
        "AVENUE DE COUR", "12"]
    df.loc[(df.strname == "AV. DE LA GARE") & (
        df.ville == "Lausanne"), ['strname', 'npa']] = ['AVENUE DE LA GARE', 1003]
    df.loc[(df.strname == "RTE DU GRAND MONT") & (
        df.npa == 1052), ['strname']] = ["ROUTE DU GRAND-MONT"]
    df.loc[(df.strname == "RTE DES CULLAYES") & (
        df.ville == "Servion"), ['strname', 'npa']] = ['ROUTE DES CULLAYES', 1077]
    df.loc[(df.strname == "RTE DE VAULION") & (
        df.npa == 1324), ['strname']] = ["ROUTE DE VAULION"]
    df.loc[(df.strname == "CH DE CHESALLES") & (
        df.npa == 1683), ['strname']] = ["ROUTE DE CHESALLES"]
    df.loc[(df.strname == "CH. DU BOUARD") & (
        df.npa == 1032), ['strname']] = ["CHEMIN DU BOULARD"]
    df.loc[(df.strname == "PERDONNET") & (
        df.npa == 1800), ['strname']] = ["QUAI PERDONNET"]
    df.loc[(df.strname.str.contains("AU CHATEAU")) & (
        df.npa == 1521), ['strname']] = ["CHEMIN DU CHATEAU"]
    df.loc[(df.strname == "CH DE SOUS ROCHE") & (
        df.npa == 1052), ['strname']] = ["CHEMIN SOUS-ROCHE"]
    df.loc[(df.strname == "CH LA PAIX") & (
        df.npa == 1802), ['strname']] = ["CHEMIN DE LA PAIX"]
    df.loc[(df.strname == "RTE DU JORAT") & (
        df.npa == 1027), ['strname', 'npa']] = ["ROUTE DU JORAT", 1000]
    df.loc[((df.strname == "CH DE BOISSONNET") | (df.strname == "CH. DE BOISSONNET")) & (
        df.ville == "Lausanne"), ['strname', 'npa']] = ["CHEMIN LOUIS-BOISSONNET", 1010]
    df.loc[(df.strname.str.contains("DE RUMINE")) & (
        df.npa == 1005), ['strname']] = ["AVENUE GABRIEL-DE-RUMINE"]
    df.loc[(df.strname == "CH DE RIANT PRE") & (
        df.npa == 1010), ['strname']] = ["CHEMIN RIANT-PRE"]
    df.loc[(df.strname == "CIGALE") & (
        df.npa == 1018), ['strname', 'npa']] = ["CHEMIN DE LA CIGALE", 1010]
    df.loc[(df.strname == "AV. DE RECORDON") & (
        df.npa == 1006), ['strname', 'npa']] = ["AVENUE FREDERIC-RECORDON", 1004]
    df.loc[(df.strname == "A CH. PRELAZ") & (
        df.npa == 1004), ['strname']] = ["JARDINS-DE-PRELAZ"]
    df.loc[(df.strname == "CH. DU BOIS-GENTIL") & (
        df.npa == 1004), ['strname', 'npa']] = ["CHEMIN DU BOIS-GENTIL", 1018]
    df.loc[(df.strname == 'AVENUE DE COUR') & (
        df.npa == 1012), ['npa']] = [1007]
    df.loc[(df.strname == "BD. DE LA FORET") & (
        df.npa == 1009), ['strname']] = ["BOULEVARD DE LA FORET"]
    df.loc[(df.strname == "CH MARJOVET") & (
        df.npa == 1040), ['strname']] = ["CHEMIN DE MARJOVET"]
    df.loc[(df.strname == "RUE ST. CLAIRE") & (
        df.npa == 1350), ['strname']] = ["RUE SAINTE-CLAIRE"]
    df.loc[(df.strname == "CH DES MAYORESSES") & (
        df.npa == 1007), ['strname', 'npa']] = ["CHEMIN DES MAYORESSES", 1012]
    df.loc[(df.strname == "AV. DE LAVAUX") & (
        df.npa == 1003), ['npa', 'ville']] = [1009, "Pully"]
    df.loc[(df.strname.str.contains("GRAND RUE")) & (
        df.npa == 1095), ['strname']] = ["GRAND-RUE"]
    df.loc[(df.strname.str.contains("AV. DE PRAZ")) & (
        df.npa == 1800), ['strname']] = ["AVENUE DE PRA"]
    df.loc[((df.strname == "RUE ST ROCH") | (df.strname == "RUE ST.-ROCH")) & (
        df.npa == 1004), ['strname']] = ["RUE SAINT-ROCH"]
    df.loc[(df.strname.str.contains("AV.DU LEMAN")) & (
        df.npa == 1005), ['strname']] = ["AVENUE DU LEMAN"]
    df.loc[(df.strname.str.contains("AV.DES BOVERESSES")) & (
        df.npa == 1010), ['strname']] = ["AVENUE DES BOVERESSES"]
    df.loc[(df.strname == "AV. DU GREY") & (
        df.npa == 1018), ['strname', 'npa']] = ['AVENUE DU GREY', 1004]
    df.loc[(df.strname == "RUE ST MARTIN") & (
        df.npa == 1003), ['strname']] = ["RUE SAINT-MARTIN"]
    df.loc[(df.strname == "CH D'ENTREBOIS") & (
        df.npa == 1018), ['strname']] = ["CHEMIN D'ENTRE-BOIS"]
    df.loc[(df.strname == "CH DE LA COLLIE") & (
        df.npa == 1007), ['strname']] = ["CHEMIN DE LA COLLINE"]
    df.loc[(df.strname == "TRABADAN") & (
        df.npa == 1006), ['strname']] = ["CHEMIN DU TRABANDAN"]
    df.loc[(df.strname == "AV.DU TEMPLE") & (
        df.npa == 1012), ['strname']] = ["AVENUE DU TEMPLE"]
    df.loc[(df.strname == "AV.DU MONT D'OR") & (
        df.npa == 1007), ['strname']] = ["AVENUE DU MONT-D'OR"]
    df.loc[(df.strname == "STE MARIE") & (
        df.npa == 1033), ['strname']] = ["CHEMIN DE SAINTE-MARIE"]
    df.loc[(df.strname == "LOUIS DE SAVOIE") & (
        df.npa == 1110), ['strname']] = ["RUE LOUIS-DE-SAVOIE"]
    df.loc[(df.strname == "MARIONNETTES") & (
        df.npa == 1095), ['strname', 'npa', 'ville']] = ["CHEMIN DES MARIONNETTES", 1093, "La Conversion"]
    df.loc[(df.strname == "ST GEORGES") & (
        df.npa == 1020), ['strname']] = ["CHEMIN DE SAINT-GEORGES"]

    if period_name == "f1":  # Look at F2 (same locality) / Handle postal code

        df.loc[df.pt == 6595, ['strname', 'deinr']] = [
            "CHEMIN DE LA CHAVANNAZ", "3"]
        df.loc[df.pt == 33, ['strname', 'deinr']] = [
            "ROUTE DE DIZY", "1"]
        df.loc[df.pt == 33, ['strname', 'deinr']] = [
            "ROUTE DE DIZY", "1"]
        df.loc[df.pt == 1288, ['strname', 'deinr']] = [
            "IMPASSE DE LA SAITORAZ", "1"]
        df.loc[df.pt == 4634, ['strname', 'deinr']] = [
            "CHEMIN DU TALENT", "8"]
        df.loc[df.pt == 4790, ['strname', 'deinr']] = [
            "AVENUE HENRI-ANTOINE-DE-CROUSAZ", "2b"]
        df.loc[df.pt == 4796, ['strname', 'deinr']] = [
            "ROUTE DE BOSSONNENS", "1"]
        df.loc[df.pt == 6154, ['strname', 'deinr']] = [
            "ROUTE DU PRE", "7"]
        df.loc[df.pt == 25, ['strname']] = [
            "CHEMIN DU DROUTZAY"]
        df.loc[df.pt == 3019, ['strname']] = [
            "CHEMIN DE PREPLAN"]
        df.loc[df.pt == 3041, ['strname']] = [
            "ROUTE DE CHAVANNES"]
        df.loc[df.pt == 3460, ['strname', 'deinr', 'npa']] = [
            "ROUTE DE CHAVANNES", "209", 1007]

    if period_name == "f2":  # Look at F3 (same locality) / Handle postal code

        df.loc[df.pt == 310, ['strname', 'npa']] = [
            "AVENUE SAMUEL-AUGUSTE-ANDRE-DAVID-TISSOT", 1006]
        df.loc[df.pt == 651, ['strname', 'npa']] = [
            "AVENUE DU 24-JANVIER", 1020]
        df.loc[df.pt == 6030, 'npa'] = 1920
        df.loc[df.pt == 5888, ["strname", 'deinr']] = [
            "AVENUE DU MONT-D'OR", "42"]
        df.loc[df.pt == 16, 'npa'] = 1010
        df.loc[df.pt == 810, ['strname', 'npa']] = [
            "CHEMIN DE MEILLERIE", 1006]
        df.loc[df.pt == 944, 'strname'] = "CHEMIN DU CLOSEL"
        df.loc[df.pt == 3957, ['npa', 'ville']] = [
            1674, "Montet (Glâne)"]
        df.loc[df.pt == 4343, 'npa'] = 1004
        df.loc[df.pt == 4345, 'npa'] = 1854
        df.loc[df.pt == 4953, ['strname', 'npa']] = [
            "CHEMIN DE LA MORAINE", 1162]
        df.loc[df.pt == 6585, ['strname', 'npa']] = [
            "CHEMIN DES CHESAUX", 1053]
        df.loc[df.pt == 6624, ['strname', 'npa']] = [
            "CHEMIN DES SAUGES", 1018]
        df.loc[df.pt == 9615, ['strname', 'npa']] = [
            "CHEMIN ANTOINE-DE-CHANDIEU", 1006]
        df.loc[df.pt == 1080, ['strname', 'npa']] = [
            "RUE LOUIS-DE-SAVOIE", 1110]
        df.loc[df.pt == 95, 'deinr'] = '19'
        df.loc[df.pt == 243, 'deinr'] = '8'
        df.loc[df.pt == 1575, ['strname', 'deinr']] = [
            'CHEMIN DE LA VALLOMBREUSE', '32.2']
        df.loc[df.pt == 2618, 'deinr'] = '17'
        df.loc[df.pt == 4869, 'deinr'] = '42'
        df.loc[df.pt == 5260, 'deinr'] = '16'
        df.loc[df.pt.isin([353, 459, 532, 897, 908, 1654, 1941,
                          2537, 2717, 3477, 5671, 6001, 6457]), 'npa'] = 1018

    else:
        pass

    return df


if __name__ == "__main__":
    main()
