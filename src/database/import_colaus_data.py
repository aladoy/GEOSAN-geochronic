'''
For each CoLaus dataset (Baseline, F1, F2, F3), the code
retrieves the individual's addresses and import the GeoDataFrame
in GEOSAN DB (schema=geochronic).
'''

import pandas as pd
import geopandas as gpd
import datetime
from shapely.geometry import Point
import sys
import os


sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
except FileNotFoundError:
    print("Wrong file or file path")


project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/"
data_dir: str = r"/mnt/data/GEOSAN/GEOSAN DB/data/COLAUS"


def main():

    # REDIRECT STDOUT INTO TEXT FILE
    output_print_file = os.sep.join(
        [project_dir, "results", "code_outputs/import_colaus_data.txt"]
    )
    sys.stdout = open(output_print_file, "w")

    print('BASELINE')

    b = pd.read_csv(os.sep.join(
        [data_dir, "request_12dec22/Ladoy1.csv"]), delimiter=';')
    b_na = b[b.datexam.isna() & b.datarrival.isna()].index
    if b_na.size > 0:
        b = remove_nan(b, b_na)
    b['datexam'] = b.datexam.map(convert_date)
    b['datarrival'] = b.datarrival.map(convert_date)
    b_geo = extract_geo_dataset(0)
    b = add_geo(b, b_geo)

    # Insert to database
    b = b.astype({'brnsws': 'Int64', 'edtyp3': 'Int64', 'edtyp4': 'Int64', 'mrtsts2': 'Int64', 'job_curr8': 'Int64', 'cvdbase': 'Int64', 'cvdbase_adj': 'Int64',
                  'sbsmk': 'Int64', 'HTA': 'Int64', 'hctld': 'Int64', 'dbtld': 'Int64', 'DIAB': 'Int64', 'phyact': 'Int64'})
    db.import_data('geosan', 'aladoy', b, 'colaus_b', pk='pt',
                   schema='geochronic', idx_geom=True, ifexists='replace')

    print()
    print('FOLLOW-UP 1')

    f1 = pd.read_csv(os.sep.join(
        [data_dir, "request_12dec22/Ladoy2.csv"]), delimiter=';')
    f1_na = f1[f1.F1datexam.isna()].index
    if f1_na.size > 0:
        f1 = remove_nan(f1, f1_na)
    f1['F1datexam'] = f1.F1datexam.map(convert_date)
    f1_geo = extract_geo_dataset(1)
    f1 = add_geo(f1, f1_geo)

    # Insert to database
    f1 = f1.astype({'F1mrtsts2': 'Int64', 'F1job_curr8': 'Int64', 'F1sbsmk': 'Int64', 'F1HTA': 'Int64',
                    'F1hctld': 'Int64', 'F1dbtld': 'Int64', 'F1DIAB': 'Int64', 'F1CVD': 'Int64'})
    db.import_data('geosan', 'aladoy', f1, 'colaus_f1', pk='pt',
                   schema='geochronic', idx_geom=True, ifexists='replace')

    print()
    print('FOLLOW-UP 2')

    f2 = pd.read_csv(os.sep.join(
        [data_dir, "request_12dec22/Ladoy3.csv"]), delimiter=';')
    f2_na = f2[f2.F2datexam.isna() & f2.F2datquest.isna()].index
    if f2_na.size > 0:
        f2 = remove_nan(f2, f2_na)
    f2['F2datexam'] = f2.F2datexam.map(convert_date)
    f2['F2datquest'] = f2.F2datquest.map(convert_date)
    f2_geo = extract_geo_dataset(2)
    f2 = add_geo(f2, f2_geo)

    # Insert to database
    for column in ['F2sex', 'F2mrtsts', 'F2mrtsts2', 'F2dmst', 'F2nochd', 'F2sclhlp1', 'F2sclhlp2', 'F2job_curr1', 'F2job_curr4', 'F2job_curr7', 'F2job_not1', 'F2job_prev4', 'F2family18', 'F2income1', 'F2income2', 'F2income3', 'F2income4', 'F2income5', 'F2income6', 'F2Quest1', 'F2conso_hebdo', 'F2alcool2', 'F2sbsmk', 'F2hypdr', 'F2crbpmed', 'F2HTA', 'F2hctld', 'F2hypolip', 'F2dbtld', 'F2orldrg', 'F2insn', 'F2antiDIAB', 'F2DIAB', 'F2care1', 'F2care1b', 'F2care2', 'F2seden', 'F2BMI_cat2', 'F2waist_cat', 'Polypharm', 'F2CVD']:
        f2[column] = f2[column].astype('Int64')
    db.import_data('geosan', 'aladoy', f2, 'colaus_f2', pk='pt',
                   schema='geochronic', idx_geom=True, ifexists='replace')

    # print()
    # print('FOLLOW-UP 3')

    # f3 = pd.read_csv(os.sep.join(
    #     [data_dir, "request_12dec22/Ladoy4.csv"]), delimiter=';')
    # f3_na = f3[f3.F3datexam.isna() & f3.F3datquest.isna()].index
    # if f3_na.size > 0:
    #     f3 = remove_nan(f3, f3_na)
    # f3['F3datexam'] = f3.F3datexam.map(convert_date)
    # f3['F3datquest'] = f3.F3datquest.map(convert_date)
    # f3_geo = extract_geo_dataset(3)
    # f3_init = extract_init_dataset(3)
    # f3 = add_geo(f3, f3_init, f3_geo)
    # f3 = transform_to_gdf(f3)

    # # Insert to database
    # for column in ['F3mrtsts', 'F3mrtsts2', 'F3dmst', 'F3nochd', 'F3sclhlp3', 'F3job_curr1', 'F3job_curr4', 'F3job_curr7', 'F3job_not2', 'F3job_prev3', 'F3income3', 'F3income4', 'F3income5', 'F3income6', 'F3assur1', 'F3Quest1', 'F3conso_hebdo', 'F3alcool2', 'F3sbsmk', 'F3hypdr', 'F3crbpmed', 'F3HTA', 'F3hctld', 'F3dbtld', 'F3DIAB', 'F3care1', 'F3care1a', 'F3care1b', 'F3care3', 'F3care3a', 'F3care4', 'F3care4a', 'F3IPAQ_excl', 'F3IPAQ_score', 'F3BMI_cat2', 'F3waist_cat', 'Polypharm', 'F3CVD', 'plz4']:
    #     f3[column] = f3[column].astype('Int64')
    # db.import_data('geosan', 'aladoy', f3, 'colaus_f3', pk='NULL',
    #                schema='geochronic', idx_geom=True, ifexists='replace')

    sys.stdout.close()


def remove_nan(df, nas):

    print('Number of NaN data (which are directly dropped from the dataset): ', nas.size)
    df.drop(nas, inplace=True)

    return df


def convert_date(x):

    return pd.to_datetime(x, format='%d%b%Y')


def extract_geo_dataset(cohort_period):

    subdir = {0: 'baseline', 1: 'f1', 2: 'f2', 3: 'f3'}
    filename = subdir[cohort_period] + '_geocoded.gpkg'

    addr_geo = gpd.read_file(os.sep.join(
        [project_dir, "processed_data/geocoding", filename]))
    addr_geo = addr_geo[['pt', 'init_addr', 'matching_addr',
                         'egid', 'note_geocoding', 'gkode', 'gkodn', 'geometry']]
    return addr_geo


def add_geo(data, addr_geo):

    df = data.merge(addr_geo, how='left', on='pt')
    gdf = gpd.GeoDataFrame(df, crs="EPSG:2056", geometry='geometry')

    if not data.shape[0] == addr_geo.shape[0]:
        print('Several individuals could not be geocoded and will be inserted in DB without a geometry')
        print('Number of individuals without geometry: ',
              gdf[gdf.gkode.isna()].shape[0])

    return gdf


if __name__ == "__main__":
    main()
