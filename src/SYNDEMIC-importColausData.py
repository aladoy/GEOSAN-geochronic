'''
For each CoLaus dataset (Baseline, F1, F2, F3), the code 
retrieves the individual's addresses and import the GeoDataFrame
in GEOSAN DB (schema=syndemic).
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


project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)/GEOSAN-syndemic/"
data_dir: str = r"/mnt/data/GEOSAN/GEOSAN DB/data/COLAUS"

# BASELINE

# File sent by Pedro that contains the covariates required for the study (but no geo information)
b = pd.read_csv(os.sep.join([data_dir, "request_072022/Ladoy1.csv"]), delimiter=';')
print('Number of rows: ', b.shape[0])
b['datexam'] = pd.to_datetime(b.datexam, format='%d%b%Y')
b['datarrival'] = pd.to_datetime(b.datarrival, format='%d%b%Y')

# File containing the geocoded data (geocoding process done by Marco Viera.)
b_geo = pd.read_csv(os.sep.join([data_dir, "geodata_marco/BASELINE/F0_encod_mapgeo.csv"]), delimiter=',')
b_geo.columns = ['pt', 'strname', 'deinr', 'plz4', 'municipality', 'coordx_21781', 'coordy_21781']

# File containing the initial addresses (not modified during the geocoding process)
b_init = pd.read_csv(os.sep.join([data_dir, "geodata_marco/initial_file_address/AdressesF0.csv"]), delimiter=',')
b_init = b_init[['uid','street','number','zipcode']]
b_init.columns = ['pt', 'strname_init', 'deinr_init', 'plz4_init']

# Merge dataframes
b_geo_init = b_geo.merge(b_init, how='right', on='pt')
if not b_geo_init.shape[0] == b_init.shape[0] == b_geo.shape[0]:
    raise Warning('This is not a perfect match. Some rows are missing.')

b_df = b.merge(b_geo_init, how='left', on='pt')
if not b.shape[0] == b_geo.shape[0] == b_df.shape[0]:
    print('Number of individuals without geometry: ', b.shape[0]-b_geo.shape[0])
    raise Warning('This is not a perfect match. Some rows will not have a geometry.')

# Convert to geodataframe
b_df['geometry'] = b_df.apply(lambda row: Point(row.coordx_21781, row.coordy_21781), axis=1)
b_geo = gpd.GeoDataFrame(b_df, geometry=b_df.geometry, crs="epsg:21781")
b_geo = b_geo.to_crs(2056)

# Insert to database
b_geo = b_geo.astype({'brnsws': 'Int64', 'edtyp3': 'Int64', 'edtyp4': 'Int64','mrtsts2': 'Int64', 'job_curr8': 'Int64', 'cvdbase': 'Int64', 'cvdbase_adj': 'Int64', 'sbsmk': 'Int64', 'HTA': 'Int64', 'hctld': 'Int64', 'dbtld': 'Int64', 'DIAB': 'Int64', 'phyact': 'Int64', 'plz4': 'Int64', 'plz4_init': 'Int64'})
db.import_data('geosan', 'aladoy', b_geo, 'colaus_b', pk = 'NULL', schema='syndemic', idx_geom=True, ifexists='append')


# FOLLOW-UP 1

# File sent by Pedro that contains the covariates required for the study (but no geo information)
f1 = pd.read_csv(os.sep.join([data_dir, "request_072022/Ladoy2.csv"]), delimiter=';')
f1['F1datexam'] = pd.to_datetime(f1.F1datexam, format='%d%b%Y')

# File containing the geocoded data (geocoding process done by Marco Viera.)
f1_geo = pd.read_csv(os.sep.join([data_dir, "geodata_marco/F1/F1_encod_mapgeo.csv"]), delimiter=',', encoding='iso-8859-1')
f1_geo.columns = ['pt', 'strname', 'deinr', 'plz4', 'municipality', 'coordx_21781', 'coordy_21781']

# File containing the initial addresses (not modified during the geocoding process)
f1_init = pd.read_excel(os.sep.join([data_dir, "geodata_marco/initial_file_address/AdressesFU1.xlsx"]))
f1_init = f1_init[['F1Code','Rue','No','CP']]
f1_init.columns = ['pt', 'strname_init', 'deinr_init', 'plz4_init']

# Merge dataframes
f1_geo_init = f1_geo.merge(f1_init, how='right', on='pt')
if not f1_geo_init.shape[0] == f1_init.shape[0] == f1_geo.shape[0]:
    raise Warning('This is not a perfect match. Some rows are missing.')
f1_df = f1.merge(f1_geo_init, how='left', on='pt')
if not f1.shape[0] == f1_geo.shape[0] == f1_df.shape[0]:
    print('Number of individuals without geometry: ', f1.shape[0]-f1_geo.shape[0])
    raise Warning('This is not a perfect match. Some rows will not have a geometry.')

# Convert to geodataframe
f1_df['geometry'] = f1_df.apply(lambda row: Point(row.coordx_21781, row.coordy_21781), axis=1)
f1_geo = gpd.GeoDataFrame(f1_df, geometry=f1_df.geometry, crs="epsg:21781")
f1_geo = f1_geo.to_crs(2056)

# Insert to database
f1_geo = f1_geo.astype({'F1mrtsts2': 'Int64', 'F1job_curr8': 'Int64', 'F1sbsmk': 'Int64', 'F1HTA': 'Int64', 'F1hctld': 'Int64', 'F1dbtld': 'Int64', 'F1DIAB': 'Int64', 'F1CVD': 'Int64', 'plz4': 'Int64', 'plz4_init': 'Int64'})
db.import_data('geosan', 'aladoy', f1_geo, 'colaus_f1', pk = 'NULL', schema='syndemic', idx_geom=True, ifexists='append')



# FOLLOW-UP 2

# File sent by Pedro that contains the covariates required for the study (but no geo information)
f2 = pd.read_csv(os.sep.join([data_dir, "request_072022/Ladoy3.csv"]), delimiter=';')
f2['F2datexam'] = pd.to_datetime(f2.F2datexam, format='%d%b%Y')
f2['F2datquest'] = pd.to_datetime(f2.F2datquest, format='%d%b%Y')

# File containing the geocoded data (geocoding process done by Marco Viera.)
f2_geo = pd.read_csv(os.sep.join([data_dir, "geodata_marco/F2/F2_encod_mapgeo.csv"]), delimiter=',', encoding='iso-8859-1')
f2_geo.columns = ['pt', 'strname', 'deinr', 'plz4', 'municipality', 'coordx_21781', 'coordy_21781']

# File containing the initial addresses (not modified during the geocoding process)
f2_init = pd.read_csv(os.sep.join([data_dir, "geodata_marco/initial_file_address/AdressesFU2.csv"]), delimiter=';')
f2_init = f2_init[['pt','Rue','No','CP']]
f2_init.columns = ['pt', 'strname_init', 'deinr_init', 'plz4_init']

# Merge dataframes
f2_geo_init = f2_geo.merge(f2_init, how='right', on='pt')
if not f2_geo_init.shape[0] == f2_init.shape[0] == f2_geo.shape[0]:
    raise Warning('This is not a perfect match. Some rows are missing.')
f2_df = f2.merge(f2_geo_init, how='left', on='pt')
if not f2.shape[0] == f2_geo.shape[0] == f2_df.shape[0]:
    print('Number of individuals without geometry: ', f2.shape[0]-f2_geo.shape[0])
    raise Warning('This is not a perfect match. Some rows will not have a geometry.')

# Convert to geodataframe
f2_df['geometry'] = f2_df.apply(lambda row: Point(row.coordx_21781, row.coordy_21781), axis=1)
f2_geo = gpd.GeoDataFrame(f2_df, geometry=f2_df.geometry, crs="epsg:21781")
f2_geo = f2_geo.to_crs(2056)

# Insert to database
for column in ['F2sex', 'F2mrtsts', 'F2mrtsts2', 'F2dmst', 'F2nochd', 'F2sclhlp1', 'F2sclhlp2', 'F2job_curr1', 'F2job_curr4', 'F2job_curr7', 'F2job_not1', 'F2job_prev4', 'F2family18', 'F2income1', 'F2income2', 'F2income3', 'F2income4', 'F2income5', 'F2income6', 'F2Quest1', 'F2conso_hebdo', 'F2alcool2', 'F2sbsmk', 'F2hypdr', 'F2crbpmed', 'F2HTA', 'F2hctld', 'F2dbtld', 'F2DIAB', 'F2care1', 'F2care1b', 'F2care2', 'F2seden', 'F2BMI_cat2', 'F2waist_cat', 'Polypharm', 'F2CVD', 'plz4'] :
    f2_geo[column] = f2_geo[column].astype('Int64')
db.import_data('geosan', 'aladoy', f2_geo, 'colaus_f2', pk = 'NULL', schema='syndemic', idx_geom=True, ifexists='append')



# FOLLOW-UP 3

# File sent by Pedro that contains the covariates required for the study (but no geo information)
f3 = pd.read_csv(os.sep.join([data_dir, "request_072022/Ladoy4.csv"]), delimiter=';')
f3['F3datexam'] = pd.to_datetime(f3.F3datexam, format='%d%b%Y')
f3['F3datquest'] = pd.to_datetime(f3.F3datquest, format='%d%b%Y')

# File containing the geocoded data (geocoding process done by Marco Viera.)
f3_geo = pd.read_csv(os.sep.join([data_dir, "geodata_marco/F3/20220310_F3_encod_mapgeo_tot_21781_complete.csv"]), delimiter=',', encoding='iso-8859-1')
f3_geo = f3_geo[['pt','street','number','zipcode','Ville','Y_F2','X_F2']] # x and y were inverted
f3_geo.columns = ['pt', 'strname', 'deinr', 'plz4', 'municipality', 'coordx_21781', 'coordy_21781']

# File containing the initial addresses (not modified during the geocoding process)
f3_init = pd.read_excel(os.sep.join([data_dir, "geodata_marco/initial_file_address/AdressesFU3.xlsx"]))
f3_init = f3_init[['F3pt','Rue','No','CP']]
f3_init.columns = ['pt', 'strname_init', 'deinr_init', 'plz4_init']

# Merge dataframes
f3_geo_init = f3_geo.merge(f3_init, how='right', on='pt')
if not f3_geo_init.shape[0] == f3_init.shape[0] == f3_geo.shape[0]:
    raise Warning('This is not a perfect match. Some rows are missing.')
f3_df = f3.merge(f3_geo_init, how='left', on='pt')
if not f3.shape[0] == f3_geo.shape[0] == f3_df.shape[0]:
    print('Number of individuals without geometry: ', f3.shape[0]-f3_geo.shape[0])
    raise Warning('This is not a perfect match. Some rows will not have a geometry.')

# Convert to geodataframe
f3_df['geometry'] = f3_df.apply(lambda row: Point(row.coordx_21781, row.coordy_21781), axis=1)
f3_geo = gpd.GeoDataFrame(f3_df, geometry=f3_df.geometry, crs="epsg:21781")
f3_geo = f3_geo.to_crs(2056)

# Insert to database
for column in ['F3mrtsts', 'F3mrtsts2', 'F3dmst', 'F3nochd', 'F3sclhlp3', 'F3job_curr1', 'F3job_curr4', 'F3job_curr7', 'F3job_not2', 'F3job_prev3', 'F3income3', 'F3income4', 'F3income5', 'F3income6', 'F3assur1', 'F3Quest1', 'F3conso_hebdo', 'F3alcool2', 'F3sbsmk', 'F3hypdr', 'F3crbpmed', 'F3HTA', 'F3hctld', 'F3dbtld', 'F3DIAB', 'F3care1', 'F3care1a', 'F3care1b', 'F3care3', 'F3care3a', 'F3care4', 'F3care4a', 'F3IPAQ_excl', 'F3IPAQ_score', 'F3BMI_cat2', 'F3waist_cat', 'Polypharm', 'F3CVD', 'plz4'] :
    f3_geo[column] = f3_geo[column].astype('Int64')
db.import_data('geosan', 'aladoy', f3_geo, 'colaus_f3', pk = 'NULL', schema='syndemic', idx_geom=True, ifexists='append')
