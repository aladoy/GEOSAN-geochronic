'''
The code extracts the Geocovid dataset from GEOSAN DB and filter 
data for multiple testing. 
Same code than the one used for in GEOCOVID-phase 2 (GEOCOVID-processMultipleDemands.py)
'''

import pandas as pd
import geopandas as gpd
import sys
from matplotlib import pyplot as plt
import os
import numpy as np
import geofeather


# Import functions from GIRAPH-functions repository
sys.path.append(r'/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/')
try:
    import db_utils as db
except FileNotFoundError:
    print("Wrong file or file path")


project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)/GEOSAN-syndemic/"

# CONNECT TO GEOSAN DB - GEOCOVID SCHEMA
engine, conn, cursor = db.connect_db('geosan', 'aladoy')

# READ TABLE (ORDER BY DATE_RECEPTION)
sql = ("SELECT * "
       "FROM geocovid.s3_notfiltered_tests_geo "
       "ORDER BY date_reception ASC")
data = gpd.read_postgis(sql, conn, geom_col='geometry')
data['date_reception'] = pd.to_datetime(data.date_reception)

# PRELIMINARY ANALYSES
print('Are data ordered by reception date: ',
      data.date_reception.is_monotonic_increasing)
print('Dataset size: ', data.shape)
print('Is attribute id_demande_study2 unique? ',
      data.id_demande_study2.is_unique)
idx_dup = data[data.duplicated(
    subset='id_patient_study2', keep=False)].id_patient_study2.unique()
print('Number of individuals with several demands: ', len(idx_dup))


# FIND THE NUMBER OF DAYS INDIVIDUALS STAY POSITIVE

def cov_pos_duration(df):
    # Filter positive tests
    df = df[df.res_cov == 1]

    # Compute time difference between tests for each individual
    df = df.assign(
        time_diff=df.groupby('id_patient_study2').date_reception.apply(
            lambda x: x - x.iloc[0]))

    # Convert datetime to integer (7 days > 7)
    df['time_diff'] = df.time_diff.dt.days

    # Compute the time difference maximum for each individual
    # > duration of SARS-Cov-2 positivity
    df = pd.DataFrame(df.groupby(
        'id_patient_study2')['time_diff'].max().reset_index(drop=False))

    # For all individuals
    # Descriptive statistics
    print(('Statistics of SARS-Cov-2 positivity duration (between first and '
           'positive test), including individuals with only 1 test during the '
           'time period: '))
    print(df.time_diff.describe())
    # Frequency distribution
    n, bins, patches = plt.hist(x=df.time_diff, bins='auto', color='#0504aa',
                                alpha=0.7, rwidth=0.85)
    plt.grid(axis='y', alpha=0.75)
    plt.xlabel('Duration')
    plt.ylabel('Frequency')
    plt.title('Duration of PCR positive tests')
    plt.show()

    # Without individuals with only 1 positive test
    # Descriptive statistics
    print(('Statistics of SARS-Cov-2 positivity duration (between first and '
           'last positive test), without individuals with only 1 test during '
           'the time period: '))
    print(df[df.time_diff > 0].time_diff.describe())

    # Frequency distribution
    n, bins, patches = plt.hist(x=df[df.time_diff > 0].time_diff, bins='auto',
                                color='#0504aa', alpha=0.7, rwidth=0.85)
    plt.grid(axis='y', alpha=0.75)
    plt.xlabel('Duration')
    plt.ylabel('Frequency')
    plt.title('Duration of PCR positive tests (for positive tests > 0)')
    plt.show()

    return df.time_diff[df.time_diff > 0].mean(), df.time_diff[df.time_diff > 0].median()


# Compute positiviy duration for the whole period
pos_dur_All = cov_pos_duration(data)


# PERIOD DEFINITION (same that the ones used by David D.R.)

def add_waves(df):

    df['wave'] = np.nan
    df.loc[df.date_reception.between(
        min(df.date_reception), '2020-06-30', inclusive='both'), 'wave'] = 1
    df.loc[df.date_reception.between(
        '2020-07-01', '2020-12-15', inclusive='both'), 'wave'] = 2
    df.loc[df.date_reception.between(
        '2020-12-16', '2021-08-18', inclusive='both'), 'wave'] = 3
    df.loc[df.date_reception.between(
        '2021-08-19', max(df.date_reception), inclusive='both'), 'wave'] = 4

    return df


data = add_waves(data)


# First vague
pos_w1_mean, pos_w1_median = cov_pos_duration(data[data.wave == 1])
pos_w2_mean, pos_w2_median = cov_pos_duration(data[data.wave == 2])
pos_w3_mean, pos_w3_median = cov_pos_duration(data[data.wave == 3])
pos_w4_mean, pos_w4_median = cov_pos_duration(data[data.wave == 4])


# PROCESS MULTIPLE DEMANDS

# Function to split into x-days episodes

def split_episodes(df, set, dur, first_positif=None):

    # Isolate the episode
    start_date = df.iloc[0].date_reception

    if first_positif is None:
        # print('1')
        episode = df[df.date_reception <= (start_date+pd.DateOffset(days=dur))]
    elif (start_date+pd.DateOffset(days=dur) < first_positif):
        # print('2')
        episode = df[df.date_reception <= (start_date+pd.DateOffset(days=dur))]
    else:
        # print('3')
        episode = df[df.date_reception < first_positif]

    # If the episode contains at least one positive test, keep the first one
    # that is not NaN (if all NaN keep first test)
    try:
        # set=set.union({episode.loc[episode.viral_load==episode.viral_load.max(),'id_demande_study2'].values[0]})
        set = set.union(
            {episode[(episode.res_cov == 1) & (~episode.viral_load.isna())
                     ].iloc[0].id_demande_study2})
    except Exception:
        try:
            set = set.union({
                episode.loc[episode.res_cov == 1,
                            'id_demande_study2'].values[0]})
        except Exception:
            set = set.union({episode.iloc[0].id_demande_study2})

    df = df[~df.index.isin(episode.index)].reset_index(drop=True)
    return df, set


# Initialize variables before For loop
final_keep = set()
opt1 = set()
opt2 = set()
opt3 = set()
opt4 = set()
opt5 = set()
opt0 = set()

# Initialize period duration
usr_dur = input("Choose period duration: (nb of days)")
if usr_dur.isdigit():
    duration = int(usr_dur)
else:
    raise ValueError("Invalid input. Please use another value.")


for i in idx_dup:
    # print(i)

    # Create subset of dataframe
    df = data.loc[data.id_patient_study2 == i]

    # Reset index for the subset
    df.reset_index(drop=True, inplace=True)
    # Create a column with time difference (in days) between rows
    df['diff_time'] = df.date_reception.diff().dt.days
    # Create a column with cumulated time difference
    df['cum_time'] = df['diff_time'].cumsum()

    # If all tests are comprised in a n days interval (duration) and are all
    # negative: keep first test
    if (df.diff_time.cumsum().max() <= duration) & (df.res_cov == 0).all():
        to_keep = {df.iloc[0, 0]}
        opt1 = opt1.union({i})

    # If all tests are comprised in a n days interval (duration) and are
    # negative and positive, keep the first positive (which is not NaN)
    elif (df.diff_time.cumsum().max() <= duration):
        try:
            to_keep = {
                df[(df.res_cov == 1) & (~df.viral_load.isna())
                   ].iloc[0].id_demande_study2
            }  # First positive that has no viral load = NaN
        except Exception:
            # First positive (if all positive tests have viral load = NaN)
            to_keep = {df.loc[df.res_cov == 1, 'id_demande_study2'].values[0]}
        opt2 = opt2.union({i})

    # If all tests are separated by a n days interval (duration),
    # keep all occurences
    elif (df[~df.diff_time.isna()].diff_time > duration).all():
        to_keep = set(df.id_demande_study2)
        opt3 = opt3.union({i})

    # If several episodes, keep the first event of each n days (duration)
    elif (df[~df.diff_time.isna()].cum_time > duration).any() & (df.res_cov == 0).all():
        # Initialize empty list
        to_keep = set()
        while df.empty is False:  # while dataframe not empty
            res = split_episodes(df, to_keep, duration)
            df = res[0]
            to_keep = to_keep.union(res[1])
        opt4 = opt4.union({i})

    elif (df[~df.diff_time.isna()].cum_time > duration).any() & (df.res_cov == 1).any():

        first_pos = df[df.res_cov == 1].iloc[0].date_reception
        # print('first_pos '+str(first_pos))

        # Initialize empty list
        to_keep = set()

        # while before first positive
        while df.date_reception.iloc[0] < first_pos:
            res = split_episodes(df, to_keep, duration, first_pos)
            df = res[0]
            to_keep = to_keep.union(res[1])
            # print(to_keep)

        # while dataframe not empty
        while df.empty is False:
            res = split_episodes(df, to_keep, duration)
            df = res[0]
            to_keep = to_keep.union(res[1])
            # print(to_keep)

        opt5 = opt5.union({i})

    else:
        opt0 = opt0.union({i})

    final_keep = final_keep.union(to_keep)

print('Only one episode and tests are all negative')
len(opt1)
print('Only one episode and tests are both positive / negative or all positive')
len(opt2)
print('Several episodes but only one test for each')
len(opt3)
print('Several episodes and all tests are negative')
len(opt4)
print('Several episodes and tests are both positive / negative')
len(opt5)
print('Others - should be 0')
len(opt0)

# data[data.duplicated(subset='id_patient_study2', keep=False)
#      ].groupby('id_patient_study2').size().sort_values(ascending=False)
# data[data.duplicated(subset='id_patient_study2', keep='first')]

# CREATE FINAL DATASET
print('Number of tests before processing: ' + str(data.shape[0]))

# Extract id_demande_study2 for individuals that did only one test
single_tests = set(
    data[~data.duplicated(subset='id_patient_study2', keep=False)
         ].id_demande_study2)
print('Number of tests for individuals who were tested only once: ',
      len(single_tests))

# Concatenate with the set final_keep (algorithm output)
tests_to_keep = single_tests.union(final_keep)
print('Number of tests to keep: ', len(tests_to_keep))

# Extract full info for tests to keep with a query to GEOSAN DB
final_dataset = gpd.read_postgis(
    "SELECT * FROM geocovid.s3_notfiltered_tests_geo WHERE id_demande_study2 \
    IN {} ORDER BY date_reception ASC".format(tuple(tests_to_keep)),
    conn, geom_col='geometry')
final_dataset.shape


# Add waves
final_dataset = add_waves(final_dataset)
final_dataset['wave'] = final_dataset.wave.astype(int)

# SAVE RESULTS

# Add to GEOSAN DB
db.import_data('geosan', 'aladoy', final_dataset, 's3_20dfiltered_tests_geo',
               pk='id_demande_study2', schema='geocovid', idx_geom=True, ifexists='replace')


output_file = os.sep.join(
    [project_dir, "data/geocovid/s3_20dfiltered_tests_geo.feather"])
try:
    if os.path.exists(output_file):
        os.remove(output_file)
    geofeather.to_geofeather(final_dataset, output_file)
    print('Sucess')
except Exception:
    print('Error while saving data on disk')

# CLOSE CONNECTION TO GEOSAN DB
conn.close()
