'''This code combines outcomes + covariates for Follow-up 2 and upload the study dataset in GEOSAN DB'''

import pandas as pd
import geopandas as gpd
import sys
import os

sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db  # commit 8670e15f12e07d81b8b00d3e5067e78e2c734ccc
    import basic_utils as bu  # commit 8670e15f12e07d81b8b00d3e5067e78e2c734ccc
except FileNotFoundError:
    print("Wrong file or file path")


project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic"


def main():

    # REDIRECT STDOUT INTO TEXT FILE
    output_print_file = os.sep.join(
        [project_dir, "results", "code_outputs/create_study_dataset.txt"]
    )
    sys.stdout = open(output_print_file, "w")

    outcomes = gpd.read_file(
        os.sep.join(
            [
                project_dir,
                "processed_data/f2_outcomes.gpkg",
            ]
        ),
        driver="GPKG")

    covariates = gpd.read_file(
        os.sep.join(
            [
                project_dir,
                "processed_data/f2_indiv_covariates.gpkg",
            ]
        ),
        driver="GPKG")
    # Keep only participants that are in outcomes
    covariates = covariates[covariates.pt.isin(outcomes.pt)]

    baseline = gpd.read_file(os.sep.join(
        [project_dir, "processed_data/b_indiv_covariates.gpkg", ]), driver="GPKG")

    # Add other residential attributes
    engine, conn, cursor = db.connect_db("geosan", "aladoy")
    res_info = pd.read_sql(
        "SELECT pt, has_moved_dist, reli FROM geochronic.f2_geo_vaud", conn)

    # Extract initial size of F2 dataset
    cursor.execute("SELECT COUNT(*) FROM geochronic.colaus_f2")
    size = cursor.fetchone()[0]

    print()
    print('DROP MISSING OUTCOMES')

    cleaned_outcome = drop_missing_val(outcomes, size, list(outcomes)[1])
    for out in list(outcomes)[2:-1]:
        cleaned_outcome = drop_missing_val(cleaned_outcome, size, out)
    outcomes = outcomes[outcomes.pt.isin(cleaned_outcome.pt)]

    print()
    print('DROP MISSING COVARIATES')

    # If missing, use baseline info. Remove participant if still NaN.
    print('Number of missing covariates in Follow-up 2')
    print(covariates.isna().sum())

    imputed_cov = covariates.set_index(
        'pt').fillna(baseline.set_index('pt'))

    print('Number of missing covariates after imputing with baseline data')
    imputed_cov.isna().sum()
    imputed_cov.reset_index(drop=False, inplace=True)

    cleaned_cov = drop_missing_val(imputed_cov, size, list(covariates)[2])
    for cov in list(covariates)[3:-1]:
        cleaned_cov = drop_missing_val(cleaned_cov, size, cov)
    imputed_cov = imputed_cov[imputed_cov.pt.isin(cleaned_cov.pt)]

    print()
    print('ADD RELI AND RESIDENTIAL HISTORY')

    # Gather all the information to create the study dataset
    full_df = merge_dfs(outcomes, res_info)
    full_df = merge_dfs(full_df, imputed_cov.drop('geometry', axis=1))

    # Convert & save
    full_gdf = gpd.GeoDataFrame(full_df, crs="EPSG:2056", geometry='geometry')
    f2_study_dataset = full_gdf[['pt', 'f2datexam'] + list(outcomes)[1:-1] + list(
        covariates)[2:-1] + ['reli', 'has_moved_dist', 'geometry']]

    if len(list(full_df)) != len(list(f2_study_dataset)):
        raise Exception("Columns are missing")

    print()
    print('SAVE')

    db.import_data('geosan', 'aladoy', f2_study_dataset, 'f2_study_dataset', pk='pt',
                   schema='geochronic', idx_geom=True, ifexists='replace')
    bu.save_gdf(os.sep.join([project_dir, "processed_data"]),
                'f2_study_dataset.gpkg',  f2_study_dataset, driver="GPKG")

    sys.stdout.close()


def drop_missing_val(data, n, attr):

    nb_na = data[data[attr].isna()].shape[0]
    print("Missing values for " + attr + ": " + str(nb_na) +
          " ( " + str(round(100*nb_na/n, 2)) + " %)")
    res = data.dropna(subset=[attr])

    return res


def merge_dfs(df1, df2):

    res_df = pd.merge(df1, df2, on='pt', how='inner')

    if res_df.shape[0] != df1.shape[0]:
        print("Number of rows dropped in the merging process: " +
              str(abs(res_df.shape[0] - df1.shape[0])))

    return res_df


if __name__ == "__main__":
    main()
