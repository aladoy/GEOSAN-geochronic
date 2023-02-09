'''This code combines outcomes + covariates for Follow-up 2 and upload the study dataset in GEOSAN DB'''

import pandas as pd
import geopandas as gpd
import sys
import os

sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db  # commit 8670e15f12e07d81b8b00d3e5067e78e2c734ccc
except FileNotFoundError:
    print("Wrong file or file path")


def main():

    project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic"

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

    if outcomes.shape[0] == covariates.shape[0]:
        dataset = pd.merge(outcomes,
                           covariates.drop("geometry", axis=1), on='pt', how='inner')

    else:
        raise Exception("Sorry, no numbers below zero")

    db.import_data('geosan', 'aladoy', dataset, 'f2_study_dataset',
                   pk='pt', schema='geochronic', idx_geom=True, ifexists='replace')


if __name__ == "__main__":
    main()
