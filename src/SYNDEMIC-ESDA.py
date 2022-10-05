from libpysal.weights import Kernel
import pandas as pd
import geopandas as gpd
import sys
import os
import geoplot as gplt
import matplotlib.pyplot as plt

project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)/GEOSAN-syndemic/"

# IMPORT DATA
colaus_outcomes = gpd.read_file(os.sep.join(
    [project_dir, "processed_data/prev_reli_colaus_outcomes.gpkg"]), driver='GPKG')


colaus_outcomes.dtypes
colaus_outcomes.crs

fig, ax = plt.subplots(figsize=(10, 10))
gplt.choropleth(colaus_outcomes, hue='cvd_all_logit', ax=ax)
fig.show()
