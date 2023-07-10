'''This code computes a Geographically Weighted Regression'''

import pandas as pd
import geopandas as gpd
from mgwr.gwr import GWR, MGWR
from mgwr.sel_bw import Sel_BW
import sys
import os
import numpy as np
from libpysal.weights import min_threshold_distance, Kernel
import libpysal as ps
import matplotlib.pyplot as plt
import multiprocessing as mp
import time
from mgwr.utils import compare_surfaces
from sklearn.linear_model import LinearRegression
import matplotlib.cm as cm

sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
except FileNotFoundError:
    print("Wrong file or file path")

project_dir: str = r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/"


def main():

    engine, conn, cursor = db.connect_db("geosan", "aladoy")

    # Parallel processing (MGWR)
    n_proc = 12
    pool = mp.Pool(n_proc)

    # REDIRECT STDOUT INTO TEXT FILE
    output_print_file = os.sep.join(
        [project_dir, "results", "code_outputs/mgwr.txt"]
    )
    sys.stdout = open(output_print_file, "w")

    # Contextual factors
    ha = pd.read_sql(
        "SELECT reli, intden, green_sp, noise, pm25, no2, medrev, r_unemp, r_nn_pobl, r_nn_ch FROM geochronic.ha_characteristics", conn)
    ha.columns = [
        col.upper() if col != "reli" else col for col in ha.columns]

    # Health outcomes
    data = gpd.read_file(os.sep.join(
        [project_dir, "processed_data/f2_adjusted_outcomes.gpkg"]), driver="GPKG")
    data["coordx"], data["coordy"] = data.geometry.x, data.geometry.y

    # Merge outcomes with contextual factors
    data = data.merge(ha, how='inner', on='reli')

    # GLOBAL PARAMETERS

    env_vars = ["GREEN_SP", "NOISE", "PM25",
                "MEDREV", "R_UNEMP", "R_NN_POBL", "R_NN_CH"]

    print("Variables")
    print("XO:Intercept \nX1:GREEN_SP \nX2:NOISE \nX3:PM25 \nX4:MEDREV \nX5:R_UNEMP \nX6:R_NN_POBL \nX7:R_NN_CH")
    # Standardize variables
    #  Health adjusted ouctomes are already standardized as we take the Pearson residuals of the logistic regression
    data[env_vars] = to_zscore(data[env_vars])

    # compute_spatial_regression(data, "hypertension", env_vars, pool)
    # compute_spatial_regression(data, "obesity", env_vars, pool)
    # compute_spatial_regression(data, "diabetes", env_vars, pool)
    # compute_spatial_regression(data, "dyslipidemia", env_vars, pool)
    
    # # hypertension
    # data = gpd.read_file("processed_data/hypertension_mgwr.gpkg")
    # gwr_bw = 3676
    # mgwr_bw = [489, 3690, 3618, 3687, 3689, 3689, 3690, 3690]
    # map_gwr_mgwr(env_vars, data, gwr_bw, mgwr_bw, "hypertension")

    # obesity
    # data = gpd.read_file("processed_data/obesity_mgwr.gpkg")
    # gwr_bw = 1984
    # mgwr_bw = [3405, 760, 1580, 3405, 2792, 3335, 1265, 1586]
    # map_gwr_mgwr(env_vars, data, gwr_bw, mgwr_bw, "obesity")

    # # diabetes
    # data = gpd.read_file("processed_data/diabetes_mgwr.gpkg")
    # gwr_bw = 3454
    # mgwr_bw = [3454, 3454, 3454, 2111, 607, 3454, 3454, 3454]
    # map_gwr_mgwr(env_vars, data, gwr_bw, mgwr_bw, "diabetes")

    # dyslipidemia
    # data = gpd.read_file("processed_data/dyslipidemia_mgwr.gpkg")
    # gwr_bw = 3374
    # mgwr_bw = [3399, 3399, 1613, 3399, 3334, 2941, 3399, 3399]
    # map_gwr_mgwr(env_vars, data, gwr_bw, mgwr_bw, "dyslipidemia")
    


def compute_spatial_regression(data, outcome, vars_list, pool):
    
    print()
    print(outcome.upper())
    print()

    outcome_name = outcome + "_adj"
    print("Outcome name: " + outcome_name)
    print()

    data = data[~data[outcome_name].isna()]
    print("Number of events: " + str(data.shape[0]))

    y, X, coords = prepare_inputs_gwr(data, outcome_name, vars_list)

    # GWR: adaptive bandwidth with optimized bandwidth
    print()
    print("GWR")
    print()

    gwr_selector, gwr_bw = search_bandwidth(
        coords, y, X, type="adaptive", pool=pool)
    gwr_results = run_model(coords, y, X, gwr_bw)
    #print(gwr_results.summary())
    data = add_gwr_results(data, gwr_results, vars_list, type="gwr")

    # MGWR: multiscale adaptive bandwidth with optimized bandwidth
    print()
    print("MGWR")
    print()
    mgwr_selector, mgwr_bw = search_bandwidth(coords, y, X,
                                              type="multiscale", pool=pool)
    mgwr_results = run_model(coords, y, X,
                             mgwr=True, sel_mgwr=mgwr_selector, pool=pool)
    data = add_gwr_results(data, mgwr_results, vars_list, type="mgwr")

    data["gwr_GREEN_SP_TC"]
    map_gwr_mgwr(vars_list, data, gwr_bw, mgwr_bw, outcome)

    # Save results
    data.to_file(f"processed_data/{outcome}_mgwr.gpkg", driver="GPKG")


def map_gwr_mgwr(vars_list, df, gwr_bw, mgwr_bw, disease):

    kwargs1 = {'edgecolor': 'grey', 'alpha': .65}
    kwargs2 = {'edgecolor': 'grey', 'alpha': 1}

    vars_list.insert(0, "intercept")
    print(vars_list)

    # Filter for T (0.05)
    for i, var in enumerate(vars_list):
        cm.unregister_cmap(name='shiftedcmap')
        compare_surfaces(df, f"gwr_{var}", f"mgwr_{var}", getattr(df, f"gwr_{var}_T"), gwr_bw,
                         getattr(df, f"mgwr_{var}_T"), mgwr_bw[i], var, kwargs1, kwargs2, savefig=f"results/regression_models/{disease}/mgwr_{disease}_{var}_T")
    
    # Filter for T with correction
    for i, var in enumerate(vars_list):
        cm.unregister_cmap(name='shiftedcmap')
        compare_surfaces(df, f"gwr_{var}", f"mgwr_{var}", getattr(df, f"gwr_{var}_TC"), gwr_bw,
                         getattr(df, f"mgwr_{var}_TC"), mgwr_bw[i], var, kwargs1, kwargs2, savefig=f"results/regression_models/{disease}/mgwr_{disease}_{var}_TC")

    vars_list.remove("intercept")


def add_gwr_results(df, gwr_results, vars_list, type="gwr"):
    # type=gwr or mgwr

    # Extract results for intercept
    df[f'{type}_intercept'] = gwr_results.params[:, 0]
    df[f"{type}_intercept_T"], df[f"{type}_intercept_TC"] = filter_for_inference(gwr_results,
                                                                                 type, 0)

    # Extract results for each covariate
    for i, var in enumerate(vars_list):

        df[f'{type}_{var}'] = gwr_results.params[:, i + 1]
        df[f"{type}_{var}_T"], df[f"{type}_{var}_TC"] = filter_for_inference(gwr_results,
                                                                             type, i+1)

    # Extract measures of local collinearity
    lc = gwr_results.local_collinearity()

    if type == "gwr":
        df[f"{type}_CN"] = lc[2]
        df[f"{type}_R2"] = gwr_results.localR2

    else:
        df[f"{type}_CN"] = lc[0]

    return df


def filter_for_inference(results, type, index):

    # without correction
    filter_t = results.filter_tvals(alpha=0.05)[:, index]

    # with correction for multiple comparisons
    filter_tc = results.filter_tvals()[:, index]

    return filter_t, filter_tc


def run_model(coords, y, X, bw_gwr=None, mgwr=False, pool=None, sel_mgwr=None):
    # Defaults GWR(): kernel="bisquare", spherical=False, fixed=False

    if mgwr is True:
        start = time.time()
        model_results = MGWR(coords, y, X, sel_mgwr).fit(pool=pool)
        end = time.time()
        print("Elapsed time: " + str(round((end-start)/60, 2)) + " minutes.")

    else:
        model_results = GWR(coords, y, X, bw_gwr).fit(pool=pool)

    print(model_results.summary())

    return model_results


def search_bandwidth(coords, y, X, type="adaptive", pool=None):
    # type=fixed, adaptive, multiscale
    # Defaults Sel_BW(): kernel="bisquare", spherical=False
    # Defaults selector.search(): search_method='golden_section', criterion='AICc'

    start = time.time()

    if type == "fixed":
        selector = Sel_BW(coords, y, X, fixed=True, multi=False)
    elif type == "adaptive":
        selector = Sel_BW(coords, y, X, fixed=False, multi=False)
    elif type == "multiscale":
        selector = Sel_BW(coords, y, X, fixed=False, multi=True)
    else:
        raise ValueError("Type must be either fixed, adaptive, or multiscale")

    bw = selector.search(pool=pool)
    end = time.time()
    print("Elapsed time: " + str(round((end-start)/60, 2)) + " minutes.")

    return selector, bw


def to_zscore(x):

    z = (x - np.mean(x, axis=0)) / np.std(x, axis=0)

    return z


def prepare_inputs_gwr(df, outcome, vars):

    y = df[outcome].values.reshape((-1, 1))
    X = df[vars].values
    coords = list(zip(df.coordx, df.coordy))

    return y, X, coords


if __name__ == "__main__":
    main()


# create_nice_plot(diab_data, "R_NN_POBL")


# def create_nice_plot(data, var):
#     fig, ax = plt.subplots(1, 3, figsize=(15, 4))

#     # Set common plot settings
#     common_settings = {
#         'edgecolor': 'none',
#         'linewidth': 0.5,
#         'markersize': 9
#     }

#     # Plot 1: Parameter estimates
#     data.plot(var,
#               ax=ax[0],
#               legend=False,
#               **common_settings)
#     ax[0].set_title('Parameter estimates')

#     # Plot 2: Composite
#     data.plot(var,
#               ax=ax[1],
#               legend=False,
#               **common_settings)
#     data[data[var+'_T'] == 0].plot(color='grey',
#                                    alpha=0.65,
#                                    ax=ax[1],
#                                    **common_settings)
#     ax[1].set_title('Composite')

#     # Plot 3: Composite with correction
#     data.plot(var,
#               ax=ax[2],
#               legend=True,
#               **common_settings)
#     data[data[var+'_TC'] == 0].plot(color='grey',
#                                     alpha=0.65,
#                                     ax=ax[2],
#                                     **common_settings)
#     ax[2].set_title('Composite with correction')

#     # Adjust plot layout
#     plt.tight_layout()

#     # Save and display the plot
#     plt.savefig('testing.png')
#     plt.show()