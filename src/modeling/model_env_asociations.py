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
from spglm.family import Gaussian, Binomial
from statsmodels.formula.api import ols
from statsmodels.stats.outliers_influence import variance_inflation_factor
import spreg


sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import db_utils as db
    import esda_utils as esda
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
        [project_dir, "results", "code_outputs/regression_results.txt"]
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

    env_vars = ["INTDEN", "GREEN_SP", "NOISE", "PM25", "NO2",
                "MEDREV", "R_UNEMP", "R_NN_POBL", "R_NN_CH"]
    print("Variables")
    print("XO:Intercept \nX1:INTDEN \nX2:GREEN_SP \nX3:NOISE \nX4:PM25 \nX5:NO2 \nX6:MEDREV \nX7:R_UNEMP \nX8:R_NN_POBL \nX9:R_NN_CH")
    
    # Standardize variables
    #  Health adjusted ouctomes are already standardized as we take the Pearson residuals of the logistic regression
    data[env_vars] = to_zscore(data[env_vars])


    # REGRESSION MODELS
    compute_ols(data, "hypertension_adj", env_vars)
    compute_spatial_regression(data, "hypertension", env_vars, pool, is_binomial=False)

    #compute_ols(data, "obesity_adj", env_vars)
    # compute_spatial_regression(data, "obesity", env_vars, pool, is_binomial=False)

    # compute_ols(data, "diabetes_adj", env_vars)
    # compute_spatial_regression(data, "diabetes", env_vars, pool, is_binomial=False)
    
    # compute_ols(data, "dyslipidemia_adj", env_vars)
    # compute_spatial_regression(data, "dyslipidemia", env_vars, pool, is_binomial=False)

    # # hypertension
    # data = gpd.read_file("processed_data/hypertension_mgwr.gpkg")
    # gwr_bw = 3676
    # mgwr_bw = [489, 3690, 3618, 3687, 3689, 3689, 3690, 3690]
    # map_gwr_mgwr(env_vars, data, gwr_bw, mgwr_bw, "hypertension")
    

def compute_ols(df, outcome_name, vars_list):

    print()
    print("OLS - " + outcome_name.upper())
    print()

    df = df[~df[outcome_name].isna()]

    # Gaussian 200-m distance to match the kernel used in KDE
    w = esda.compute_spatial_weights(df, "Kernel", radius=200, kernel_args = {"function":"gaussian"})

    # OLS from statsmodel (should have the same results than pysal + gwr)
    results = ols(f'{outcome_name} ~ {" + ".join(vars_list)}', df).fit()
    print(results.summary())

    # Calculate VIF for each explanatory variable
    cov = results.model.exog
    vif = [variance_inflation_factor(cov, i) for i in range(cov.shape[1])]

    # Print the VIF values
    for i in range(1, len(vif)):
        print(f"VIF for {results.model.exog_names[i]}: {vif[i]:.2f}")

    # OLS from pysal (to measure spatial autocorrelation of residuals)
    y, X, coords = prepare_inputs_gwr(df, outcome_name, vars_list)
    ols_pysal = spreg.OLS(y, X)
    print(ols_pysal.summary)
    m = spreg.MoranRes(ols_pysal, w, z=True)
    print("Moran's I value of the residuals: " + str(round(m.I,4)))
    print("Moran's I standardized value: " + str(round(m.zI,4)))
    print("Moran's I p-value: " + str(round(m.p_norm,4)))


def compute_spatial_regression(data, outcome, vars_list, pool, is_binomial=True):
    
    print()
    print("SPATIAL REGRESSION - " + outcome.upper())
    print()

    if is_binomial is True:
        outcome_name = outcome
    else:
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

    if is_binomial is False:

        # MGWR: multiscale adaptive bandwidth with optimized bandwidth
        print()
        print("MGWR")
        print()
        mgwr_selector, mgwr_bw = search_bandwidth(coords, y, X,
                                                type="multiscale", pool=pool)
        mgwr_results = run_model(coords, y, X,
                                mgwr=True, sel_mgwr=mgwr_selector, pool=pool)
        data = add_gwr_results(data, mgwr_results, vars_list, type="mgwr")

        map_gwr_mgwr(vars_list, data, gwr_bw, mgwr_bw, outcome)

        # Save results
        data.to_file(f"results/regression_models/{outcome}/{outcome_name}_spatreg_results.gpkg", driver="GPKG")
    
    else:
        # Save results
        data.to_file(f"results/regression_models/{outcome}/{outcome_name}_binomial_gwr.gpkg", driver="GPKG")




def map_gwr_mgwr(vars_list, df, gwr_bw, mgwr_bw, disease):

    kwargs1 = {'edgecolor': 'grey', 'alpha': .65}
    kwargs2 = {'edgecolor': 'grey', 'alpha': 1}

    vars_list.insert(0, "intercept")
    print(vars_list)

    # # Filter for T (0.05)
    # for i, var in enumerate(vars_list):
    #     cm.unregister_cmap(name='shiftedcmap')
    #     compare_surfaces(df, f"gwr_{var}", f"mgwr_{var}", getattr(df, f"gwr_{var}_T"), gwr_bw,
    #                      getattr(df, f"mgwr_{var}_T"), mgwr_bw[i], var, kwargs1, kwargs2, savefig=f"results/regression_models/{disease}/mgwr_{disease}_{var}_T")
    
    # Filter for T with correction
    for i, var in enumerate(vars_list):
        cm.unregister_cmap(name='shiftedcmap')
        compare_surfaces(df, f"gwr_{var}", f"mgwr_{var}", getattr(df, f"gwr_{var}_TC"), gwr_bw,
                         getattr(df, f"mgwr_{var}_TC"), mgwr_bw[i], var, kwargs1, kwargs2, savefig=f"results/regression_models/{disease}/mgwr_{disease}_{var}_TC")

    vars_list.remove("intercept")


def add_gwr_results(df, gwr_results, vars_list, type="gwr", is_binomial=True):
    # type=gwr or mgwr

    # Extract results for intercept
    df[f'{type}_intercept'] = gwr_results.params[:, 0]
    df[f'{type}_intercept_Tvalues'] = gwr_results.tvalues[:, 0]
    df[f"{type}_intercept_T"], df[f"{type}_intercept_TC"] = filter_for_inference(gwr_results,
                                                                                 type, 0)

    # Extract results for each covariate
    for i, var in enumerate(vars_list):

        df[f'{type}_{var}'] = gwr_results.params[:, i + 1]
        df[f'{type}_{var}_Tvalues'] = gwr_results.tvalues[:, i + 1]
        df[f"{type}_{var}_T"], df[f"{type}_{var}_TC"] = filter_for_inference(gwr_results,
                                                                             type, i+1)

    # df[f"{type}_R2"] = gwr_results.localR2

    # Extract measures of local collinearity
    lc = gwr_results.local_collinearity()

    if type == "gwr":
        df[f"{type}_CN"] = lc[2]

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
        print("Elapsed time (model): " + str(round((end-start)/60, 2)) + " minutes.")

    else:
        start = time.time()
        model_results = GWR(coords, y, X, bw_gwr, family=Gaussian()).fit(pool=pool)
        end = time.time()
        print("Elapsed time (model): " + str(round((end-start)/60, 2)) + " minutes.")

    print(model_results.summary())

    return model_results


def search_bandwidth(coords, y, X, type="adaptive", pool=None):
    # type=fixed, adaptive, multiscale
    # Defaults Sel_BW(): kernel="bisquare", spherical=False
    # Defaults selector.search(): search_method='golden_section', criterion='AICc'

    start = time.time()

    if type == "fixed":
        selector = Sel_BW(coords, y, X, fixed=True, multi=False, family=Gaussian())
    elif type == "adaptive":
        selector = Sel_BW(coords, y, X, fixed=False, multi=False, family=Gaussian())
    elif type == "multiscale":
        selector = Sel_BW(coords, y, X, fixed=False, multi=True, family=Gaussian())
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
