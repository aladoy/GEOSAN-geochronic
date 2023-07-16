require(tidyverse)
require(sf)
require(ggplot2)
require(viridis)
require(classInt)
require(ggspatial)
require(RPostgreSQL)

source('modeling/utils_map_gwr_results.R')
setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")


create_maps_outcome <- function(data, tol_contours, vars_list, outcome){
  
  for (var in vars_list) {
    map_coefficients(data, paste0("gwr_", var), outcome, tol_contours, type = "estimates")
    map_coefficients(data, paste0("mgwr_", var), outcome, tol_contours, type = "estimates")
    map_coefficients(data, paste0("gwr_", var), outcome, tol_contours, type = "t-values")
    map_coefficients(data, paste0("mgwr_", var), outcome, tol_contours, type = "t-values")
    map_association(data, paste0("gwr_", var, "_TC"), outcome, tol_contours)
    map_association(data, paste0("mgwr_", var, "_TC"), outcome, tol_contours)
  }
  
}

# tc_columns <- names(gwr)[grep("_TC$", names(gwr))]
# lapply(tc_columns, function(var) gwr_map(gwr, var))


# HYPERTENSION ------------------------------------------------------------

# Tolerance contours
hyp.tol <- st_read("../results/spatial_disease_risk/hypertension/f2/tolerance_contours_hypertension_200.geojson")
# GWR results
hyp.data <- st_read("../results/regression_models/hypertension/hypertension_adj_spatreg_results.gpkg")

# Variables with significant associations
hyp.interest <- c("intercept", "PM25", "MEDREV", "R_NN_CH")
# Few variations in bandwidths. It seems that all variables at a global level, and that GWR and MGWR do not show strong difference

create_maps_outcome(hyp.data, hyp.tol, hyp.interest, "hypertension")
map_condition_number(hyp.data, "hypertension", hyp.tol, model="gwr")
map_condition_number(hyp.data, "hypertension", hyp.tol, model="mgwr")

# OBESITY -----------------------------------------------------------------

# Tolerance contours
obes.tol <- st_read("../results/spatial_disease_risk/obesity/f2/tolerance_contours_obesity_200_F2.geojson")
# GWR results
obes.data <- st_read("../results/regression_models/obesity/obesity_adj_spatreg_results.gpkg")

# Variables with significant associations
obes.interest <- c("intercept", "INTDEN", "GREEN_SP", "PM25", "NO2", "MEDREV", "R_UNEMP", "R_NN_POBL")
# Lot of variations between GWR and MGWR

create_maps_outcome(obes.data, obes.tol, obes.interest, "obesity")
map_condition_number(obes.data, "obesity", obes.tol, model="gwr")
map_condition_number(obes.data, "obesity", obes.tol, model="mgwr")


# DIABETES ----------------------------------------------------------------

# Tolerance contours
diab.tol <- st_read("../results/spatial_disease_risk/diabetes/f2/tolerance_contours_diabetes_200_F2.geojson")
# GWR results
diab.data <- st_read("../results/regression_models/diabetes/diabetes_adj_spatreg_results.gpkg")

# Variables with significant associations
diab.interest <- c("MEDREV", "R_NN_CH")
# Except for MEDREV that seem to act at a more local scale than what GWR was supposedly noticed, all variables act as global.

create_maps_outcome(diab.data, diab.tol, diab.interest, "diabetes")
map_condition_number(diab.data, "diabetes", diab.tol, model="gwr")
map_condition_number(diab.data, "diabetes", diab.tol, model="mgwr")


# DYSLIPIDEMIA ------------------------------------------------------------

# Tolerance contours
dys.tol <- st_read("../results/spatial_disease_risk/dyslipidemia/f2/tolerance_contours_dyslipidemia_200_F2.geojson")
# GWR results
dys.data <- st_read("../results/regression_models/dyslipidemia/dyslipidemia_adj_spatreg_results.gpkg")

map_condition_number(dys.data, "dyslipidemia", dys.tol, model="gwr")
map_condition_number(dys.data, "dyslipidemia", dys.tol, model="mgwr")

DBI::dbDisconnect(con)
