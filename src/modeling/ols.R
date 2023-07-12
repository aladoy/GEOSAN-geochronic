# Compute the Ordinatry Least Squares Regression (OLS) to be compared with GWR/MGWR, and that will be used to assess VIF

require(RPostgreSQL)
require(car)
require(tidyverse)
require(sf)
require(DBI)

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

# LOAD DATASETS -----------------------------------------------------------

ha <- read_sf(con, query="SELECT * FROM geochronic.ha_characteristics") %>% st_drop_geometry()
ha <- ha %>% rename_with(~ toupper(.), -c(reli))

indiv =  st_read("../processed_data/f2_adjusted_outcomes.gpkg") %>% st_drop_geometry()


cov <- c("INTDEN", "GREEN_SP", "NOISE", "PM25", "NO2", "MEDREV", "R_UNEMP", "R_NN_POBL", "R_NN_CH")

extract_data_outcome <- function(df, hectares, outcome_name){
  
  data <- df %>% 
    inner_join(hectares, by="reli") %>% 
    filter(!is.na(!!as.name(outcome_name))) %>%
    dplyr::select(!!as.name(outcome_name), all_of(cov))
   
  return(data)
}

run_ols <- function(data, outcome_name){
  
  ols <-lm(as.formula(paste0(outcome_name, '~  INTDEN + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_NN_CH')), 
           data=data)
  
  print(summary(ols))
  print(vif(ols))
  
}

hyp.data <- extract_data_outcome(indiv, ha, "hypertension_adj")
run_ols(hyp.data, "hypertension_adj")

obes.data <- extract_data_outcome(indiv, ha, "obesity_adj")
run_ols(obes.data, "obesity_adj")

diab.data <- extract_data_outcome(indiv, ha, "diabetes_adj")
run_ols(diab.data, "diabetes_adj")

dys.data <- extract_data_outcome(indiv, ha, "dyslipidemia_adj")
run_ols(dys.data, "dyslipidemia_adj")

DBI::dbDisconnect(con)