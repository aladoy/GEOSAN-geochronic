# This code defines the individual covariates used in the regression analysis. Homogeneize covariates between baseline and follow-up 2 in order to compare participants.

library(tidyverse)
library(sf)
require(RPostgreSQL)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

# Extract participants ----------------------------------------------------

#Create file to store results
file_res=paste0("../results/code_outputs/describe_participants.txt")
cat(paste0("Date:", Sys.Date(),'\n'), file = file_res, append = FALSE) #Overwrite the file

indiv.b <- read_sf(con, query="SELECT * FROM geochronic.colaus_b")
indiv.b <- indiv.b %>% mutate(datexam = as.Date(datexam))

indiv.f2 <- read_sf(con, query="SELECT * FROM geochronic.colaus_f2 f2 INNER JOIN  (SELECT pt, brnsws, datarrival, bthpl_dem, ethori_self, lvplyr, edtyp3, edtyp4, mrtsts2, cvdbase, cvdbase_adj FROM  geochronic.colaus_b) b USING (pt)") # including man, age, etc. from colaus_baseline
indiv.f2  <- indiv.f2  %>% mutate(f2datexam = as.Date(f2datexam))

# Define individual covariates --------------------------------------------

indiv.b <- indiv.b %>% mutate(
  age = age,
  sex = sex,
  swiss = brnsws,
  cohabiting = mrtsts2,
  education = edtyp3,
  working = if_else(job_curr8 %in% c(1,2,3), 1, 0),
  smoking = if_else(sbsmk %in% c(0,1), 0, 1),
  drinking = if_else(conso_hebdo < 14, 0, 1),
  phyact = replace(phyact, phyact==9, NA),
  inactivity = if_else(phyact == 0, 1, 0)
)


indiv.f2 <- indiv.f2 %>% mutate(
  age = f2age,
  sex = f2sex,
  swiss = brnsws,
  cohabiting = f2mrtsts2,
  education = edtyp3,
  working = f2job_curr1,
  income = replace(f2income1, f2income1 %in% c(8,9), NA),
  f2income5 = replace(f2income5, f2income5==9, NA),
  difficulties = if_else(f2income5 %in% c(0,1), 0, 1),
  smoking = if_else(f2sbsmk %in% c(0,1), 0, 1),
  drinking = if_else(f2alcool2 %in% c(0,1), 0, 1),
  inactivity = f2seden
)


cov.b <- c("age", "sex", "swiss", "cohabiting", "education", "working", "smoking", "drinking", "inactivity")
indiv.b <- indiv.b %>% select(pt, datexam, all_of(cov.b))

cov.f2 <- c("age", "sex", "swiss", "cohabiting", "education", "working", "income", "difficulties", "smoking", "drinking", "inactivity")
indiv.f2 <- indiv.f2 %>% select(pt, f2datexam, all_of(cov.f2))

# Save follow-up 2 individual covariates
st_write(indiv.b, "../processed_data/b_indiv_covariates.gpkg", driver='GPKG', delete_layer=TRUE)
st_write(indiv.f2, "../processed_data/f2_indiv_covariates.gpkg", driver='GPKG', delete_layer=TRUE)

DBI::dbDisconnect(con)

