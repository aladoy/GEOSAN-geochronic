# This code runs a logistic regression on CoLaus participants

library(tidyverse)
library(sf)
library(MASS)
require(RPostgreSQL)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")



# IMPORT DATA -------------------------------------------------------------

#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

data <- read_sf(con, query="SELECT * FROM geochronic.f2_study_dataset")


table(data$marital)

data


fullModel = glm(X1 ~ ., family = 'binomial', data = dat) # model with all 9 variables
nullModel = glm(X1 ~ 1, family = 'binomial', data = dat) # model with the intercept only
summary(stepAIC(fullModel, # start with a model containing all variables
                direction = 'backward', # run backward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0)) # do not show the step-by-step process of model selection