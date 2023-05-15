# This code run descriptive statistics about Colaus participants

library(tidyverse)
library(sf)
require(RPostgreSQL)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

# FUNCTIONS ---------------------------------------------------------------

write <- function(text){
  cat(paste(text, "\n\n"), file=file_res, append=TRUE)
}

capture <- function(result){
  capture.output(result, file=file_res, append=TRUE)
}

covariate_stats <- function(var, df){

  write(" ")
  
  if (var == 'age'){
    df %>%
      st_drop_geometry() %>%
      summarise(mean = mean(!!as.name(var)), 
                sd = sd(!!as.name(var),  na.rm=TRUE)) %>%
      capture() 
  }
  else{
    df %>%
      st_drop_geometry() %>%
      group_by(!!as.name(var)) %>%
      summarise(n = n()) %>%
      mutate(perc = round(100*(n / sum(n)), 2)) %>% capture() 
  }
}


# Extract participants ----------------------------------------------------

#Create file to store results
file_res=paste0("../results/code_outputs/describe_participants.txt")
cat(paste0("Date:", Sys.Date(),'\n'), file = file_res, append = FALSE) #Overwrite the file

indiv.b <- st_read("../processed_data/b_indiv_covariates.gpkg")

indiv.f2 <- read_sf(con, query="SELECT * FROM geochronic.f2_study_dataset_lausanne") # including sex, age, etc. from colaus_baseline

# Characteristics of participants -----------------------------------------

cov <- c("age", "sex", "swiss", "cohabiting", "education", "working", "income", "difficulties", "smoking", "drinking", "inactivity")
cov.b <- cov[!cov %in% c("difficulties", "income")]

write("\n--------------")
write("BASELINE")
write("--------------")

indiv.b <- indiv.b %>% select(pt, datexam, cov.b)

write(paste("Number of individuals:", indiv.b %>% nrow()))
write(paste("Time range:", min(indiv.b$datexam), "/", max(indiv.b$datexam)))
write(paste("Min / Max age:", min(indiv.b$age), "/", max(indiv.b$age)))

lapply(cov.b, covariate_stats, df=indiv.b)


write("\n--------------")
write("FOLLOW-UP 2")
write("--------------")

indiv.f2 <- indiv.f2 %>% select(pt, f2datexam, cov)

write(paste("Number of individuals:", indiv.f2 %>% nrow()))
write(paste("Time range:", min(indiv.f2$f2datexam, na.rm=TRUE), "/", max(indiv.f2$f2datexam, na.rm=TRUE)))
write(paste("Min / Max age:", min(indiv.f2$age), "/", max(indiv.f2$age)))

lapply(cov, covariate_stats, df=indiv.f2)

DBI::dbDisconnect(con)