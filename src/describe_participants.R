# This code run descriptive statistics about Colaus participants

library(tidyverse)
library(sf)
require(RPostgreSQL)


# Extract participants ----------------------------------------------------

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= rstudioapi::askForPassword("Database user"),rstudioapi::askForPassword("Database password"),dbname="geosan")

retrieve_dataset <- function(table_name, con){
  query <- paste0("SELECT * FROM syndemic.", table_name," WHERE NOT ST_IsEmpty(geometry);")
  table <- read_sf(con, query=query)
}

indiv.b <- retrieve_dataset("colaus_b", con)
indiv.f1 <- retrieve_dataset("colaus_f1", con)
indiv.f2 <- retrieve_dataset("colaus_f2", con)
indiv.f3 <- retrieve_dataset("colaus_f3", con)


data <- retrieve_dataset("colaus_f2_complete", con)
list_outcomes <- c("cvdbase_adj", "f2cvd", "f2bmi_cat2", "f2diab", "f2orldrg", "f2insn", "f2antidiab", "f2hta", "f2crbpmed", )

outcomes.cvd <- c("cvdbase_adj", "f2cvd")
outcomes.obesity <- c("f2bmi_cat2")
outcomes.diabetes <- c("f2diab", "f2orldrg", "f2insn", "f2antidiab")
outcomes.hypertension <- c("f2hta", "f2crbpmed")
outcomes.dyslipidemia <- c("f2hypolip", "f2chol", "f2ldlch")

compute_outcomes <- function(period){
  # f2orldrg & f2insn have both NaN values and "no data" (=9). Convert "no data" to NaN values first
  f2_outcomes.sf <- f2.sf %>% 
    mutate(
      f2orldrg = replace(f2orldrg, f2orldrg ==9, NA),
      f2insn = replace(f2insn, f2insn ==9, NA),
      f2crbpmed = replace(f2crbpmed, f2crbpmed ==9, NA),
      f2hctld = replace(f2hctld, f2hctld ==9, NA),
      f2hypolip = replace(f2hypolip, f2hypolip ==9, NA)
    )
  
  
  f2_outcomes.sf <- f2.sf %>% 
    mutate(
      cvd = case_when(if_any(c(cvdbase_adj, f2cvd), ~ .== 1) ~ 1, if_all(c(f2cvd, cvdbase_adj), ~is.na(.)) ~ NaN, TRUE ~ 0),
      obesity = case_when(f2bmi_cat2==3~1,f2bmi_cat2%in%c(0,1,2)~0,is.na(f2bmi_cat2)~NaN),
      diabetes = case_when(if_any(c(f2diab, f2orldrg, f2insn, f2antidiab), ~ .== 1) ~ 1, if_all(c(f2diab, f2orldrg, f2insn, f2antidiab), ~is.na(.)) ~ NaN, TRUE ~ 0),
      hypertension = case_when(if_any(c(f2hta, f2crbpmed), ~ .== 1) ~ 1, if_all(c(f2hta, f2crbpmed), ~is.na(.)) ~ NaN, TRUE ~ 0),
      dyslipidemia = case_when(if_any(c(f2hctld, f2hypolip), ~ .== 1) ~ 1, if_all(c(f2hctld, f2hypolip), ~is.na(.)) ~ NaN, TRUE ~ 0)
    ) %>%
    mutate(
      multimorbidity = if_else(rowSums(across(cvd:dyslipidemia),na.rm = TRUE) >= 2, 1, 0),
      polypharmacy = polypharm,
      chronic_inclusive = if_else(if_any(c(cvd, obesity, diabetes, hypertension, dyslipidemia, polypharmacy)), 1, 0)
    )
  
  f2_outcomes.sf <- f2_outcomes.sf %>% select(pt, f2age, f2sex, cvd:chronic_inclusive)
  
}





DBI::dbDisconnect(con)