# This code define health outcomes for our study on Follow-up 2

library(sf)
library(tidyverse)
library(RPostgreSQL)

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('wrangling/utils_define_outcomes.R')


con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
# con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


study_area <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")


# FUNCTIONS ---------------------------------------------------------------

# for loop with periods
periods <- c("b", "f1", "f2")

for (period in periods){
  
  data <- load_participants(con, period=period)
  
  #Create file to store results
  f=paste0("../results/code_outputs/define_outcomes_",period,".txt")
  cat(paste0("Date:", Sys.Date(),'\n'), file = f, append = FALSE) #Overwrite the file
  
  
  # Cardiovascular diseases -------------------------------------------------
  
  write("\n--------------", f)
  write("CARDIOVASCUALR DISEASES", f)
  write("--------------", f)
  
  
  if(period=="b"){
    
    write("definition: not studied due to the difficulty of verifying information", f)
    
  }else if(period=="f1"){
    
    outcomes.cvd <- c("cvdbase_adj", "f1cvd")
    write("definition: baseline CVD (cvdbase_adj=1) and/or CVD event since baseline (f1cvd=1)", f)  
    
  }else if(period=="f2"){
    
    outcomes.cvd <- c("cvdbase_adj", "f2cvd")
    write("definition: baseline CVD (cvdbase_adj=1) and/or CVD event since baseline (f2cvd=1)", f)
  
  }
  
  if(period != "b"){

    lapply(outcomes.cvd, count_distinct_val, data=data) %>% capture(f)
    
    write("note: if no cvd was reported at baseline, cvdbase_adj=NA -> change to 0", f)
    data <- data %>% mutate(cvdbase_adj = replace(cvdbase_adj, is.na(cvdbase_adj), 0))
    
    data <- data %>% 
      mutate(cvd = case_when(
        if_any(all_of(outcomes.cvd), ~ .== 1) ~ 1, 
        if_all(all_of(outcomes.cvd), ~is.na(.)) ~ NaN, 
        TRUE ~ 0))
    
    count_unique_combinations("cvd", data=data) %>% capture(f)
  }

  
  
  # Obesity ----------------------------------------------------------------
  
  write("\n--------------", f)
  write("OBESITY", f)
  write("--------------", f)
  
  
  if(period=="b"){
    
    outcomes.obesity <- "bmi_cat2"
    write("definition: obese (bmi_cat2=3)", f)
    
  }else if(period=="f1"){
    
    outcomes.obesity <- "f1bmi_cat2"
    write("definition: obese (f1bmi_cat2=3)", f)
    
  }else if(period=="f2"){
    
    outcomes.obesity <- "f2bmi_cat2"
    write("definition: obese (f2bmi_cat2=3)", f)
  
  }
  
  lapply(outcomes.obesity, count_distinct_val, data=data) %>% capture(f)
  write("note: as only obesity is of interest, we'll consider all other values as 0 while keeping NAs", f)
  
  data <- data %>% 
    mutate(obesity = case_when(
      !!as.name(outcomes.obesity) == 3~1,
      !!as.name(outcomes.obesity) %in% c(0,1,2) ~ 0,
      is.na(!!as.name(outcomes.obesity))~NaN))
  
  count_unique_combinations("obesity", data=data) %>% capture(f)
  
  
  
  # Diabetes ----------------------------------------------------------------
  
  write("\n--------------", f)
  write("DIABETES", f)
  write("--------------", f)
  
  
  if(period=="b"){
    
    outcomes.diabetes <- "diab"
    write("definition: diabetes using FPG>=7.0mmol/L and/or antidiabetic treatment (diab=1).", f)
    
  }else if(period=="f1"){
    
    outcomes.diabetes <- "f1diab"
    write("definition: diabetes using FPG>=7.0mmol/L and/or antidiabetic treatment (f1diab=1).", f)
    
  }else if(period=="f2"){
    
    outcomes.diabetes <- "f2diab"
    write("definition: diabetes using FPG>=7.0mmol/L and/or antidiabetic treatment (f2diab=1)", f)
  }
  
  lapply(outcomes.diabetes, count_distinct_val, data=data) %>% capture(f)
  
  data <- data %>% 
    mutate(diabetes = case_when(
      !!as.name(outcomes.diabetes) == 1 ~ 1, 
      !!as.name(outcomes.diabetes) == 0 ~ 0, 
      TRUE ~ NaN))
  count_unique_combinations("diabetes", data=data) %>% capture(f)
  
  
  # Hypertension ----------------------------------------------------------------
  
  write("\n--------------", f)
  write("HYPERTENSION", f)
  write("--------------", f)
  
  
  if(period=="b"){
    
    outcomes.hypertension <- "antihta"
    write("definition: antihypertensive drug treatment (antihta=1).", f)
    
    
  }else if(period=="f1"){
    
    outcomes.hypertension <- "f1antihta"
    write("definition: antihypertensive drug treatment (f1hta=1).", f)
  
    
  }else if(period=="f2"){
    
    outcomes.hypertension <- "f2antihta"
    write("definition: antihypertensive drug treatment (f2antihta=1)", f)
  }
  
  lapply(outcomes.hypertension, count_distinct_val, data=data) %>% capture(f)
  data <- data %>% 
    mutate(hypertension = case_when(
      !!as.name(outcomes.hypertension) == 1 ~ 1, 
      !!as.name(outcomes.hypertension) == 0 ~ 0, 
      TRUE ~ NaN))
  
  count_unique_combinations("hypertension", data=data) %>% capture(f)
  
  
  # Dyslipidemia ----------------------------------------------------------------
  
  write("\n--------------", f)
  write("DYSLIPIDEMIA", f)
  write("--------------", f)
  
  write("note: for dyslipidemia, we converted total cholesterol and LDL-cholesterol as binary variables based on the threshold defined in Abolhassani et al. (2017). ", f)
  
  if(period=="b"){
    
    outcomes.dyslipidemia <- c("chol", "ldlch", "hypolip")
    write("definition: Total cholesterol > 6.5mmol/L (chol=1) and/or LDL-cholesterol > 4.1mmol/L (ldlch=1) hypolipidemic drug treatment (hypolip=1)", f)
    data <- data %>% mutate(chol = if_else(chol>6.5, 1, 0),
                            ldlch = if_else(ldlch>4.1, 1, 0))
    
  }else if(period=="f1"){
    
    outcomes.dyslipidemia <- c("f1chol", "f1ldlch", "f1hypolip")
    write("definition: Total cholesterol > 6.5mmol/L (f1chol=1) and/or LDL-cholesterol > 4.1mmol/L (f1ldlch=1) hypolipidemic drug treatment (f1hypolip=1)", f)
    data <- data %>% mutate(f1chol = if_else(f1chol>6.5, 1, 0),
                            f1ldlch = if_else(f1ldlch>4.1, 1, 0))
    
    write("dyslipidemia (f1hypolip, f1chol, f1ldlch) must be requested to the CoLaus team", f)
    
  }else if(period=="f2"){
    
    outcomes.dyslipidemia <- c("f2hypolip", "f2chol", "f2ldlch")
    write("definition: Total cholesterol > 6.5mmol/L (f2chol=1) and/or LDL-cholesterol > 4.1mmol/L (f2ldlch=1) hypolipidemic drug treatment (f2hypolip=1)", f)
    data <- data %>% mutate(f2chol = if_else(f2chol>6.5, 1, 0),
                            f2ldlch = if_else(f2ldlch>4.1, 1, 0))
    
    write("note: in case of 'Not relevant' (f2hypolip=8) -> change to 0 \n  in case of 'Does not know' (f2hypolip=9) -> change to NA", f)
    data <- data %>% mutate(f2hypolip = replace(f2hypolip, f2hypolip==8, 0),
                            f2hypolip = replace(f2hypolip, f2hypolip==9, NA))
    
  }
  
  lapply(outcomes.dyslipidemia, count_distinct_val, data=data) %>% capture(f)
  
  data <- data %>% 
    mutate(dyslipidemia = case_when(
      if_any(all_of(outcomes.dyslipidemia), ~ .== 1) ~ 1, 
      if_all(all_of(outcomes.dyslipidemia), ~ .== 0) ~ 0, 
      TRUE ~ NaN))
  
  count_unique_combinations("dyslipidemia", data=data) %>% capture(f)
  
  
  # Extract outcomes --------------------------------------------------------
  
  write("\n--------------", f)
  write("SUMMARY", f)
  write("--------------", f)
  
  
  if(period=="b"){
    
    outcomes.all <- c("diabetes", "hypertension", "obesity", "dyslipidemia")
    
  }else if(period=="f1"){
    
    outcomes.all <- c("cvd", "diabetes", "hypertension", "obesity", "dyslipidemia")
    
  }else if(period=="f2"){
    
    outcomes.all <- c("cvd", "diabetes", "hypertension", "obesity", "dyslipidemia")
    
  }
  
  data.outcomes <- data %>% dplyr::select(pt, all_of(outcomes.all))
  
  lapply(outcomes.all, print_final_stats, data=data, file_res=f)
  
  write("\nSUMMARY FOR LAUSANNE AREA ONLY\n", file_res=f)
  data_laus <- filter(data, st_intersects(geometry, study_area, sparse = FALSE)[,1])
  lapply(outcomes.all, print_final_stats, data=data_laus, file_res=f)
  
  write(paste("Number of participants having a missing outcome: (which should be removed for an eligible dataset):", data.outcomes %>% filter_all(any_vars(is.na(.))) %>% nrow()), f)
  
  
  st_write(data.outcomes, paste0("../processed_data/",period,"_outcomes.gpkg"), driver='GPKG', delete_layer=TRUE, layer_options="GEOMETRY_NAME=geometry")
  
}

DBI::dbDisconnect(con)