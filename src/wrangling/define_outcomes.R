# This code define health outcomes for our study on Follow-up 2

library(tidyverse)
library(sf)
require(RPostgreSQL)

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('wrangling/utils_define_outcomes.R')


#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")




#data <- read_sf(con, query="SELECT * FROM geochronic.f2_geo_vaud")

# FUNCTIONS ---------------------------------------------------------------

period <- "f1"

data <- load_participants(con, period=period)


#Create file to store results
f=paste0("../results/code_outputs/define_outcomes_",period,".txt")
cat(paste0("Date:", Sys.Date(),'\n'), file = f, append = FALSE) #Overwrite the file


# Cardiovascular diseases -------------------------------------------------

write("\n--------------", f)
write("CARDIOVASCUALR DISEASES", f)
write("--------------", f)


if(period=="b"){
  
  outcomes.cvd <- c("cvdbase_adj")
  write("definition: baseline CVD (cvdbase_adj=1)", f)
  
}else if(period=="f1"){
  
  outcomes.cvd <- c("cvdbase_adj", "f1cvd")
  write("definition: baseline CVD (cvdbase_adj=1) and/or CVD event since baseline (f1cvd=1)", f)  
  
}else if(period=="f2"){
  
  outcomes.cvd <- c("cvdbase_adj", "f2cvd")
  write("definition: baseline CVD (cvdbase_adj=1) and/or CVD event since baseline (f2cvd=1)", f)

}

lapply(outcomes.cvd, count_distinct_val, data=data) %>% capture(f)

write("note: if no cvd was reported at baseline, cvdbase_adj=NA -> change to 0", f)
data <- data %>% mutate(cvdbase_adj = replace(cvdbase_adj, is.na(cvdbase_adj), 0))

data <- data %>% 
  mutate(cvd = case_when(
    if_any(all_of(outcomes.cvd), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.cvd), ~is.na(.)) ~ NaN, 
    TRUE ~ 0))

count_unique_combinations("cvd", data=data) %>% capture(f)


# Obesity ----------------------------------------------------------------

write("\n--------------")
write("OBESITY")
write("--------------")

outcomes.obesity <- c("f2bmi_cat2")
lapply(outcomes.obesity, count_distinct_val) %>% capture()

write("note: as only obesity is of interest, we'll consider all other values as 0 while keeping NAs")
write("definition: obese (f2bmi_cat2=3)")
data <- data %>% 
  mutate(obesity = case_when(
    f2bmi_cat2 == 3~1,
    f2bmi_cat2 %in% c(0,1,2) ~ 0,
    is.na(f2bmi_cat2)~NaN))

count_unique_combinations("obesity") %>% capture()


# Diabetes ----------------------------------------------------------------

write("\n--------------")
write("DIABETES")
write("--------------")

outcomes.diabetes <- c("f2diab", "f2orldrg", "f2insn", "f2antidiab")
lapply(outcomes.diabetes, count_distinct_val) %>% capture()

write("note: in case of 'Does not apply' (f2insn, f2orldrg=8) -> change to 0 \n  in case of 'No data' (f2insn, f2orldrg=9) -> change to NA")
data <- data %>% mutate(f2orldrg = replace(f2orldrg, f2orldrg==8, 0),
                        f2orldrg = replace(f2orldrg, f2orldrg==9, NA),
                        f2insn = replace(f2insn, f2insn==8, 0),
                        f2insn = replace(f2insn, f2insn==9, NA))

write("definition: diabetes using FPG>=7.0mmol/L (f2diab=1) and/or antidiabetic treatment (f2orldrg=1 or f2insn=1 or f2antidiab=1)")
data <- data %>% 
  mutate(diabetes = case_when(
    if_any(all_of(outcomes.diabetes), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.diabetes), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("diabetes") %>% capture()


# Hypertension ----------------------------------------------------------------

write("\n--------------", f)
write("HYPERTENSION", f)
write("--------------", f)

outcomes.hypertension <- c("f2hta", "f2crbpmed")
lapply(outcomes.hypertension, count_distinct_val) %>% capture(f)

write("note: in case of no drug treatment (f2crbpmed=2) -> change to 0, in case of 'Not relevant' (f2crbpmed=8) -> change to 0 \n  in case of 'Does not know' (f2crbpmed=9) -> change to NA", f)
data <- data %>% mutate(f2crbpmed = replace(f2crbpmed, f2crbpmed==8, 0),
                        f2crbpmed = replace(f2crbpmed, f2crbpmed==2, 0),
                        f2crbpmed = replace(f2crbpmed, f2crbpmed==9, NA))

write("definition: hypertension defined as >140/90 (f2hta=1) and/or medication for high blood pressure (f2crbpmed=1)", f)
data <- data %>% 
  mutate(hypertension = case_when(
    if_any(all_of(outcomes.hypertension), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.hypertension), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("hypertension") %>% capture(f)


# Dyslipidemia ----------------------------------------------------------------

write("\n--------------", f)
write("DYSLIPIDEMIA", f)
write("--------------", f)

write("note: for dyslipidemia, we converted f2chol and f2ldlch as binary variables based on the threshold defined in Abolhassani et al. (2017). ", f)
data <- data %>% mutate(f2chol = if_else(f2chol>6.5, 1, 0),
                        f2ldlch = if_else(f2ldlch>4.1, 1, 0))

outcomes.dyslipidemia <- c("f2hypolip", "f2chol", "f2ldlch")
lapply(outcomes.dyslipidemia, count_distinct_val) %>% capture(f)

write("note: in case of 'Not relevant' (f2hypolip=8) -> change to 0 \n  in case of 'Does not know' (f2hypolip=9) -> change to NA", f)
data <- data %>% mutate(f2hypolip = replace(f2hypolip, f2hypolip==8, 0),
                        f2hypolip = replace(f2hypolip, f2hypolip==9, NA))

write("definition: Total cholesterol > 6.5mmol/L (f2chol=1) and/or LDL-cholesterol > 4.1mmol/L (f2ldlch=1) hypolipidemic drug treatment (f2hypolip=1)", f)
data <- data %>% 
  mutate(dyslipidemia = case_when(
    if_any(all_of(outcomes.dyslipidemia), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.dyslipidemia), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("dyslipidemia") %>% capture(f)


# Polypharmacy ----------------------------------------------------------------

write("\n--------------", f)
write("POLYPHARMACY", f)
write("--------------", f)

outcomes.polypharmacy <- c("polypharm")
lapply(outcomes.polypharmacy, count_distinct_val) %>% capture(f)

write("definition: polypharmacy (polypharm=1)")
data <- data %>% 
  mutate(polypharmacy = if_else(polypharm==1,1,0))

count_unique_combinations("polypharmacy") %>% capture(f)


# Morbidity ----------------------------------------------------------------

write("\n--------------")
write("MORBIDITY")
write("--------------")

outcomes.morbidity <- c("cvd", "obesity", "diabetes", "hypertension", "dyslipidemia")
lapply(outcomes.morbidity, count_distinct_val) %>% capture(f)

write("definition: CVD and/or obesity and/or diabetes and/or hypertension and/or dyslipidemia", f)
data <- data %>% 
  mutate(morbidity = case_when(
    if_any(all_of(outcomes.morbidity), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.morbidity), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("morbidity") %>% capture(f)


# Multimorbidity ----------------------------------------------------------------

write("\n--------------", f)
write("MULTIMORBIDITY", f)
write("--------------", f)

outcomes.multimorbidity <- c("cvd", "obesity", "diabetes", "hypertension", "dyslipidemia")

write("definition: at least two conditions between CVD, obesity, diabetes, hypertension, dyslipidemia", f)
data <- data %>% 
  mutate(multimorbidity = case_when(
    rowSums(across(outcomes.multimorbidity),na.rm = TRUE) >= 2 ~ 1, 
    (rowSums(across(outcomes.multimorbidity),na.rm = TRUE) == 0) & (rowSums(is.na(across(outcomes.multimorbidity))) == 1) ~ 0, 
    (rowSums(across(outcomes.multimorbidity),na.rm = TRUE) < 2) & (rowSums(is.na(across(outcomes.multimorbidity))) >= 1) ~ NaN, 
    TRUE ~ 0))

count_unique_combinations("multimorbidity") %>% capture(f)


# Extract outcomes --------------------------------------------------------

write("\n--------------")
write("SUMMARY")
write("--------------")

outcomes.all <- c("cvd", "diabetes", "hypertension", "obesity", "dyslipidemia", "morbidity", "multimorbidity", "polypharmacy")

data.outcomes <- data %>% select(pt, all_of(outcomes.all))

lapply(outcomes.all, print_final_stats)

write("Number of participants having a missing outcome: (which we should remove for an eligible dataset:")
data.outcomes %>% filter_all(any_vars(is.na(.))) %>% nrow() %>% capture(f)

st_write(data.outcomes, "../processed_data/f2_outcomes.gpkg", driver='GPKG', delete_layer=TRUE)

DBI::dbDisconnect(con)