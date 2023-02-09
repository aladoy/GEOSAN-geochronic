# This code define health outcomes for our study on Follow-up 2

library(tidyverse)
library(sf)
require(RPostgreSQL)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

data <- read_sf(con, query="SELECT * FROM geochronic.f2_geo_vaud")

# FUNCTIONS ---------------------------------------------------------------

count_distinct_val <- function(var){
  data %>% 
    st_drop_geometry() %>% 
    group_by(!!as.name(var)) %>% 
    summarise(n())
}

write <- function(text){
  cat(paste(text, "\n\n"), file=file_res, append=TRUE)
}

capture <- function(result){
  capture.output(result, file=file_res, append=TRUE)
}

count_unique_combinations <- function(outcome){
  list_name <- paste0("outcomes.", outcome)
  print(data %>% st_drop_geometry() %>% select(all_of(!!as.name(list_name)), !!as.name(outcome)) %>% group_by_all() %>% summarise(n()), n=Inf) %>% capture()
}

print_final_stats <- function(var){
  
  subset <- data.outcomes %>% st_drop_geometry() %>% select(pt, !!as.name(var))
  
  n = subset %>% nrow()
  cases <- subset %>% filter(!!as.name(var)==1) %>% nrow()
  nan <- subset %>% filter(is.nan(!!as.name(var))) %>% nrow()
  
  write("")
  write(paste("Statistics for", str_to_upper(var)))
  print(paste("Cases:", cases, "(", round(100*(cases/n),2), "%)")) %>% capture()
  print(paste("Missing values:", nan, "(", round(100*(nan/n),2), "%)")) %>% capture()
  
}



#Create file to store results
file_res=paste0("../results/code_outputs/define_outcomes.txt")
cat(paste0("Date:", Sys.Date(),'\n'), file = file_res, append = FALSE) #Overwrite the file


# Cardiovascular diseases -------------------------------------------------

write("\n--------------")
write("CARDIOVASCUALR DISEASES")
write("--------------")

outcomes.cvd <- c("cvdbase_adj", "f2cvd")
lapply(outcomes.cvd, count_distinct_val) %>% capture()

write("note: if no cvd was reported at baseline, cvdbase_adj=NA -> change to 0")
data <- data %>% mutate(cvdbase_adj = replace(cvdbase_adj, is.na(cvdbase_adj), 0))

write("definition: baseline CVD (cvdbase_adj=1) and/or CVD event since baseline (f2cvd=1)")
data <- data %>% 
  mutate(cvd = case_when(
    if_any(all_of(outcomes.cvd), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.cvd), ~is.na(.)) ~ NaN, 
    TRUE ~ 0))

count_unique_combinations("cvd") %>% capture()


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

write("\n--------------")
write("HYPERTENSION")
write("--------------")

outcomes.hypertension <- c("f2hta", "f2crbpmed")
lapply(outcomes.hypertension, count_distinct_val) %>% capture()

write("note: in case of no drug treatment (f2crbpmed=2) -> change to 0, in case of 'Not relevant' (f2crbpmed=8) -> change to 0 \n  in case of 'Does not know' (f2crbpmed=9) -> change to NA")
data <- data %>% mutate(f2crbpmed = replace(f2crbpmed, f2crbpmed==8, 0),
                        f2crbpmed = replace(f2crbpmed, f2crbpmed==2, 0),
                        f2crbpmed = replace(f2crbpmed, f2crbpmed==9, NA))

write("definition: hypertension defined as >140/90 (f2hta=1) and/or medication for high blood pressure (f2crbpmed=1)")
data <- data %>% 
  mutate(hypertension = case_when(
    if_any(all_of(outcomes.hypertension), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.hypertension), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("hypertension") %>% capture()


# Dyslipidemia ----------------------------------------------------------------

write("\n--------------")
write("DYSLIPIDEMIA")
write("--------------")

write("note: for dyslipidemia, we converted f2chol and f2ldlch as binary variables based on the threshold defined in Abolhassani et al. (2017). ")
data <- data %>% mutate(f2chol = if_else(f2chol>6.5, 1, 0),
                        f2ldlch = if_else(f2ldlch>4.1, 1, 0))

outcomes.dyslipidemia <- c("f2hypolip", "f2chol", "f2ldlch")
lapply(outcomes.dyslipidemia, count_distinct_val) %>% capture()

write("note: in case of 'Not relevant' (f2hypolip=8) -> change to 0 \n  in case of 'Does not know' (f2hypolip=9) -> change to NA")
data <- data %>% mutate(f2hypolip = replace(f2hypolip, f2hypolip==8, 0),
                        f2hypolip = replace(f2hypolip, f2hypolip==9, NA))

write("definition: Total cholesterol > 6.5mmol/L (f2chol=1) and/or LDL-cholesterol > 4.1mmol/L (f2ldlch=1) hypolipidemic drug treatment (f2hypolip=1)")
data <- data %>% 
  mutate(dyslipidemia = case_when(
    if_any(all_of(outcomes.dyslipidemia), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.dyslipidemia), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("dyslipidemia") %>% capture()


# Polypharmacy ----------------------------------------------------------------

write("\n--------------")
write("POLYPHARMACY")
write("--------------")

outcomes.polypharmacy <- c("polypharm")
lapply(outcomes.polypharmacy, count_distinct_val) %>% capture()

write("definition: polypharmacy (polypharm=1)")
data <- data %>% 
  mutate(polypharmacy = if_else(polypharm==1,1,0))

count_unique_combinations("polypharmacy") %>% capture()


# Morbidity ----------------------------------------------------------------

write("\n--------------")
write("MORBIDITY")
write("--------------")

outcomes.morbidity <- c("cvd", "obesity", "diabetes", "hypertension", "dyslipidemia")
lapply(outcomes.morbidity, count_distinct_val) %>% capture()

write("definition: CVD and/or obesity and/or diabetes and/or hypertension and/or dyslipidemia")
data <- data %>% 
  mutate(morbidity = case_when(
    if_any(all_of(outcomes.morbidity), ~ .== 1) ~ 1, 
    if_all(all_of(outcomes.morbidity), ~ .== 0) ~ 0, 
    TRUE ~ NaN))

count_unique_combinations("morbidity") %>% capture()


# Multimorbidity ----------------------------------------------------------------

write("\n--------------")
write("MULTIMORBIDITY")
write("--------------")

outcomes.multimorbidity <- c("cvd", "obesity", "diabetes", "hypertension", "dyslipidemia")

write("definition: at least two conditions between CVD, obesity, diabetes, hypertension, dyslipidemia")
data <- data %>% 
  mutate(multimorbidity = case_when(
    rowSums(across(outcomes.multimorbidity),na.rm = TRUE) >= 2 ~ 1, 
    (rowSums(across(outcomes.multimorbidity),na.rm = TRUE) == 0) & (rowSums(is.na(across(outcomes.multimorbidity))) == 1) ~ 0, 
    (rowSums(across(outcomes.multimorbidity),na.rm = TRUE) < 2) & (rowSums(is.na(across(outcomes.multimorbidity))) >= 1) ~ NaN, 
    TRUE ~ 0))

count_unique_combinations("multimorbidity") %>% capture()


# Extract outcomes --------------------------------------------------------

write("\n--------------")
write("SUMMARY")
write("--------------")

outcomes.all <- c("cvd", "obesity", "diabetes", "hypertension", "dyslipidemia", "morbidity", "multimorbidity", "polypharmacy")

data.outcomes <- data %>% select(pt, all_of(outcomes.all))

lapply(outcomes.all, print_final_stats)

st_write(data.outcomes, "../processed_data/f2_outcomes.gpkg", driver='GPKG', delete_layer=TRUE)

DBI::dbDisconnect(con)