# This code run descriptive statistics about Colaus participants

library(tidyverse)
library(sf)
require(RPostgreSQL)


# FUNCTIONS ---------------------------------------------------------------

write <- function(text){
  cat(paste(text, "\n\n"), file=file_res, append=TRUE)
}

capture <- function(result){
  capture.output(result, file=file_res, append=TRUE)
}

retrieve_dataset <- function(table_name, con){
  query <- paste0("SELECT * FROM syndemic.", table_name," WHERE NOT ST_IsEmpty(geometry);")
  table <- read_sf(con, query=query)
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

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= rstudioapi::askForPassword("Database user"),rstudioapi::askForPassword("Database password"),dbname="geosan")

indiv.b <- retrieve_dataset("colaus_b", con)
indiv.b <- indiv.b %>% mutate(datexam = as.Date(datexam))

indiv.f2 <- retrieve_dataset("colaus_f2_complete", con) # including sex, age, etc. from colaus_baseline
indiv.f2  <- indiv.f2  %>% mutate(f2datexam = as.Date(f2datexam))

# Define individual covariates --------------------------------------------

indiv.b <- indiv.b %>% mutate(
  age = age,
  sex = sex,
  swiss = brnsws,
  marital = mrtsts2,
  education = edtyp4,
  smoking = sbsmk,
  alcohol = case_when(conso_hebdo == 0 ~0, 
                      (conso_hebdo>=1 & conso_hebdo<=13) ~1, 
                      (conso_hebdo>=14 & conso_hebdo<=34) ~2,
                      conso_hebdo >= 35 ~3),
  phyact = replace(phyact, phyact==9, NA),
  inactivity = if_else(phyact %in% c(0,1), 1, 0)
)


indiv.f2 <- indiv.f2 %>% mutate(
  age = f2age,
  sex = f2sex,
  swiss = brnsws,
  marital = f2mrtsts2,
  education = edtyp4,
  f2income5 = replace(f2income5, f2income5==9, NA),
  difficulties = if_else(f2income5 %in% c(0,1), 0, 1),
  smoking = f2sbsmk,
  alcohol = f2alcool2,
  inactivity = f2seden,
  bmi = f2bmi_cat2
)



# Characteristics of participants -----------------------------------------

write("\n--------------")
write("BASELINE")
write("--------------")

cov.b <- c("age", "sex", "swiss", "marital", "education", "smoking", "alcohol", "inactivity")
indiv.b <- indiv.b %>% select(pt, datexam, all_of(cov.b))

write(paste("Number of individuals:", indiv.b %>% nrow()))
write(paste("Time range:", min(indiv.b$datexam), "/", max(indiv.b$datexam)))

lapply(cov.b, covariate_stats, df=indiv.b)


write("\n--------------")
write("FOLLOW-UP 2")
write("--------------")

cov.f2 <- c("age", "sex", "swiss", "marital", "education", "difficulties", "smoking", "alcohol", "inactivity", "bmi")
indiv.f2 <- indiv.f2 %>% select(pt, f2datexam, all_of(cov.f2))

write(paste("Number of individuals:", indiv.f2 %>% nrow()))
write(paste("Time range:", min(indiv.f2$f2datexam, na.rm=TRUE), "/", max(indiv.f2$f2datexam, na.rm=TRUE)))

lapply(cov.f2, covariate_stats, df=indiv.f2)

# Save follow-up 2 individual covariates
st_write(indiv.f2, "../processed_data/f2_indiv_covariates.gpkg", driver='GPKG', delete_layer=TRUE)

DBI::dbDisconnect(con)