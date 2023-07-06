# This code run descriptive statistics about Colaus participants

library(tidyverse)
library(sf)
library(RPostgreSQL)
library(stats)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

# con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
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

comp_categorical <- function(vector1, vector2, var_name){
  
  write(var_name)
  
  data <- data.frame(
    group = c(rep("df1", length(vector1)), rep("df2", length(vector2))),
    val = c(vector1, vector2)
  )
  chisq.test(data$group, data$val) %>% capture()
  
}


# Extract participants ----------------------------------------------------

#Create file to store results
file_res=paste0("../results/code_outputs/describe_participants.txt")
cat(paste0("Date:", Sys.Date(),'\n'), file = file_res, append = FALSE) #Overwrite the file

indiv.b <- st_read("../processed_data/b_indiv_covariates.gpkg")

indiv.f2 <- read_sf(con, query="SELECT * FROM geochronic.f2_study_dataset_lausanne") # including sex, age, etc. from colaus_baseline

indiv.f2.outside <- st_read("../processed_data/f2_indiv_covariates.gpkg") %>% filter(!pt %in% indiv.f2$pt)


# Characteristics of participants -----------------------------------------

cov <- c("age", "sex", "swiss", "cohabiting", "education", "working", "income", "difficulties", "smoking", "drinking", "inactivity")
cov.b <- cov[!cov %in% c("difficulties", "income", "inactivity")]

write("\n--------------")
write("BASELINE")
write("--------------")

indiv.b <- indiv.b %>% select(pt, datexam, all_of(cov.b))

cov.b.fact <- cov.b[! cov.b %in% c("age")]
indiv.b <- indiv.b %>% mutate_at(cov.b.fact, as.factor)

write(paste("Number of individuals:", indiv.b %>% nrow()))
write(paste("Time range:", min(indiv.b$datexam), "/", max(indiv.b$datexam)))
write(paste("Min / Max age:", min(indiv.b$age), "/", max(indiv.b$age)))

lapply(cov.b, covariate_stats, df=indiv.b)


write("\n--------------")
write("FOLLOW-UP 2")
write("--------------")

indiv.f2 <- indiv.f2 %>% select(pt, f2datexam, all_of(cov))

cov.f2.fact <- cov[! cov %in% c("age", "income")]
indiv.f2 <- indiv.f2 %>% mutate_at(cov.f2.fact, as.factor)

write(paste("Number of individuals:", indiv.f2 %>% nrow()))
write(paste("Time range:", min(indiv.f2$f2datexam, na.rm=TRUE), "/", max(indiv.f2$f2datexam, na.rm=TRUE)))
write(paste("Min / Max age:", min(indiv.f2$age), "/", max(indiv.f2$age)))

lapply(cov, covariate_stats, df=indiv.f2)



write("\n--------------")
write("FOLLOW-UP 2 - IN / OUTSIDE STUDY AREA")
write("--------------")

cov.f2.out.fact <- cov[! cov %in% c("age", "income")]
indiv.f2.outside <- indiv.f2.outside %>% mutate_at(cov.f2.out.fact, as.factor)

write("Statistics for geocoded individuals living outside Lausanne")

write(paste("Number of individuals:", indiv.f2.outside %>% nrow()))
lapply(cov, covariate_stats, df=indiv.f2.outside)


t.test(indiv.f2$age, indiv.f2.outside$age)
comp_categorical(indiv.f2$sex, indiv.f2.outside$sex, "sex")
comp_categorical(indiv.f2$swiss, indiv.f2.outside$swiss, "swiss")
comp_categorical(indiv.f2$cohabiting, indiv.f2.outside$cohabiting, "cohabiting")
comp_categorical(indiv.f2$education, indiv.f2.outside$education, "education")
comp_categorical(indiv.f2$working, indiv.f2.outside$working, "working")
comp_categorical(indiv.f2$difficulties, indiv.f2.outside$difficulties, "difficulties")
comp_categorical(indiv.f2$smoking, indiv.f2.outside$smoking, "smoking")
comp_categorical(indiv.f2$drinking, indiv.f2.outside$drinking, "drinking")
comp_categorical(indiv.f2$inactivity, indiv.f2.outside$inactivity, "inactivity")


DBI::dbDisconnect(con)