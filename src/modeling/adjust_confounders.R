# This code adjusts Follow-up 2 data for individual confounders (age, sex, poverty)
#   we use all Follow-up 2 data (not only the participants within the study area)

library(lme4)
require(RPostgreSQL)
library(sf)
library(tidyverse)
library(car)

source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('modeling/utils_model_individual_outcomes.R')

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


# Individual covariates we want to adjust for.
#   Use financial difficulties instead of income because less missing values
cov.indiv <- c("age", "sex", "swiss", "cohabiting", "education", "difficulties")
cov.outcomes <- c("cvd", "diabetes", "hypertension", "obesity", "dyslipidemia")
cov.env <- c("PTOT", "GREEN_SP", "NOISE", "PM25", "NO2", "MEDREV", "R_UNEMP", "R_NN_POBL", "R_FFB", "R_NN_FRA")

# Import outcomes and confounders separately & merge
data = read_sf(con, query="SELECT * FROM geochronic.f2_study_dataset_lausanne") %>%
  dplyr::select(reli, pt, all_of(cov.outcomes), all_of(cov.indiv))

# Add environmental characteristics
ha = read_sf(con, query="SELECT reli, PTOT, GREEN_SP, NOISE, PM25, NO2, MEDREV, R_UNEMP, R_NN_POBL, R_FFB, R_NN_FRA, geometry FROM geochronic.ha_characteristics WHERE st_intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))")
names(ha)[names(ha) != c("reli", "geometry")] <- toupper(names(ha)[names(ha) != c("reli", "geometry")])

# Merge
data <- inner_join(ha %>% st_drop_geometry(), data, by="reli") %>% st_drop_geometry()

# Standardize environmental variables
data_std <- data %>%
  mutate_at(cov.env, ~ scale(.)[,1])



select_outcome <- function(df, outcome, confounders, envs){

  df <- df %>% 
    dplyr::select(pt, !!as.name(outcome), all_of(confounders),  all_of(envs)) %>%
    filter(!is.na(!!as.name(outcome)))
  
  columns_to_convert <- setdiff(confounders, c("age", "pt"))
  df[columns_to_convert] <- lapply(df[columns_to_convert], factor)
  
  print(paste("Outcome selected: ", outcome))
  print(paste("Number of events: ", nrow(df)))
  print(paste("Prevalence: ", round(100*nrow(df %>% filter(!!as.name(outcome)==1))/nrow(df),2)))
  
  return(df)
}


# Visualize linear relationship between a binary and a continuous variable
plot_bin_cont_relationship <- function(df, bin, cont){
 
  ggplot(df, aes(x=!!as.symbol(cont), y=as.integer(!!as.symbol(bin)))) + geom_point() +
    stat_summary_bin(fun='mean', bins=15,
                     color='orange', size=2, geom='point')
   
}


# Compare models by adding quadratic and cubic terms, as suggested by Osborne et al., 2015
test_age_disease_linearity <- function(disease, df){
  
  linear <- glm(as.formula(paste0(disease, '~ age')) , data=df, family="binomial")
  quadratic <- update(linear, '~ . + I(age^2)')
  print(compare_models(linear, quadratic))
  
  cubic <- update(quadratic, '~ . + I(age^3)')
  print(compare_models(quadratic, cubic))
  
}



# CARDIOVASCULAR DISEASES -------------------------------------------------

sink("../results/regression_models/cvd/spatial_variation_risk_cvd.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

cvd.data <- select_outcome(data_std, "cvd", cov.indiv, cov.env)

# CVD adjustment
plot_bin_cont_relationship(cvd.data, "cvd", "age")
print("The graph age / CVD may indicate a non linear relationship between the two variables.")

test_age_disease_linearity("cvd", cvd.data)
print("Adding quadratic and cubic term do result in a substantial improvment of the model so we keep the linear term only.")

cvd.log.1 <- glm(cvd ~ age + sex, data = cvd.data, family = "binomial")
print_model_summary(cvd.log.1)
vif(cvd.log.1)

cvd.log.2 <- update(cvd.log.1,  '~ . + education + difficulties')
print_model_summary(cvd.log.2)
compare_models(cvd.log.1, cvd.log.2)
vif(cvd.log.2)

print("Remove financial difficulties who do not seem to contribute to CVD risk.")

cvd.log.3 <- update(cvd.log.2,  '~ . + swiss + cohabiting')
print_model_summary(cvd.log.3)
compare_models(cvd.log.2, cvd.log.3)
vif(cvd.log.3)

cvd.log.4 <- update(cvd.log.1, '~ . + education')
print_model_summary(cvd.log.4)
compare_models(cvd.log.1, cvd.log.4)


cvd.model.adj <- cvd.log.4
cvd.data <- cvd.data %>% mutate(cvd_adj = unname(resid(cvd.model.adj, type = "pearson")))

# OLS with characteristics of the living environment

cvd.ols.1 <-lm(cvd_adj ~ PTOT + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB + R_NN_FRA, data=cvd.data)
print_model_summary(cvd.ols.1)
vif(cvd.ols.1)

print("Based on the presence of high collinearity (VIF > 10) between PM2.5 and NO2, and considering that NO2 demonstrated a larger effect size in comparing risk areas, it is advisable to exclude PM2,5 from the analysis.")

cvd.ols.2 <- update(cvd.ols.1, '~ . - PM25')
print_model_summary(cvd.ols.2)
vif(cvd.ols.2)

cvd.ols.3 <- cvd.ols.2 %>% step( direction = "backward", trace = 1)

cvd.ols.4 <- lm(cvd_adj ~ R_NN_POBL, data=cvd.data)
print_model_summary(cvd.ols.4)

compare_models(cvd.ols.2, cvd.ols.4)

# cvd.test <- glm(cvd ~ age  + sex + PTOT + NB_ACDNT + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB + R_NN_FRA, data=cvd.data, family="binomial")
# print_model_summary(cvd.test)



# DIABETES ----------------------------------------------------------------

sink("../results/regression_models/diabetes/spatial_variation_risk_cvd.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

diab.data <- select_outcome(data_std, "diabetes", cov.indiv, cov.env)

# Diabetes adjustment
plot_bin_cont_relationship(diab.data, "diabetes", "age")
print("The graph age / diabetes indicates a non linear relationship between the two variables.")

test_age_disease_linearity("diabetes", diab.data)
print("The relationship between age / diabetes is better captured with a quadratic term but the difference between the quadratic and cubic effect is small so we'll favor model simplicity.")

diab.log.1 <- glm(diabetes ~ age + I(age^2) + sex, data = diab.data, family = "binomial")
print_model_summary(diab.log.1)

diab.log.2 <- update(diab.log.1,  '~ . + education + difficulties')
print_model_summary(diab.log.2)
compare_models(diab.log.1, diab.log.2)

diab.log.3 <- update(diab.log.2,  '~ . + swiss + cohabiting')
print_model_summary(diab.log.3)
compare_models(diab.log.2, diab.log.3)

diab.log.4 <- update(diab.log.2,  '~ . + swiss')
print_model_summary(diab.log.4)
compare_models(diab.log.2, diab.log.4)

print("Adding marital status and nationality do not contribute substantially to the model's performance and may introduce unnecessary complexity so we keep the former model.")

diab.model.adj <- diab.log.2

print("Adjust diabetes for age, sex, education, and income")

diab.data <- diab.data %>% mutate(diabetes_adj = unname(resid(diab.model.adj, type = "pearson")))


# OLS with characteristics of the living environment

diab.ols.1 <- lm(diabetes_adj ~ PTOT + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB + R_NN_FRA, data=diab.data)
print_model_summary(diab.ols.1)
vif(diab.ols.1)

print("Based on the presence of high collinearity (VIF > 10) between PM2.5 and NO2, and considering that PM2.5 demonstrated a larger effect size in comparing risk areas, it is advisable to exclude NO2 from the analysis.")

diab.ols.2 <- update(diab.ols.1, '~ . - NO2')
print_model_summary(diab.ols.2)
vif(diab.ols.2)

diab.ols.3 <- diab.ols.2 %>% step( direction = "backward", trace = 1)

diab.ols.4 <- lm(diabetes_adj ~ GREEN_SP + R_NN_POBL, data=diab.data)
print_model_summary(diab.ols.4)

sink()



# HYPERTENSION ------------------------------------------------------------

sink("../results/regression_models/hypertension/spatial_variation_risk_cvd.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

hyp.data <- select_outcome(data_std, "hypertension", cov.indiv, cov.env)

# Hypertension adjustment

plot_bin_cont_relationship(hyp.data, "hypertension", "age")
print("The graph age / diabetes indicates a linear relationship between the two variables.")

hyp.log.1 <- glm(hypertension ~ age + sex, data = hyp.data, family = "binomial")
print_model_summary(hyp.log.1)

hyp.log.2 <- update(hyp.log.1,  '~ . + education + difficulties')
print_model_summary(hyp.log.2)
compare_models(hyp.log.1, hyp.log.2)

hyp.log.3 <- update(hyp.log.1,  '~ . + education')
print_model_summary(hyp.log.3)
compare_models(hyp.log.2, hyp.log.3)

print("Do not adjust hypertension for financial difficulties.")

hyp.log.4 <- update(hyp.log.3,  '~ . + swiss + cohabiting')
print_model_summary(hyp.log.4)
compare_models(hyp.log.3, hyp.log.4)

hyp.log.5 <- update(hyp.log.3,  '~ . + swiss')
print_model_summary(hyp.log.5)
compare_models(hyp.log.3, hyp.log.5)

print("Adjust hypertension for age, sex, education, and nationality.")

hyp.model.adj <- hyp.log.5
hyp.data <- hyp.data %>% mutate(hypertension_adj = unname(resid(hyp.model.adj, type = "pearson")))

# Hypertension - Environment

hyp.ols.1 <- lm(hypertension_adj ~ PTOT + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB + R_NN_FRA, data=hyp.data)
print_model_summary(hyp.ols.1)
vif(hyp.ols.1)

hyp.ols.2 <- update(hyp.ols.1, '~ . - NO2')
print_model_summary(hyp.ols.2)
vif(hyp.ols.2)

hyp.ols.3 <- hyp.ols.2 %>% step( direction = "backward", trace = 1)

hyp.ols.4 <- lm(hypertension_adj ~ GREEN_SP + R_NN_FRA + R_FFB + MEDREV, data=hyp.data)
print_model_summary(hyp.ols.4)
vif(hyp.ols.4)

print("The association with greenspaces is weird but we should compare with 2015 cadastral data. Otherwise, keep accidents and remove greenspaces.")

compare_models(hyp.ols.2, hyp.ols.4)

sink()



# OBESITY ------------------------------------------------------------

sink("../results/regression_models/obesity/spatial_variation_risk_cvd.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

obes.data <- select_outcome(data_std, "obesity", cov.indiv, cov.env)

# Obesity adjustment

plot_bin_cont_relationship(obes.data, "obesity", "age")
print("The graph age / diabetes may indicate non linear relationship between the two variables.")

test_age_disease_linearity("obesity", obes.data)
print("The relationship between age / diabetes is better captured with a cubic term.")

obes.log.1 <- glm(obesity ~ age + I(age^2) + I(age^3) + sex, data = obes.data, family = "binomial")
print_model_summary(obes.log.1)

obes.log.2 <- update(obes.log.1,  '~ . + education + difficulties')
print_model_summary(obes.log.2)
compare_models(obes.log.1, obes.log.2)

obes.log.3 <- update(obes.log.2,  '~ . + swiss + cohabiting')
print_model_summary(obes.log.3)
compare_models(obes.log.2, obes.log.3)

obes.log.4 <- update(obes.log.2,  '~ . + swiss')
print_model_summary(obes.log.4)
compare_models(obes.log.2, obes.log.4)

print("Adjust obesity for age, sex, education, income, and nationality.")

obes.model.adj <- obes.log.4
obes.data <- obes.data %>% mutate(obesity_adj = unname(resid(obes.model.adj, type = "pearson")))


# Obesity - Environment

obes.ols.1 <- lm(obesity_adj ~ PTOT + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB + R_NN_FRA, data=obes.data)
print_model_summary(obes.ols.1)
vif(obes.ols.1)

obes.ols.2 <- update(obes.ols.1, '~ . - NO2')
print_model_summary(obes.ols.2)
vif(obes.ols.2)

obes.ols.3 <- obes.ols.2 %>% step( direction = "backward", trace = 1)

obes.ols.4 <- lm(obesity_adj ~ MEDREV + PTOT, data=obes.data)
print_model_summary(obes.ols.4)
vif(obes.ols.4)

sink()



# DYSLIPIDEMIA ------------------------------------------------------------

sink("../results/regression_models/dyslipidemia/spatial_variation_risk_cvd.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file


dysli.data <- select_outcome(data_std, "dyslipidemia", cov.indiv, cov.env)

# Dyslipidemia adjustment

plot_bin_cont_relationship(dysli.data, "dyslipidemia", "age")
print("The graph age / diabetes may indicate non linear relationship between the two variables.")
test_age_disease_linearity("dyslipidemia", dysli.data)
print("The relationship between age / diabetes is best captured with a quadratic term.")

dysli.log.1 <- glm(dyslipidemia ~ age + I(age^2) + sex, data = dysli.data, family = "binomial")
print_model_summary(dysli.log.1)

dysli.log.2 <- update(dysli.log.1,  '~ . + education + difficulties')
print_model_summary(dysli.log.2)
compare_models(dysli.log.1, dysli.log.2)

dysli.log.3 <- update(dysli.log.1,  '~ . + education')
print_model_summary(dysli.log.3)
compare_models(dysli.log.2, dysli.log.3)

dysli.log.4 <- update(dysli.log.3,  '~ . + swiss + cohabiting')
print_model_summary(dysli.log.4)
compare_models(dysli.log.3, dysli.log.4)

dysli.log.5 <- update(dysli.log.3,  '~ . + swiss')
print_model_summary(dysli.log.5)
compare_models(dysli.log.3, dysli.log.5)

dysli.model.adj <- dysli.log.5
dysli.data <- dysli.data %>% mutate(dyslipidemia_adj = unname(resid(dysli.model.adj, type = "pearson")))


# Dyslipidemia - Environment

dysli.ols.1 <- lm(dyslipidemia_adj ~ PTOT + GREEN_SP + NOISE + PM25 + NO2 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB + R_NN_FRA, data=dysli.data)
print_model_summary(dysli.ols.1)
vif(dysli.ols.1)

dysli.ols.2 <- update(dysli.ols.1, '~ . - PM25')
print_model_summary(dysli.ols.2)
vif(dysli.ols.2)

dysli.ols.3 <- dysli.ols.2 %>% step( direction = "backward", trace = 1)

dysli.ols.4 <- lm(dyslipidemia_adj ~ R_FFB + R_NN_FRA, data=dysli.data)
print_model_summary(dysli.ols.4)


sink()



# MERGE & SAVE ------------------------------------------------------------

# Left join with dataset
adjusted_outcomes <- data %>% dplyr::select(-all_of(cov.indiv)) %>%
  left_join(cvd.data %>% dplyr::select(pt, cvd_adj), by='pt') %>% 
  left_join(diab.data %>% dplyr::select(pt, diabetes_adj), by='pt') %>%
  left_join(hyp.data %>% dplyr::select(pt, hypertension_adj), by='pt') %>%
  left_join(obes.data %>% dplyr::select(pt, obesity_adj), by='pt') %>%
  left_join(dysli.data %>% dplyr::select(pt, dyslipidemia_adj), by='pt') 


# threshold <- 4/length(diab.model$residuals)
# diab.data %>% filter(diabetes_cooks_dist > threshold)
# # Save potential outliers (influential observations) to file
# diab.outliers <- diab.data %>% filter(diabetes_cooks_dist > threshold)
# # inner_join(diab.outliers, outcomes %>% dplyr::select(pt, geometry), by="pt") %>% st_write("outliers.geojson")

# Save to file
st_write(adjusted_outcomes, paste0("../processed_data/f2_adjusted_outcomes.gpkg"), driver='GPKG', delete_layer=TRUE)

DBI::dbDisconnect(con)
