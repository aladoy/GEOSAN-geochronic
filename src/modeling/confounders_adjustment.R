# This code adjusts Follow-up 2 data for individual confounders (age, sex, poverty, education, nationality)
#   we use all Follow-up 2 data (not only the participants within the study area)

require(RPostgreSQL)
library(sf)
library(tidyverse)

source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('modeling/utils_confounders_adjustment.R')

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


# Individual covariates we want to adjust for.
#   Use financial difficulties instead of income because less missing values
cov.indiv <- c("age", "sex", "education", "difficulties", "swiss")
cov.outcomes <- c("cvd", "diabetes", "hypertension", "obesity", "dyslipidemia")
# cov.env <- c("PTOT", "GREEN_SP", "NOISE", "PM25", "MEDREV", "R_UNEMP", "R_NN_POBL", "R_FFB")

# Import outcomes and confounders separately & merge
data = load_participants(con) %>%
  dplyr::select(reli, pt, all_of(cov.outcomes), all_of(cov.indiv))


# We choose to adjust for age, sex, income, education, nationality because it was significant for most outcomes (that's why we dropped marital status).
# Furthermore, it is common to adjust for age, sex, socio-economic status and nationality (see Fleury et al.), and most significant "baseline" confounders (these confounders will seriously impact lifestyle behaviors and the place where you'll live)

# HYPERTENSION ------------------------------------------------------------

sink("../results/regression_models/hypertension/confounders_adjustment_hypertension.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

hyp.data <- select_outcome(data, "hypertension", cov.indiv)

plot_bin_cont_relationship(hyp.data, "hypertension", "age")
test_age_disease_linearity("hypertension", hyp.data)
print("Linear relationship between age and hypertension.")

hyp.log.1 <- glm(hypertension ~ age + sex + difficulties + education + swiss, data = hyp.data, family = "binomial")
print_model_summary(hyp.log.1)

hyp.data <- hyp.data %>% mutate(hypertension_adj = unname(resid(hyp.log.1, type = "pearson")))


# # Hypertension - Environment
# 
# hyp.ols.1 <- lm(hypertension_adj ~ PTOT + GREEN_SP + NOISE + PM25 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB, data=hyp.data)
# print_model_summary(hyp.ols.1)
# vif(hyp.ols.1)
# 
# hyp.ols.2 <- hyp.ols.1 %>% step( direction = "backward", trace = 1)
# 
# hyp.ols.3 <- lm(hypertension_adj ~ GREEN_SP + R_FFB + MEDREV, data=hyp.data)
# print_model_summary(hyp.ols.3)
# vif(hyp.ols.3)
# 
# print("The association with greenspaces is weird but we should compare with 2015 cadastral data. Otherwise, keep accidents and remove greenspaces.")
# 
# compare_models(hyp.ols.2, hyp.ols.3)

sink()



# OBESITY ------------------------------------------------------------

sink("../results/regression_models/obesity/confounders_adjustment_obesity.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

obes.data <- select_outcome(data, "obesity", cov.indiv)

plot_bin_cont_relationship(obes.data, "obesity", "age")
test_age_disease_linearity("obesity", obes.data)
print("Quadratic relationship between age and obesity.")

obes.log.1 <- glm(obesity ~ age + sex + difficulties + education + swiss, data = obes.data, family = "binomial")
print_model_summary(obes.log.1)

obes.log.2 <- update(obes.log.1,  '~ . + I(age^2)')
print_model_summary(obes.log.2)

compare_models(obes.log.1, obes.log.2)

obes.data <- obes.data %>% mutate(obesity_adj = unname(resid(obes.log.2, type = "pearson")))


sink()


# DIABETES ----------------------------------------------------------------

sink("../results/regression_models/diabetes/confounders_adjustment_diabetes.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

diab.data <- select_outcome(data, "diabetes", cov.indiv)

plot_bin_cont_relationship(diab.data, "diabetes", "age")
test_age_disease_linearity("diabetes", diab.data)
print("Quadratic relationship between age and diabetes.")

diab.log.1 <- glm(diabetes ~ age + sex + difficulties + education + swiss, data = diab.data, family = "binomial")
print_model_summary(diab.log.1)

diab.log.2 <- update(diab.log.1,  '~ . + I(age^2)')
print_model_summary(diab.log.2)

compare_models(diab.log.1, diab.log.2)

diab.data <- diab.data %>% mutate(diabetes_adj = unname(resid(diab.log.2, type = "pearson")))

sink()


# DYSLIPIDEMIA ------------------------------------------------------------

sink("../results/regression_models/dyslipidemia/confounders_adjustment_dyslipidemia.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

dys.data <- select_outcome(data, "dyslipidemia", cov.indiv)

plot_bin_cont_relationship(dys.data, "dyslipidemia", "age")
test_age_disease_linearity("dyslipidemia", dys.data)
print("Quadratic relationship between age and dyslipidemia.")


dys.log.1 <- glm(dyslipidemia ~ age + sex + difficulties + education + swiss, data = dys.data, family = "binomial")
print_model_summary(dys.log.1)

dys.log.2 <- update(dys.log.1,  '~ . + I(age^2)')
print_model_summary(dys.log.2)

compare_models(dys.log.1, dys.log.2)

dys.data <- dys.data %>% mutate(dyslipidemia_adj = unname(resid(dys.log.2, type = "pearson")))

sink()



# MERGE & SAVE ------------------------------------------------------------

# Left join with dataset
adjusted_outcomes <- data %>% dplyr::select(-all_of(cov.indiv)) %>%
  left_join(hyp.data %>% dplyr::select(pt, hypertension_adj), by='pt') %>%
  left_join(obes.data %>% dplyr::select(pt, obesity_adj), by='pt') %>%
  left_join(diab.data %>% dplyr::select(pt, diabetes_adj), by='pt') %>%
  left_join(dys.data %>% dplyr::select(pt, dyslipidemia_adj), by='pt') 


# threshold <- 4/length(diab.model$residuals)
# diab.data %>% filter(diabetes_cooks_dist > threshold)
# # Save potential outliers (influential observations) to file
# diab.outliers <- diab.data %>% filter(diabetes_cooks_dist > threshold)
# # inner_join(diab.outliers, outcomes %>% dplyr::select(pt, geometry), by="pt") %>% st_write("outliers.geojson")

# Save to file
st_write(adjusted_outcomes, paste0("../processed_data/f2_adjusted_outcomes.gpkg"), driver='GPKG', delete_layer=TRUE)

DBI::dbDisconnect(con)
