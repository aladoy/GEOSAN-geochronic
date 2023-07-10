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



# # CARDIOVASCULAR DISEASES -------------------------------------------------
# 
# sink("../results/regression_models/cvd/spatial_variation_risk_cvd.txt")
# cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file
# 
# cvd.data <- select_outcome(data_std, "cvd", cov.indiv, cov.env)
# 
# # CVD adjustment
# plot_bin_cont_relationship(cvd.data, "cvd", "age")
# print("The graph age / CVD may indicate a non linear relationship between the two variables.")
# 
# test_age_disease_linearity("cvd", cvd.data)
# print("Adding quadratic and cubic term do result in a substantial improvment of the model so we keep the linear term only.")
# 
# cvd.log.1 <- glm(cvd ~ age + sex, data = cvd.data, family = "binomial")
# print_model_summary(cvd.log.1)
# vif(cvd.log.1)
# 
# cvd.log.2 <- update(cvd.log.1,  '~ . + education + difficulties')
# print_model_summary(cvd.log.2)
# compare_models(cvd.log.1, cvd.log.2)
# vif(cvd.log.2)
# 
# print("Remove financial difficulties who do not seem to contribute to CVD risk.")
# 
# cvd.log.3 <- update(cvd.log.2,  '~ . + swiss + cohabiting')
# print_model_summary(cvd.log.3)
# compare_models(cvd.log.2, cvd.log.3)
# vif(cvd.log.3)
# 
# cvd.log.4 <- update(cvd.log.1, '~ . + education')
# print_model_summary(cvd.log.4)
# compare_models(cvd.log.1, cvd.log.4)
# 
# 
# cvd.model.adj <- cvd.log.4
# cvd.data <- cvd.data %>% mutate(cvd_adj = unname(resid(cvd.model.adj, type = "pearson")))
# 
# # OLS with characteristics of the living environment
# 
# cvd.ols.1 <-lm(cvd_adj ~ PTOT + GREEN_SP + NOISE + PM25 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB, data=cvd.data)
# print_model_summary(cvd.ols.1)
# vif(cvd.ols.1)
# 
# cvd.ols.2 <- cvd.ols.1 %>% step( direction = "backward", trace = 1)
# 
# cvd.ols.3 <- lm(cvd_adj ~ R_NN_POBL, data=cvd.data)
# print_model_summary(cvd.ols.3)
# 
# compare_models(cvd.ols.1, cvd.ols.3)
# 
# # cvd.test <- glm(cvd ~ age  + sex + PTOT + GREEN_SP + NOISE + PM25 + MEDREV + R_UNEMP + R_NN_POBL + R_FFB, data=cvd.data, family="binomial")
# # print_model_summary(cvd.test)


# # Add environmental characteristics
# ha = read_sf(con, query="SELECT reli, PTOT, GREEN_SP, NOISE, PM25, MEDREV, R_UNEMP, R_NN_POBL, R_FFB, geometry FROM geochronic.ha_characteristics WHERE st_intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))")
# names(ha)[names(ha) != c("reli", "geometry")] <- toupper(names(ha)[names(ha) != c("reli", "geometry")])
# 
# # Merge
# data <- inner_join(ha %>% st_drop_geometry(), data, by="reli") %>% st_drop_geometry()


# assess_normality <- function(df, var){
#   distrib <- ggplot(df) +
#     geom_histogram(aes(x = !!sym(var)), fill = "steelblue", alpha = 0.5) +
#     theme_minimal() 
#   print(distrib)
#   shapiro.test(df[[var]])
# }
# 
# assess_normality(data, "GREEN_SP")
# assess_normality(data, "PTOT")
# assess_normality(data, "NOISE")
# assess_normality(data, "PM25")
# assess_normality(data, "NO2")
# assess_normality(data, "MEDREV")
# assess_normality(data, "R_UNEMP")
# assess_normality(data, "R_NN_POBL")
# assess_normality(data, "R_FFB")
# assess_normality(data, "R_NN_FRA")
# 
# log_vars <- c("PM25", "NO2")
# 
# # Standardize environmental variables
# data_std <- data %>%
#   mutate_at(log_vars, ~ log(.))
# 
# data_std <- data %>%
#   mutate_at(cov.env, ~ scale(.)[,1])


