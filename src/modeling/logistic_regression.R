# This code runs a logistic regression on CoLaus participants

library(stats)
require(RPostgreSQL)
library(MapGAM)
library(MASS)
library(mgcv)
library(sf)
library(tidyverse)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

# IMPORT DATA -------------------------------------------------------------

# Import F2 Colaus dataset
data.sf <- read_sf(con, query="SELECT *, ST_X(geometry) as coordx, ST_Y(geometry) as coordy FROM geochronic.f2_study_dataset")
# Import information about the living environment
env.sf <- read_sf("/mnt/data/GEOSAN/RESEARCH PROJECTS/COMMUNE EN SANTE @ UNISANTE/commune-en-sante/results/hectares/ha_indicators.gpkg", layer="qgis_project")
study_area.df <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
#study_area.df <- as(study_area.df, "Spatial")

# Filter data that lie within Lausanne
data.sf <- data.sf %>% st_filter(study_area.df$geometry, .predicates = st_intersects)
# Add env. attributes
data.sf <- st_join(data.sf, env.sf, join=st_within)

# Drop geo
data <- data.sf %>% st_drop_geometry()

# Convert binary / categorical variables
col <- names(data)
factor_var <- col[! col %in% c("pt", "f2datexam", "age", "reli", "has_moved_dist", "coordx", "coordy", "geometry")]
data <- data %>% mutate_at(factor_var, as.factor)

# Jitter data
data$x <- jitter(data$coordx)
data$y <- jitter(data$coordy)

data <- data %>% select(diabetes, x, y ,c("age", "sex", "swiss", "cohabiting", "education", "difficulties", "smoking", "drinking", "inactivity"))
data <- as.data.frame(data)

# LOGISTIC --------------------------

m1 <- mgcv::gam(diabetes ~ lo(x,y), data=data, method="REML", family=binomial())
summary(m1)

m2 <- mgcv::gam(diabetes ~ lo(x,y) + s(age) + age + sex + swiss + difficulties + education + smoking + inactivity  , data=data, method="REML", family=binomial())
summary(m2)

m3 <- mgcv::gam(diabetes ~ age + sex + swiss + difficulties + education + smoking + inactivity  , data=data, method="REML", family=binomial())
summary(m3)


plot(study_area.df)
points(data$x, data$y, col=data$diabetes)

predgrid(data)


predgrid <- predgrid(data[, c("x","y")], map=as(study_area.df, "Spatial"))
m1 <- modgam(diabetes~lo(x, y), data=data, rgrid=predgrid, sp=0.2, type="all")
summary(m1)
plot(m1, exp=TRUE, data, contours = "response")

m2 <- modgam(diabetes~ age + sex + lo(x, y), data=data, rgrid=predgrid, sp=0.2, type="all")
summary(m2)
plot(m2, exp=TRUE, data, contours = "response")

m_ex <- modgam(Case~lo(Xcoord, Ycoord), data=MAdata, rgrid=gamgrid, sp=NULL, type="all")

glm.empty <- glm(diabetes ~ 1, data=data, family=binomial())
summary(glm.empty)
glm.basic <- update(model.empty, ~age+sex)
summary(glm.basic)
glm.indiv <- update(glm.basic, ~ + swiss + cohabiting + education + difficulties + smoking + drinking + inactivity)
summary(glm.indiv)

glm.step <- glm(diabetes ~ age + sex + swiss + cohabiting + education + difficulties + smoking + drinking + inactivity, data=data, family=binomial()) %>% stepAIC( direction = "backward")
glm.final <- glm(diabetes ~ age + sex + swiss + smoking + difficulties + inactivity + education, data=data, family=binomial())

summary(glm.final)

anova(glm.empty, glm.basic, test="LRT")
anova(glm.basic, glm.final, test="LRT")


exp(coef(summary(glm.final)))
exp(confint(glm.final, level=0.95))

# Generate predicted probabilities
glm.final$predicted_prob <- predict(glm.final, type = "response")

# View predicted probabilities
head(glm.final$predicted_prob)


# Split to train and test dataset
set.seed(123)
train <- data %>% dplyr::sample_frac(0.80) 
test  <- dplyr::anti_join(data, train, by = 'pt')

# Add pt as rownmames
train <- train %>% dplyr::select(-pt)
test <- test %>% dplyr::select(-pt)


# Select cov with outcome
covariates <- c("age", "sex", "swiss", "cohabiting", "education", "difficulties", "smoking", "drinking", "inactivity")
data <- data %>% dplyr::select(pt, diabetes, all_of(covariates))
# GAM ---------------------------------------------------------------------

m <- ppm(diabetes ~ age, data=data, use.gam=TRUE)


# LOGISTIC REGRESSION -----------------------------------------------------

# Full model
model.full <- glm(diabetes ~ ., data = data, family = binomial())
summary(model.full)
# exp(cbind(OR = coef(model.full), confint(model.full)))
# coef(model.full)

# Empty model
model.empty <- glm(diabetes ~ 1, data=train, family=binomial())
summary(model.empty)

# Perform stepwise (backward) logistic regression
model.backward <- model.full %>% step(direction='backward')
summary(model.backward)
coef(model.backward)

# Perform stepwise (backward) logistic regression
model.forward <- model.full %>% step(direction='forward')
summary(model.forward)
coef(model.backward)

# Remove cohabiting
model.final <- glm(diabetes ~ age + sex + education + difficulties + smoking + inactivity, data = data, family = binomial())
summary(model.final)
exp(cbind(OR = coef(model.final), confint(model.final)))


# Prediction accuracy - Full model
prob.full <- model.full %>% predict(test, type = "response")
pred.full <- ifelse(prob.full > 0.5, 1, 0)
obs.full <- test$diabetes
mean(pred.full == obs.full)


# Prediction accuracy - Stepwise model
prob.step <- model.step %>% predict(test, type = "response")
pred.step <- ifelse(prob.step > 0.5, 1, 0)
obs.step <- test$diabetes
mean(pred.step == obs.step)


data.geo <- read_sf(con, query="SELECT f.*, CAST(l.pkuid AS DECIMAL ) FROM geochronic.f2_study_dataset f INNER JOIN lausanne_sectors l  ON ST_Intersects(f.geometry, l.geometry)")
data <- data.geo %>% st_drop_geometry()
data <- data %>% mutate_at(c("pt", "pkuid", "reli"), as.factor)
data <- data %>% mutate_at(factor_var, as.factor)
data <- data %>% mutate(diabetes=as.integer(diabetes))

a <- data %>% group_by(pkuid) %>% summarise(n())

data <- data %>% filter(!pkuid %in% c(7,17,43,55))

data <- data %>% mutate(diabetes=if_else(pkuid %in% 0:20, 1,0))


# EMPTY MODEL -------------------------------------------------------------

M0 <- glmer(morbidity ~ age + sex + smoking + (1| pkuid), data=data, family=binomial("logit"))

summary(M0)

icc <- M0@theta[1]^2/ (M0@theta[1]^2 + (3.14159^2/3))
icc

spat.corr.diagnostic(multimorbidity~1,
                     units.m=~ntest,data=rb,
                     coords=~I(utm_x/1000)+I(utm_y/1000),
                     likelihood = "Binomial",
                     lse.variogram = TRUE)




# LOGISTIC REGRESSION -----------------------------------------------------


full_model = glm(multimorbidity ~ age + sex + swiss + marital + education + difficulties + smoking + alcohol + inactivity, family = 'binomial', data = data)
final_model = stepAIC(full_model, direction="backward")

summary(final_model)

fullModel = glm(X1 ~ ., family = 'binomial', data = dat) # model with all 9 variables
nullModel = glm(X1 ~ 1, family = 'binomial', data = dat) # model with the intercept only
summary(stepAIC(fullModel, # start with a model containing all variables
                direction = 'backward', # run backward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0)) # do not show the step-by-step process of model selection