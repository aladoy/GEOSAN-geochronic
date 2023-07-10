require(stats)
require(sf)
require(MASS)
require(tidyverse)
require(caret)
require(DBI)
require(mgcv)

# LOAD COLAUS DATA
load_participants <- function(con, laus_only=TRUE, drop_geom=TRUE){
  
  data <- read_sf(con, query="SELECT *, ST_X(geometry) as coordx, ST_Y(geometry) as coordy FROM geochronic.f2_study_dataset")
  reli.laus <- dbGetQuery(con, "SELECT reli FROM lausanne_reli_polygon") %>% pull()
  
  # Add noise to points location to prevent duplicated points.
  data <- data %>% 
    mutate(coordx=jitter(coordx), coordy=jitter(coordy))
  
  # Drop geometry
  if(drop_geom == TRUE){
    data <- data %>% st_drop_geometry()
  }
  
  
  # Rename several factors and create a factor for moving > 300m (bandwidth used for spatial autocorrelation of environmental covariates) 
  data <- data %>% 
    mutate(sex = if_else(sex==0, "W", "M"), 
           education = case_when(education==1 ~ "High", education==2 ~ "Medium", education==3 ~ "Low")) %>%
    mutate(out_laus = if_else(reli %in% reli.laus, 0, 1)) 
  
  if(laus_only == TRUE){
    data <- data %>% filter(out_laus == 0)
  }
  
  return(data)
}


# LOAD GEOGRAPHIC BOUNDARIES
load_boundaries <- function(con){
  
  boundary.laus <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
  boundary.vd <- read_sf(con, query="SELECT * FROM vd_canton")
  
  return(list(laus=boundary.laus, vaud=boundary.vd))
}


# ADD ENVIRONMENTAL COVARIATES
add_env_info <- function(indiv){
  
  env <- read_sf("../processed_data/neighborhood_vd.gpkg")

  data <- left_join(indiv, 
                    env %>% st_drop_geometry(), 
                    by="reli")
  
  paste("Number of participants removed because no covariate data for corresponding RELI: ", 
        data %>% filter(is.na(D_SPORT)) %>% nrow())
  
  data <- data %>% filter(!is.na(D_SPORT))
 
  return(data) 
}


# FILTER DATASET FOR A SPECIFIED OUTCOME
select_outcome_regression <- function(df, outcome, cov_indiv, cov_env){
  
  df <- df %>% 
    mutate_at(cov_indiv[! cov_indiv == "age"], as.factor) %>% 
    mutate(diabetes = factor(!!as.name(outcome), levels = c("0", "1"))) %>%
    dplyr::select(pt, outcome, !!as.name(outcome), all_of(cov.indiv), coordx, coordy, all_of(cov.env), reli)
  
  print(paste("Outcome selected: ", outcome))
  print(paste("Prevalence: ", round(100*nrow(df %>% filter(!!as.name(outcome)=="1"))/nrow(df),2)))
  
  return(df)
}


# SPLIT DATASET INTO TRAIN AND TEST SUBSET
create_train_dataset <- function(df, frac=0.80, seed=NULL){
  
  if(is.null(seed)){
    train <- df %>% dplyr::sample_frac(frac) 
  }else{
    set.seed(seed)
    train <- df %>% dplyr::sample_frac(frac)
  }
  test  <- dplyr::anti_join(df, train, by = 'pt')
  
  # Add pt as rownmames
  train <- train %>% dplyr::select(-pt)
  test <- test %>% dplyr::select(-pt)
  
  res <- list("train" = train, "test" = test)
  return(res)
  
  # paste("Number of cases in test: ", test %>% filter(diabetes==1) %>% nrow())
}


# DISTRIBUTION OF CONTINUOUS COVARIATES WITHIN CASES AND CONTROLS
plot_cov_distribution <- function(data, outcome, continous_cov){
  
  data_long <- data %>% 
    dplyr::select(!!as.name(outcome), all_of(continous_cov)) %>% 
    gather(key = "Variable", value = "Value", - !!as.name(outcome))
  
  ggplot(data_long, aes(x = Value, y = !!as.name(outcome))) +
    geom_violin() +
    labs(x="", y=outcome) + 
    facet_wrap(~ Variable, scales = "free_x") 
  
}





# Return the invesrse logit. For a given log-odd, it returns the probability of having a disease. Rounded at 2-decimals.
inverse_logit <- function(log_odd){
  
  p = exp(log_odd) / (1 + exp(log_odd))
  p = round(p,2)
  return(p)
}





# RUN STEPWISE REGRESSION
#  if trace=0, the selection procedure is not printed (only the final model is)
stepwise_logistic_regression <- function(data, outcome, cov, trace=1){
  
  data <- data %>% dplyr::select(!!as.name(outcome), all_of(cov))
  
  glm <- glm(as.formula(paste(outcome, "~ .")),
             data = data,
             family = binomial()) %>%
    stepAIC(direction = "backward", trace=trace)
  
  print_model_summary(glm, type="glm")
  
  return(glm)
}


draw_predicted_prob <- function(model, data, outcome_name){
  
  predicted_data <- data.frame(prob.outcome = model$fitted.values, outcome=data[, outcome_name])
  predicted_data <- predicted_data %>% arrange(prob.outcome)
  predicted_data$rank <- 1:nrow(predicted_data)
  
  ggplot(data=predicted_data, aes(x=rank, y=prob.outcome)) +
    geom_point(aes(color=!!as.name(outcome_name)), alpha=1, shape=4, stroke=1) +
    xlab("Index") +
    ylab("Predicted probability of having diabetes")
}


# violin_plot <- function(data, continuous_var, outcome_var){
#   
#   data %>% 
#     ggplot(aes(x=!!as.name(continuous_var), y=!!as.name(outcome_var))) +
#     geom_violin() +
#     labs(title=paste("Relationship between", outcome_var, "and", continuous_var))
#   
# }




# EMPTY MULTILEVEL
empty_multilevel <- function(data, outcome, level2id){

  m <- glmer(as.formula(paste(outcome, "~ (1 |", level2id, ")")),
             data=data, family=binomial("logit"))
  
  print(summary(m))

  icc <- m@theta[1]^2/ (m@theta[1]^2 + (3.14159^2/3))
  cat(paste("ICC:", icc, "\n"))
  
  return(m)
}


# CROSS-VALIDATION
cross_validation <- function(model, test_data, outcome, pred_threshold){
  
  pred <- predict(model, newdata=test_data, type="response")
  
  cm <- confusionMatrix(factor(as.numeric(pred>pred_threshold)), 
                  test_data %>% pull(!!as.name(outcome)),
                  positive = "1")
  
  print(cm)
}


