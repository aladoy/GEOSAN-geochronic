library(stats)
library(caret)

# LOAD COLAUS DATA
load_participants <- function(con){
  
  data <- st_read("../processed_data/events_f2_analysis.geojson") # data used in spatial clustering analysis
  
  cat(paste("Number of participants: ", nrow(data), "\n"))
  
  return(data)
}


select_outcome <- function(df, outcome, confounders, envs=NULL){
  
  cat("\n")
  df <- df %>% 
    st_drop_geometry() %>%
    dplyr::select(pt, !!as.name(outcome), all_of(confounders),  all_of(envs)) %>%
    filter(!is.na(!!as.name(outcome)))
  
  columns_to_convert <- setdiff(confounders, c("age", "pt"))
  df[columns_to_convert] <- lapply(df[columns_to_convert], factor)
  
  # Change level for education (ref: low)
  df$education <- relevel(df$education, ref = "3")
  
  
  print(paste("Outcome selected: ", outcome))
  print(paste("Number of events: ", nrow(df)))
  print(paste("Prevalence: ", round(100*nrow(df %>% filter(!!as.name(outcome)==1))/nrow(df),2)))
  cat("\n")
  
  return(df)
}


# Visualize linear relationship between a binary and a continuous variable
plot_bin_cont_relationship <- function(df, bin, cont){
  
  cat("\nAGE-OUTCOME RELATIONSHIP\n")
  
  ggplot(df, aes(x=!!as.symbol(cont), y=as.integer(!!as.symbol(bin)))) + geom_point() +
    stat_summary_bin(fun='mean', bins=15,
                     color='orange', size=2, geom='point')
  
}


# Compare models by adding quadratic and cubic terms, as suggested by Osborne et al., 2015
test_age_disease_linearity <- function(disease, df){
  cat("\n")
  linear <- glm(as.formula(paste0(disease, '~ age')) , data=df, family="binomial")
  quadratic <- update(linear, '~ . + I(age^2)')
  print(compare_models(linear, quadratic))
  # cat("\n")
  # cubic <- update(quadratic, '~ . + I(age^3)')
  # print(compare_models(quadratic, cubic))
  cat("\n")
}


compare_models <- function(m1, m2){
  
  # AIC - should be minimized
  # McFadden's Pseudo-R2 - should be maximized
  cat("\n")
  print("Model 1")
  cat(paste0("AIC: " , round(AIC(m1),5), "\n"))
  cat(mcfadden(m1))
  
  print("Model 2")
  cat(paste0("AIC: " , round(AIC(m2),5), "\n"))
  cat(mcfadden(m2))
  
  # Chi-Square test - should be significant
  print(anova(m1, m2, test="Chisq"))
  cat("\n")
}


# PRINT MODEL SUMMARY (estimates, p-values, OR, confidence intervals)
#   type can be "gam" or "glm"
print_model_summary <- function(model, type="glm"){
  
  print(summary(model))
  mcfadden(model)
  
  if(type=="glm"){
    print(round(exp(cbind(OR = coef(model), confint(model))),2))
  }else{
    print(round(exp(OR = coef(model)),2))
  }
  
  
}




# COMPUTE THE McFADDEN CRITERIA (OR LIKELIHOOD RATIO TEST).
#   determine the quality of the logistic regression model.
#   generally, a score (R2) > 0.2 is considered a good fit for a logistic regression model.
mcfadden <- function(model){
  
  ll.null <- model$null.deviance/-2
  ll.proposed <- model$deviance/-2
  score <- round((ll.null-ll.proposed)/ll.null, 5)
  
  # print(paste("Overall effect size of the model=", (ll.null-ll.proposed)/ll.null))
  # print(paste("P-value=", 1-pchisq(2*(ll.proposed-ll.null), df=(length(model$coefficients)-1))))
  cat(paste("McFadden criteria (Likelihood ratio test):",
            score, "\n"))
}