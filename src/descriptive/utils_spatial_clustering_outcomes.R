require(spatstat)
require(smacpod)
require(RPostgreSQL)
require(raster)
require(tmap)
require(sf)
require(tidyverse)


# LOAD COLAUS DATA
load_participants <- function(con){
  
  data.all <- read_sf(con, query="SELECT *, ST_X(geometry) as coordx, ST_Y(geometry) as coordy FROM geochronic.f2_study_dataset")
  data.laus <- read_sf(con, query="SELECT f.*, ST_X(f.geometry) as coordx, ST_Y(f.geometry) as coordy FROM geochronic.f2_study_dataset f, lausanne_sectors_extent l WHERE st_intersects(f.geometry, l.geometry)")
  
  cat(paste("Number of participants in Lausanne: ", nrow(data.laus), "\n"))
  cat(paste("Pourcentage of the study dataset: ", round(100*nrow(data.laus)/nrow(data.all), 2)))
  
  # Convert to factors
  col <- names(data.laus)
  cov.fact <- col[! col %in% c("pt", "f2datexam", "age", "reli", "has_moved_dist", "coordx", "coordy", "geometry")]
  data.laus <- data.laus %>% mutate_at(cov.fact, as.factor)
  
  # Add noise to points location to prevent duplicated points.
  data.laus$x <- jitter(data.laus$coordx)
  data.laus$y <- jitter(data.laus$coordy)
  
  return(data.laus)
  }


# LOAD GEOGRAPHIC BOUNDARIES
load_boundaries <- function(con){
  
  study_area.df <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
  sectors.laus <- read_sf(con, query="SELECT * FROM lausanne_sectors")
  
  return(list(extent=study_area.df, sectors=sectors.laus))
}


# LOAD NEIGHBORHOOD CHARACTERISTICS
load_env <- function(){
  
  env <- read_sf("../processed_data/neighborhood_lausanne.gpkg")
  
  return(env)
}



select_outcome <- function(df, outcome, cov=NULL){
  
  df <- df %>% mutate(outcome = factor(if_else(!!as.name(outcome)==1, "case", "control")))
  df <- df %>% dplyr::select(pt, outcome, reli, x, y, geometry, all_of(cov)) %>% arrange(outcome)
  
  print(paste("Outcome selected: ", outcome))
  print(paste("Prevalence: ", round(100*nrow(df %>% filter(outcome=="case"))/nrow(df),2)))
  
  return(df)
}


# MAP THE LOCATIONS OF CASES AND CONTROLS
plot_case_control <- function(df, window=NULL, title='Disease'){
  
  ggplot() +
    # add polygon layer
    geom_sf(data = window, fill = "#f5f5f2") +
    # add points layer
    geom_sf(data = df, aes(color = outcome), size=0.5) + 
    theme_void() +
    scale_color_manual(title, values = c("#F8766D", "#00BFC4"))
  
}


# CREATE A POINT PATTERN OBJECT FROM A DATAFRAME WITH SPECIFIED X- AND Y-COORDINATES.
create_ppp <- function(df, study_area, marks=NULL, title="Event locations"){
  
  window <- simplify.owin(as.owin(study_area$geometry), 50)
  
  df <- ppp(df$x, df$y, window = window, marks=marks, cex=0.5)
  plot(df, main=title)
  
  return(df)
}


# COMPUTE THE (GAUSSIAN) KERNEL SMOOTHED SPATIAL DENSITY OF POINTS 
#  use the Jones-Diggle formula for edge correction
#  use the Scott's Rule for Bandwidth Selection
spatial_density <- function(ppp){
  
  density <- spdensity(ppp, sigma=bw.scott, at="pixels", kernel="gaussian", edges=TRUE,  diggle=TRUE)
  
  return(density)
}


# MAP THE SPATIAL DENSITY OF BOTH CASES AND CONTROLS SEPARATLY
compare_spatial_dens <- function(cases, controls, title="Spatial density of case-control events"){
 
  cat("Scott's rule bandwidth for cases\n")
  print(bw.scott(cases, isotropic=FALSE, d=NULL))
  cat("Scott's rule bandwidth for controls\n")
  print(bw.scott(controls, isotropic=FALSE, d=NULL))
  
  cases.density <- spatial_density(cases)
  controls.density <- spatial_density(controls)
  plot(cases.density)
  plot(controls.density)
  
}


# COMPUTE THE LOG-RELATIVE RISK SURFACE
#   the value of sigma can be chosen as a compromise between the sigmas of cases and controls density (function spatial_density).
log_ratio_spatial_dens <- function(ppp, sigma){
  
  # Construct log relative risk surface
  lrr =  relrisk(ppp, sigma=sigma, at="pixels", edge=TRUE, casecontrol=TRUE, relative=TRUE, diggle=TRUE, control="control")
  plot(lrr, "Log relative risk of cases vs. controls")
  contour(lrr, add=TRUE)
  # Test for random labeling hypothesis
  lrr.signif = logrr(ppp, sigma=sigma, at="pixels", edge=TRUE, casecontrol=TRUE, relative=TRUE, case="case", diggle=FALSE, nsim=999, level=0.9)
  plot(lrr.signif)
  # Estimate overall clustering (p.166-167, Waller and Gotway(2004))
  logrr.test(lrr.signif)
  
  return(lrr.signif)
}


# PLOT THE DIFFERENCE BETWEEN THE K-FUNCTIONS OF CASE AND CONTROL PROCESSES
k_func_diff <- function(ppp_events){
  
  kd = kdest(ppp_events, case = "case")
  plot(kd, cbind(iso, theo) ~ r, legend = FALSE, main = "")
  
  kdenv <- kdest(ppp_events, case = "case", nsim = 999, level = 0.9, correction = "border")
  plot(kdenv)
  kdplus.test(kdenv)
  
}


# POLYGONIZE LOG RELATIVE RISK SURFACE
polygonize_logrr <- function(lrr, ha){
  
  lrr_signif <- rasterToPolygons(raster(lrr$nrenv), dissolve=TRUE) %>% st_as_sf()
  st_crs(lrr_signif) <- 2056
  
  # polygons of high- and low- incidence areas
  lrr_signif <- lrr_signif %>% 
    mutate(risk = case_when(layer==-1 ~ "Reduced risk", layer==0 ~ "No significant difference", layer==1 ~ "Higher risk")) %>%
    mutate(risk = factor(risk, c("Higher risk", "Reduced risk", "No significant difference"))) %>%
    dplyr::select(-layer) 
  
  # hectares within high- and low- incidence areas
  ha_signif <- lrr_signif %>% st_join(ha, join=st_intersects) %>% filter(risk %in% c("Higher risk", "Reduced risk"))
  
  return(list(lrr.poly=lrr_signif, lrr.ha=ha_signif))
}


# MAP INCIDENCE AREAS
map_significant_areas <- function(lrr_poly, baselayer){
  
  ggplot() +
    geom_sf(data=baselayer, fill=NA, colour="black") +
    geom_sf(data = lrr_poly %>% filter(risk=="Reduced risk") %>% st_simplify(TRUE, 50), fill="#00BFC4", colour="#00BFC4", alpha=0.5) +
    geom_sf(data = lrr_poly %>% filter(risk=="Higher risk") %>% st_simplify(TRUE, 50), fill="#F8766D", colour="#F8766D", alpha=0.5) +
    theme_void()
  
}


# COMPARE RISK AREAS WITH BOXPLOTS
compare_risk_areas <- function(lrr_ha, cov_env){
  
  data_long <- lrr_ha %>% 
    st_drop_geometry() %>% 
    dplyr::select(risk, all_of(cov_env)) %>% 
    gather(key = "Variable", value = "Value", -risk)
  
  ggplot(data_long, aes(x = risk, y = Value, fill=risk, outlier.size=0.01)) +
    geom_boxplot() + 
    labs(x="", y="") +
    facet_wrap(~ Variable, scales = "free_y") +
    theme(legend.position="none", text = element_text(size=9))
  
  
}



# COMPARE THE CHARACTERISTICS OF HIGH- AND LOW- INCIDENCE AREAS
compare_risk_groups <- function(lrr_ha, cov_env){

  lrr_ha <- lrr_ha %>% st_drop_geometry() %>% dplyr::select(risk, reli, all_of(cov_env))
  
  for (var in names(lrr_ha)[!names(lrr_ha) %in% c("risk", "reli")]) {
    
    cat(var)
    cat("\n")
    
    print(wilcox.test(as.formula(paste(var, "~ risk")), data=lrr_ha, alternative = "two.sided"))
    
    cat("\n")
  }
  
  # m <- lm(as.formula(paste(var_name, "~ risk")), data = sf)
  # anova <- aov(m)
  # print(summary(anova))
  # # Run a Tukey HSD test to identify which groups differ significantly
  # tukey <- TukeyHSD(anova)
  # print(tukey$risk)
  
}