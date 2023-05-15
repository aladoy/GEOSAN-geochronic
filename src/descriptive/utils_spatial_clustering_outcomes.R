require(spatstat)
require(smacpod)
require(sparr)
require(RPostgreSQL)
require(raster)
require(rstatix)
require(viridis)
require(tmap)
require(sf)
require(tidyverse)


# LOAD COLAUS DATA
load_participants <- function(con){
  
  data <- read_sf(con, query="SELECT *, ST_X(geometry) as coordx, ST_Y(geometry) as coordy FROM geochronic.f2_study_dataset_lausanne")

  # Convert to factors
  col <- names(data)
  cov.fact <- col[! col %in% c("pt", "f2datexam", "age", "reli", "has_moved_dist", "coordx", "coordy", "geometry")]
  data <- data %>% mutate_at(cov.fact, as.factor)
  
  cat(paste("Number of participants: ", nrow(data), "\n"))

  # Add noise to points location to prevent duplicated points.
  data$coordx <- jitter(data$coordx)
  data$coordy <- jitter(data$coordy)
  
  return(data)
}


# LOAD HECTARES
load_hectares <- function(con){
  
ha <- read_sf(con, query="SELECT *, F_65_74+F_75_84+F_85_M+M_65_74+M_75_84+M_85_M as P_65_M FROM geochronic.ha_characteristics WHERE st_intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))")
  
  # Convert neighborhood characteristics to upper case
  ha <- ha %>% rename_with(~ toupper(.), -c(reli,geometry))
  
  return(ha)
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



select_outcome_spatial <- function(df, outcome, cov=NULL){
  
  df <- df %>% mutate(outcome = factor(if_else(!!as.name(outcome)==1, "case", "control")))
  df <- df %>% dplyr::select(pt, outcome, reli, coordx, coordy, geometry, all_of(cov)) %>% arrange(outcome)
  
  print(paste("Outcome selected: ", outcome))
  print(paste("Prevalence: ", round(100*nrow(df %>% filter(outcome=="case"))/nrow(df),2)))
  
  return(df)
}


# MAP THE LOCATIONS OF CASES AND CONTROLS
plot_case_control <- function(df, extent, buildings, title='Disease'){
  
  counts <- table(df$outcome)
  
  ggplot() +
    geom_sf(data=extent, fill=NA, colour="black") +
    geom_sf(data=buildings, fill="grey", alpha=0.4, color="grey") +
    # add points layer
    geom_sf(data = df, aes(color = outcome), size=0.8) + 
    theme_void() +
    theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16), legend.position = "top") +
    scale_color_manual(title, 
                       values = c("#F8766D", "#00BFC4"),
                       labels = c(paste("case", " (n =", counts[1], ")", sep = ""), paste("control", " (n =", counts[2], ")", sep = "")
                       ))
  
  filename <- tolower(title)
  ggsave(paste0("../results/spatial_disease_risk/", filename, "/events_", filename, ".png"), 
         bg="white", width=300, height=250, units=c("mm"), dpi=100)
  # dev.off()
  
}


# MAP INCIDENCE AREAS
map_significant_areas <- function(lrr_poly, basemap, basemap_type = "vector", title="Disease", bandwidth=NaN){
  
  p <- plot_basemap(basemap, basemap_type)
  
  p +
    geom_sf(data = lrr_poly %>% filter(risk=="Reduced risk") %>% st_simplify(TRUE, 50), fill="#00BFC4", colour="#00BFC4", alpha=0.5) +
    geom_sf(data = lrr_poly %>% filter(risk=="Higher risk") %>% st_simplify(TRUE, 50), fill="#F8766D", colour="#F8766D", alpha=0.5) +
    theme_void()
  
  filename <- tolower(title)
  ggsave(paste0("../results/spatial_disease_risk/", filename, "/significant_areas_", filename, bandwidth, ".png"), 
         bg="white", width=300, height=250, units=c("mm"), dpi=100)

}


# PLOT BASEMAP
plot_basemap <- function(basemap, basemap_type){
  
  if(basemap_type=="vector"){
    p <-ggplot() +
      geom_sf(data=basemap, fill=NA, colour="black")
      #geom_sf(data=buildings, fill="grey", alpha=0.4, color="grey")
  }else if(basemap_type=="raster"){
    df <- as.data.frame(basemap, xy = TRUE, na.rm=FALSE)
    p <- ggplot() +
      geom_tile(mapping=aes(x, y, fill = lausanne_dem_final_5m, alpha=0.7), data=df, show.legend=FALSE) +
      scale_fill_gradientn(colours = gray.colors(10), na.value = "white")
  }
  
  return(p)
  
}


# CREATE A POINT PATTERN OBJECT FROM A DATAFRAME WITH SPECIFIED X- AND Y-COORDINATES.
create_ppp <- function(df, study_area, marks=NULL, title="Event locations"){
  
  window <- simplify.owin(as.owin(study_area$geometry), 50)
  
  df <- ppp(df$coordx, df$coordy, window = window, marks=marks, cex=0.5)
  
  return(df)
}


# SPLIT A MARKED PPP IN TWO PPP (ONE FOR EACH MARK = CASE / CONTROL)
split_ppp <- function(ppp){
  
  cases.ppp <- subset(ppp, marks=="case")
  controls.ppp <- subset(ppp, marks=="control")
  
  return(list(cases = cases.ppp, controls=controls.ppp))
}


# DIFFERENT CRITERIAS TO SELECT KERNEL BANDWIDTH
optimal_bandwidths <- function(ppp){
  
  cases.ppp  <- split_ppp(ppp)$cases
  controls.ppp  <- split_ppp(ppp)$controls
  
  cat("\nKERNEL BANDWIDTHS\n")
  
  cat("Scott's rule of thumb (spatstat) or Normal scale rule (sparr)\n")
  print(paste("For cases: ", bw.scott(cases.ppp, isotropic = FALSE)))
  print(paste("For controls: ", bw.scott(controls.ppp, isotropic = FALSE)))
  
  cat("\nBerman and Diggle (1989) cross validated bandwitdth selection (spatstat)\n")
  print(paste("For cases: ", bw.diggle(cases.ppp)))
  print(paste("For controls: ", bw.diggle(controls.ppp)))
  
  cat("\nJOINTLY OPTIMAL BANDWIDTHS\n")
  
  cat("\nKelsall and Diggle (1995) approximate minimisation of the MISE (sparr)\n")
  print(LSCV.risk(f=cases.ppp, g=controls.ppp, type="fixed", method="kelsall-diggle"))
  
  cat("\nDavies (1995) using plug-in approach to estimate the asymptotic MISE (sparr)\n")
  print(LSCV.risk(f=cases.ppp, g=controls.ppp, type="fixed", method="davies"))
  
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


# COMPUTE THE LOG RELATIVE RISK/DENSITY SURFACE
log_ratio_spatial_dens <- function(ppp, sigma, nsim=999, seed=12345, outcome_name="Disease"){
  
  cases.ppp  <- split_ppp(ppp)$cases
  controls.ppp  <- split_ppp(ppp)$controls
  
  set.seed(seed)
  
  # USING SPARR
  # Spatial relative risk/density ratio
  sparr_risk <- risk(cases.ppp, controls.ppp, h0=sigma, log=TRUE, doplot=FALSE, edge="diggle")
  # P-value surface based on Monte-Carlo permutations
  tol <- tolerance(rs=sparr_risk, method="MC", ITER=nsim, parallelise=10)
  # Visualize significant areas of elevated risk at different p-values
  
  png(paste0("../results/spatial_disease_risk/",outcome_name,"/sparr_risk_", outcome_name, sigma, ".png"),
      width=300, height=200, units=c("mm"), res=100)
  plot(sparr_risk, alpha=0.7)
  tol.contour(tol, test="upper", levels=c(0.01, 0.05),lty=1:2, add=TRUE) 
  dev.off()
  
  # USING SMACPOD
  smacpod_risk = logrr(ppp, sigma=sigma, at="pixels", edge=TRUE, casecontrol=TRUE, case="case", diggle=TRUE, nsim=nsim, level=0.9, test="two.sided")
  
  png(paste0("../results/spatial_disease_risk/",outcome_name,"/smacpod_risk_", outcome_name, sigma, ".png"),
      width=300, height=200, units=c("mm"), res=100)
  plot(smacpod_risk)
  dev.off()
  
  # Estimate overall clustering (p.166-167, Waller and Gotway(2004))
  cat("\nGlobal test of clustering using log ratio of spatial densities\n")
  print(logrr.test(smacpod_risk))
  
  results = list(sparr_risk = sparr_risk, sparr_tol = tol, smacpod_risk = smacpod_risk)
  return(results)
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
polygonize_logrr <- function(lrr, ha, indiv, keep=c("Reduced risk", "Not significant")){
  
  lrr_signif <- rasterToPolygons(raster(lrr$nrenv), dissolve=TRUE) %>% st_as_sf()
  st_crs(lrr_signif) <- 2056
  
  # polygons of high- and low- incidence areas
  lrr_signif <- lrr_signif %>% 
    st_cast("POLYGON") %>%
    rowid_to_column(var="polyID") %>%
    mutate(risk = case_when(layer==-1 ~ "Reduced risk", layer==0 ~ "Not significant", layer==1 ~ "Higher risk")) %>%
    mutate(risk = factor(risk, c("Higher risk", "Reduced risk", "Not significant"))) %>%
    dplyr::select(-layer) 
  
  objects_signif <- extract_data_for_area(lrr_signif, ha, indiv)
  ha_signif = objects_signif$ha %>% filter(risk %in% c("Higher risk", keep))
  indiv_signif = objects_signif$indiv %>% filter(risk %in% c("Higher risk", keep))

  return(list(lrr.poly=lrr_signif, lrr.ha=ha_signif, lrr.indiv=indiv_signif))
}


# SAVE RASTER FILES
save_raster_lrr <- function(lrr, outcome_name, bandwidth){
  
  sparr_tol.path <- paste0("../results/spatial_disease_risk/", outcome_name, "/sparr_tolerance_", outcome_name, bandwidth, ".tif")
  sparr_tol <- raster(lrr$sparr_tol)
  crs(sparr_tol) <- CRS("epsg:2056")
  writeRaster(sparr_tol, sparr_tol.path, format="GTiff", overwrite=TRUE)
  
  sparr_logrr.path <- paste0("../results/spatial_disease_risk/", outcome_name, "/sparr_logrr_", outcome_name, bandwidth, ".tif")
  sparr_logrr <- raster(lrr$sparr_risk$rr)
  crs(sparr_logrr) <- CRS("epsg:2056")
  writeRaster(sparr_logrr, sparr_logrr.path, format="GTiff", overwrite=TRUE)
  
}


# EXTRACT HA & INDIVIDUALS
extract_data_for_area <- function(area, ha, indiv){
  
  # hectares (spatial intersect)
  ha_signif <- ha %>% 
    st_join(area, join=st_intersects, largest=TRUE)

  # individuals (non spatial join with reli)
  indiv_signif <- indiv %>% 
    inner_join(ha_signif %>% 
                 st_drop_geometry() %>% 
                 select(reli, polyID, risk), by="reli", relationship = "many-to-many")
  
  return(list(ha=ha_signif, indiv=indiv_signif))
}


# MAP EXTRACTED_DATA
map_extracted_areas <- function(lrr, areas_signif, outcome_name, risk_cat=c("Higher risk", "Reduced risk")){
  
  png(paste0("../results/spatial_disease_risk/",outcome_name,"/lrr_signif_extraction_", outcome_name,".png"),
      width=300, height=200, units=c("mm"), res=100)
  plot(lrr$smacpod_risk, main=paste("outcome :",outcome_name))
  plot(areas_signif$lrr.ha %>% dplyr::filter(risk %in% risk_cat), col="transparent", add=TRUE) 
  plot(areas_signif$lrr.indiv %>% dplyr::filter(risk %in% risk_cat), pch=16, col="grey", cex=0.6, add=TRUE)
  dev.off()
  
}


# PLOT HIGH RISK POLYGONS
map_high_risk_areas <- function(lrr_poly, baselayer){
  
  lrr_poly <- lrr_poly %>% filter(risk=="Higher risk")
  
  lrr_poly$centroid <- st_centroid(lrr_poly$geometry)
  
  ggplot(data=lrr_poly) + 
    geom_sf(data=baselayer) +
    geom_sf(data=lrr_poly, fill="#F8766D") +
    geom_label(x = st_coordinates(lrr_poly$centroid)[, 1],
             y = st_coordinates(lrr_poly$centroid)[, 2],
             label=lrr_poly$polyID) +
    theme_void()
  
}


# MERGE TWO POLYGONS
merge_polygons <- function(lrr, id_1, id_2){
  
  # assuming id is in polyID attribute
  lrr$lrr.poly[lrr$lrr.poly$polyID==id_1,]$geometry = lrr$lrr.poly %>% 
    filter(polyID %in% c(id_1,id_2)) %>% st_union()
  lrr$lrr.poly <- lrr$lrr.poly %>% filter(polyID != id_2)
  
  # change attribution for lrr.ha and lrr.indiv
  lrr$lrr.ha <- lrr$lrr.ha %>% mutate(polyID=if_else(polyID==id_2, id_1, polyID))
  lrr$lrr.indiv <- lrr$lrr.indiv %>% mutate(polyID=if_else(polyID==id_2, id_1, polyID))
  
  return(list(lrr.poly=lrr$lrr.poly, lrr.ha=lrr$lrr.ha, lrr.indiv=lrr$lrr.indiv))
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



# COMPARE THE CHARACTERISTICS OF RISK INCIDENCE AREAS
compare_risk_groups <- function(sets, var, type="cont"){
  
  cat("\n---")
  cat(var)
  cat("\n")
  
  variable <- sets %>% pull(var)
  areas <- sets %>% pull(risk)
  
  
  if(type=="cont"){
  
    # Kruskal-Wallis rank sum test
    kw <- kruskal.test(variable, areas)
    print(kw)
    
    cat("Median in each group\n")
    print(aggregate(as.formula(paste(var, "~ risk")), data=sets, median))
    
    # If significant: post-hoc Wilcoxon rank-sum test comparisons adjusting for multiple comparison (Bonferroni)
    if(kw$p.value < 0.05){
      print(pairwise.wilcox.test(variable, areas, p.adjust.method = "bonf"))
    }
      
  }
  
  else if(type=="bin"){
    
    # Pearson's Chi-squared test
    ct <- chisq.test(areas, variable)
    print(ct)
    
    table <- table(variable, areas)
    print(table)
    
    if(dim(table)[1]==3){
      prop <- apply(table, 2, function(x) round(100*(x[3]/sum(x)),2)) # 3rd row because low education
      cat("Proportion in each group\n")
      print(prop)}
    else{
      prop <- apply(table, 2, function(x) round(100*(x[2]/sum(x)),2))
      cat("Proportion in each group\n")
      print(prop) 
    }
    
    # If significant: post-hoc pairwise chi-squared tests between groups adjusting for multiple comparison (Bonferroni)
    if(ct$p.value < 0.05){
      
      if(dim(table)[1]==3){
        cat("High - Medium education\n")
        print(pairwise_prop_test(table[1:2,], p.adjust.method = "bonf"))
        cat("Medium - Low education\n")
        print(pairwise_prop_test(table[2:3,], p.adjust.method = "bonf"))
        cat("High - Low education\n")
        print(pairwise_prop_test(table[c(1,3),], p.adjust.method = "bonf"))
      }else{
        print(pairwise_prop_test(table, p.adjust.method = "bonf"))
      }

    }
  }
  
  cat("\n")
  
}