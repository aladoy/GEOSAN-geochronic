# This code computes the log relative risk surface of chronic diseases, and investigate associated individual & neighborhood characteristics.

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

require(svglite)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('descriptive/utils_spatial_clustering_outcomes.R')


con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


# LOAD DATASETS -----------------------------------------------------------

indiv <- load_participants(con)
laus <- load_boundaries(con)
ha <- load_hectares(con) 

dem <- raster("/mnt/data/GEOSAN/GEOSAN DB/data/MODELE NUMERIQUE HAUTEUR LAUSANNE/lausanne_dem_final_5m.tif")

# Basemap
build <- st_read(dsn="/mnt/data/GEOSAN/RESEARCH PROJECTS/COMMUNE EN SANTE @ UNISANTE/commune-en-sante/qgis/layers/admin_boundaries.gpkg", layer="vd_buildings")
build <- build %>% filter(st_intersects(., laus$extent, sparse = FALSE))

# Covariates
cov.indiv <- c("age", "sex", "swiss", "cohabiting", "education", "working", "difficulties", "smoking", "drinking", "inactivity")
cov.ha <- c("PTOT", "HTOT", "D_SPORT", "N_ACC_PED", "GREEN_SP", "NOISE", "PM25", "NO2", "P_65_M", "MEDREV", "R_UNEMP", "R_NN_POBL", "R_FFB", "R_NN_FRA", "AVG_PPH", "D_STOP_TOT")


# FUNCTIONS ---------------------------------------------------------------

compare_areas <- function(area_indiv, area_ha, cov_indiv, cov_ha){
  
  for(bin_var in cov.indiv[!cov_indiv %in% c("age")]){
    compare_risk_groups(area_indiv, bin_var, type="bin")
  }
  compare_risk_groups(area_indiv, "age", type="cont")
  
  for(cont_var in cov_ha){
    compare_risk_groups(area_ha, cont_var, type="cont")
  }
  
}


compare_high_risk_area <- function(lrr, cov_indiv, cov_ha, excluded_polys=NULL){
  
  hr_polys <- lrr$lrr.poly %>% filter(risk=="Higher risk") %>% pull(polyID)
  
  for(id in hr_polys[!hr_polys %in% excluded_polys]){
    
    cat("\n------------------------------------")
    cat(paste("\nHIGH-RISK AREA N°", id, "\n"))
    cat("------------------------------------\n")
    
    area.ha <- lrr$lrr.ha %>% mutate(risk=factor(if_else(polyID==id, "high", "other"))) %>% select(-c(polyID))
    stats <- area.ha %>% filter(risk=="high") %>% summarise(nPTOT=sum(PTOT, na.rm=TRUE), nHTOT = sum(HTOT, na.rm=TRUE))
    cat(paste("PTOT: ", stats %>% pull(nPTOT), "HTOT", stats %>% pull(nHTOT), "\n"))
    area.indiv <- lrr$lrr.indiv %>% mutate(risk=factor(if_else(polyID==id, "high", "other"))) %>% select(-c(polyID))
    
    compare_areas(area.indiv, area.ha, cov.indiv, cov.ha)
    
  }
  
}


# COMPARE KERNELS
x <- rnorm(100, mean = 0, sd = 1)
d_rect_300 <- density(x, kernel = "rectangular", bw = 300)
d_gauss_300 <- density(x, kernel = "gaussian", bw = 300)
d_gauss_200 <- density(x, kernel = "gaussian", bw = 200)
df_rect_300 <- data.frame(x = d_rect_300$x, y = d_rect_300$y)
df_gauss_300 <- data.frame(x = d_gauss_300$x, y = d_gauss_300$y)
df_gauss_200 <- data.frame(x = d_gauss_200$x, y = d_gauss_200$y)
ggplot() +
  geom_line(data = df_rect_300, aes(x = x, y = y, color = "Uniform kernel (b=300)")) +
  geom_line(data = df_gauss_300, aes(x = x, y = y, color = "Gaussian kernel (b=300)")) +
  geom_line(data = df_gauss_200, aes(x = x, y = y, color = "Gaussian kernel (b=100)")) +
  labs(x = "x",
       y = "Density") +
  scale_color_manual(values = c("#2dd4dc", "#092c86", "#f6546a"), name="Kernel") +
  theme_bw() +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))
ggsave("../doc/illustrate_kernel_choice.png")



# DIABETES ----------------------------------------------------------------

sink("../results/spatial_disease_risk/diabetes/spatial_variation_risk_diabetes.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

diab.data <- select_outcome_spatial(indiv, "diabetes", cov=cov.indiv)
plot_case_control(diab.data, laus$extent, build, title='Diabetes')

# Create PPP
diab.events <- create_ppp(diab.data, laus$extent, marks=diab.data$outcome, title="Diabetes - events")

# Log relative risk surface
optimal_bandwidths(diab.events)
diab.bandwidth <- 400

# png(paste0("../results/spatial_disease_risk/diabetes/controls_density_diabetes", diab.bandwidth, ".png"),
#     width=300, height=200, units=c("mm"), res=100)
# plot(diab.lrr$sparr_risk$f)
# #plot(diab.lrr$sparr_risk$g)
# dev.off()


diab.lrr <- log_ratio_spatial_dens(diab.events, diab.bandwidth, nsim=999, seed=12345, outcome_name="diabetes")

# Save rasters to create maps in QGIS
save_raster_lrr(diab.lrr, "diabetes", diab.bandwidth)


# SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS

# Extract data for risk areas
diab.areas <- polygonize_logrr(diab.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), diab.data)
map_extracted_areas(diab.lrr, diab.areas, "diabetes")

# Plot significant areas
map_significant_areas(diab.areas$lrr.poly, dem, basemap_type = "raster", title="Diabetes", bandwidth = diab.bandwidth)

# Compare individual and neighborhood factors between risk areas
compare_areas(diab.areas$lrr.indiv, diab.areas$lrr.ha, cov.indiv, cov.ha)


# SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA

# Check if some areas need to be merged
map_high_risk_areas(diab.areas$lrr.poly, laus$extent)

# Merge polygons 4-5
diab.areas <- merge_polygons(diab.areas, 4, 5)

map_high_risk_areas(diab.areas$lrr.poly, laus$extent)

compare_high_risk_area(diab.areas, cov.indiv, cov.ha, excluded_poly=c(4))

sink()


# CVD ----------------------------------------------------------------

sink("../results/spatial_disease_risk/cvd/spatial_variation_risk_cvd.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

cvd.data <- select_outcome_spatial(indiv, "cvd", cov=cov.indiv)
plot_case_control(cvd.data, window=laus$extent, title='CVD', buildings=build)

# Create PPP
cvd.events <- create_ppp(cvd.data, laus$extent, marks=cvd.data$outcome, title="CVD - events")

# Log relative risk surface
optimal_bandwidths(diab.events)
cvd.lrr <- log_ratio_spatial_dens(cvd.events, 400, nsim=999, seed=12345, outcome_name="cvd")

# SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS

# Extract data for risk areas
cvd.areas <- polygonize_logrr(cvd.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), cvd.data)
map_extracted_areas(cvd.lrr, cvd.areas, "cvd")

# Compare individual and neighborhood factors between risk areas
compare_areas(cvd.areas$lrr.indiv, cvd.areas$lrr.ha, cov.indiv, cov.ha)


