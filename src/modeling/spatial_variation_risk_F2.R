# This code computes the log relative risk surface of chronic diseases, and investigate associated individual & neighborhood characteristics.

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

require(svglite)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('modeling/utils_spatial_variation_risk.R')

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


# LOAD DATASETS -----------------------------------------------------------

indiv <- load_participants(con, period_shrt="f2", seed=199991)
# st_write(indiv, "../processed_data/events_f2_analysis.geojson", delete_dsn = TRUE)
indiv <- st_read("../processed_data/events_f2_analysis.geojson")

# Convert to factors
col <- names(indiv)
cov.fact <- col[! col %in% c("pt", "f2datexam", "age", "reli", "has_moved_dist", "coordx", "coordy", "geometry")]
indiv <- indiv %>% mutate_at(cov.fact, as.factor)

laus <- load_boundaries(con)
ha <- load_hectares(con) 

# Basemaps
build <- st_read(dsn="/mnt/data/GEOSAN/RESEARCH PROJECTS/COMMUNE EN SANTE @ UNISANTE/commune-en-sante/qgis/layers/admin_boundaries.gpkg", layer="vd_buildings")
build <- build %>% filter(st_intersects(., laus$extent, sparse = FALSE))
dem <- raster("/mnt/data/GEOSAN/GEOSAN DB/data/MODELE NUMERIQUE HAUTEUR LAUSANNE/lausanne_dem_final_5m.tif")

# Covariates
cov.indiv <- c("age", "sex", "swiss", "cohabiting", "education", "working", "difficulties", "smoking", "drinking", "inactivity")
cov.ha <- c("PTOT", "INTDEN", "GREEN_SP", "NOISE", "PM25", "NO2", "MEDREV", "R_UNEMP", "R_NN_POBL", "R_NN_CH")

cov.ha.env <- c("INTDEN", "GREEN_SP", "NOISE", "PM25")
cov.ha.env.labels <- c("Intersection density [-]", "Greenness [%]", "Nighttime traffic noise [dB]", "PM2.5 exposure [ug/m3]")
cov.ha.soc <- c("MEDREV", "R_UNEMP", "R_NN_POBL", "R_NN_CH")
cov.ha.soc.labels <- c("Median income [kCHF]", "Unemployment [%]", "Compulsory education [%]", "Foreign population [%]")

compare_kernels("gaussian", 100, "gaussian", 300, "rectangular", 300)

print(paste("Number of FU2 individuals in Lausanne:", nrow(indiv)))



# HYPERTENSION ----------------------------------------------------------------

sink("../results/spatial_disease_risk/hypertension/f2/spatial_variation_risk_hypertension.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

hyp.data <- select_outcome_spatial(indiv, "hypertension", cov=cov.indiv)
plot_case_control(hyp.data, laus$extent, build, title='Hypertension')

# Create PPP
hyp.events <- create_ppp(hyp.data, laus$extent, marks=hyp.data$outcome, title="Hypertension - events")

# Log relative risk surface
optimal_bandwidths(hyp.events)

# # Compare bandwidths (for supplementary materials)
# cat("\nCompare different bandwidths\n")
# for (bw in seq(100, 600, 100)){
#   cat(paste0("\nBandwidth: ", bw, " meters\n"))
#   hyp.lrr <- log_ratio_spatial_dens(hyp.events, bw, nsim=999, seed=12345, outcome_name="hypertension")
#   global_clustering(hyp.lrr$smacpod_risk)
# } 

hyp.bandwidth <- 200 # (also compare the following stats with 300m)

cat(paste0("\nSelected bandwidth: ", hyp.bandwidth, "\n"))

hyp.lrr <- log_ratio_spatial_dens(hyp.events, hyp.bandwidth, nsim=999, seed=12345, outcome_name="hypertension")
global_clustering(hyp.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(hyp.lrr, "hypertension", hyp.bandwidth)


# SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS

# Extract data for risk areas
hyp.areas <- polygonize_logrr(hyp.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), hyp.data)
map_extracted_areas(hyp.lrr, hyp.areas, "hypertension", bandwidth = hyp.bandwidth)

# Plot significant areas
map_significant_areas(hyp.areas$lrr.poly, dem, basemap_type = "raster", title="Hypertension", bandwidth = hyp.bandwidth)

# Compare individual and neighborhood factors between risk areas
compare_areas(hyp.areas$lrr.indiv, hyp.areas$lrr.ha, cov.indiv, cov.ha)
pairwise_violin_plot(hyp.areas$lrr.ha, cov.ha.env, cov.ha.env.labels, "hypertension", bandwidth = hyp.bandwidth)
pairwise_violin_plot(hyp.areas$lrr.ha, cov.ha.soc, cov.ha.soc.labels, "hypertension", bandwidth = hyp.bandwidth)

# SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA

# Check if some areas need to be merged / and removed areas without disease events
map_high_risk_areas(hyp.areas$lrr.poly, laus$extent, "hypertension", bandwidth = hyp.bandwidth)
compare_high_risk_area(hyp.areas, cov.indiv, cov.ha, excluded_polys=c(10))

cov.ha.env.labels

sink()


# OBESITY ----------------------------------------------------------------

sink("../results/spatial_disease_risk/obesity/f2/spatial_variation_risk_obesity.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

obes.data <- select_outcome_spatial(indiv, "obesity", cov=cov.indiv)
plot_case_control(obes.data, laus$extent, build, title='Obesity')

# Create PPP
obes.events <- create_ppp(obes.data, laus$extent, marks=obes.data$outcome, title="Obesity - events")

# Log relative risk surface
optimal_bandwidths(obes.events)

# # Compare bandwidths (for supplementary materials)
# cat("\nCompare different bandwidths\n")
# for (bw in seq(100, 600, 100)){
#   cat(paste0("\nBandwidth: ", bw, " meters\n"))
#   obes.lrr <- log_ratio_spatial_dens(obes.events, bw, nsim=999, seed=12345, outcome_name="obesity")
#   global_clustering(obes.lrr$smacpod_risk)
# }

obes.bandwidth <- 200

cat(paste0("\nSelected bandwidth: ", obes.bandwidth, "\n"))

obes.lrr <- log_ratio_spatial_dens(obes.events, obes.bandwidth, nsim=999, seed=12345, outcome_name="obesity")
global_clustering(obes.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(obes.lrr, "obesity", obes.bandwidth)

# SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS

# Extract data for risk areas
obes.areas <- polygonize_logrr(obes.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), obes.data)
map_extracted_areas(obes.lrr, obes.areas, "obesity", bandwidth=obes.bandwidth)

# Plot significant areas
map_significant_areas(obes.areas$lrr.poly, dem, basemap_type = "raster", title="Obesity", bandwidth = obes.bandwidth)

# Compare individual and neighborhood factors between risk areas
compare_areas(obes.areas$lrr.indiv, obes.areas$lrr.ha, cov.indiv, cov.ha)
pairwise_violin_plot(obes.areas$lrr.ha, cov.ha.env, cov.ha.env.labels, "obesity", bandwidth=obes.bandwidth)
pairwise_violin_plot(obes.areas$lrr.ha, cov.ha.soc, cov.ha.soc.labels, "obesity", bandwidth = obes.bandwidth)

# SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA

# Check if some areas need to be merged
map_high_risk_areas(obes.areas$lrr.poly, laus$extent, "obesity", bandwidth = obes.bandwidth)

compare_high_risk_area(obes.areas, cov.indiv, cov.ha, excluded_polys = c(18))

sink()


# DIABETES ----------------------------------------------------------------

sink("../results/spatial_disease_risk/diabetes/f2/spatial_variation_risk_diabetes.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

diab.data <- select_outcome_spatial(indiv, "diabetes", cov=cov.indiv)
plot_case_control(diab.data, laus$extent, build, title='Diabetes')

# Create PPP
diab.events <- create_ppp(diab.data, laus$extent, marks=diab.data$outcome, title="Diabetes - events")

# Log relative risk surface
optimal_bandwidths(diab.events)

# # Compare bandwidths (for supplementary materials)
# cat("\nCompare different bandwidths\n")
# for (bw in seq(100, 600, 100)){
#   cat(paste0("\nBandwidth: ", bw, " meters\n"))
#   diab.lrr <- log_ratio_spatial_dens(diab.events, bw, nsim=999, seed=12345, outcome_name="diabetes")
#   global_clustering(diab.lrr$smacpod_risk)
# }

diab.bandwidth <- 200 
cat(paste0("\nSelected bandwidth: ", diab.bandwidth, "\n"))

diab.lrr <- log_ratio_spatial_dens(diab.events, diab.bandwidth, nsim=999, seed=12345, outcome_name="diabetes")
global_clustering(diab.lrr$smacpod_risk)

# # Save cases / controls density for presentations
# png(paste0("../results/spatial_disease_risk/diabetes/controls_density_diabetes", diab.bandwidth, ".png"),
#     width=300, height=200, units=c("mm"), res=100)
# #plot(diab.lrr$sparr_risk$f)
# plot(diab.lrr$sparr_risk$g)
# dev.off()

# Save rasters to create maps in QGIS
save_raster_lrr(diab.lrr, "diabetes", diab.bandwidth)

# SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS

# Extract data for risk areas
diab.areas <- polygonize_logrr(diab.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), diab.data)
map_extracted_areas(diab.lrr, diab.areas, "diabetes", bandwidth = diab.bandwidth)

# Plot significant areas
map_significant_areas(diab.areas$lrr.poly, dem, basemap_type = "raster", title="Diabetes", bandwidth = diab.bandwidth)

# Compare individual and neighborhood factors between risk areas
compare_areas(diab.areas$lrr.indiv, diab.areas$lrr.ha, cov.indiv, cov.ha)
pairwise_violin_plot(diab.areas$lrr.ha, cov.ha.env, cov.ha.env.labels, "diabetes", bandwidth = diab.bandwidth)
pairwise_violin_plot(diab.areas$lrr.ha, cov.ha.soc, cov.ha.soc.labels, "diabetes", bandwidth = diab.bandwidth)

# SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA

# Check if some areas need to be merged
map_high_risk_areas(diab.areas$lrr.poly, laus$extent, "diabetes", bandwidth = diab.bandwidth)

compare_high_risk_area(diab.areas, cov.indiv, cov.ha, excluded_polys = c(13,15,10))

sink()



# DYSLIPIDEMIA ----------------------------------------------------------------

sink("../results/spatial_disease_risk/dyslipidemia/f2/spatial_variation_risk_dyslipidemia.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

dys.data <- select_outcome_spatial(indiv, "dyslipidemia", cov=cov.indiv)
plot_case_control(dys.data, laus$extent, build, title='Dyslipidemia')

# Create PPP
dys.events <- create_ppp(dys.data, laus$extent, marks=dys.data$outcome, title="Dyslipidemia - events")

# Log relative risk surface
optimal_bandwidths(dys.events)

# # Compare bandwidths (for supplementary materials)
# cat("\nCompare different bandwidths\n")
# for (bw in seq(100, 600, 100)){
#   cat(paste0("\nBandwidth: ", bw, " meters\n"))
#   dys.lrr <- log_ratio_spatial_dens(dys.events, bw, nsim=999, seed=12345, outcome_name="dyslipidemia")
#   global_clustering(dys.lrr$smacpod_risk)
# }

dys.bandwidth <- 200 

cat(paste0("\nSelected bandwidth: ", dys.bandwidth, "\n"))

dys.lrr <- log_ratio_spatial_dens(dys.events, dys.bandwidth, nsim=999, seed=12345, outcome_name="dyslipidemia")
global_clustering(dys.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(dys.lrr, "dyslipidemia", dys.bandwidth)

# SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS

# Extract data for risk areas
dys.areas <- polygonize_logrr(dys.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), dys.data)
map_extracted_areas(dys.lrr, dys.areas, "dyslipidemia")

# Plot significant areas
map_significant_areas(dys.areas$lrr.poly, dem, basemap_type = "raster", title="Dyslipidemia", bandwidth = dys.bandwidth)

# Compare individual and neighborhood factors between risk areas
compare_areas(dys.areas$lrr.indiv, dys.areas$lrr.ha, cov.indiv, cov.ha)
pairwise_violin_plot(dys.areas$lrr.ha, cov.ha.env, cov.ha.env.labels, "dyslipidemia", bandwidth = dys.bandwidth)
pairwise_violin_plot(dys.areas$lrr.ha, cov.ha.soc, cov.ha.soc.labels, "dyslipidemia", bandwidth = dys.bandwidth)

# SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA

# Check if some areas need to be merged
map_high_risk_areas(dys.areas$lrr.poly, laus$extent, "dyslipidemia", bandwidth = dys.bandwidth)


compare_high_risk_area(dys.areas, cov.indiv, cov.ha, excluded_polys = c(11,18))


sink()


# # CVD ----------------------------------------------------------------
# 
# sink("../results/spatial_disease_risk/cvd/f2/spatial_variation_risk_cvd.txt")
# cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file
# 
# cvd.data <- select_outcome_spatial(indiv, "cvd", cov=cov.indiv)
# plot_case_control(cvd.data, laus$extent, build, title='CVD')
# 
# # Create PPP
# cvd.events <- create_ppp(cvd.data, laus$extent, marks=cvd.data$outcome, title="CVD - events")
# 
# # Log relative risk surface
# optimal_bandwidths(cvd.events)
# cvd.bandwidth <- 350 # Scott's rule
# 
# cvd.lrr <- log_ratio_spatial_dens(cvd.events, cvd.bandwidth, nsim=999, seed=12345, outcome_name="cvd")
# 
# # Save rasters to create maps in QGIS
# save_raster_lrr(cvd.lrr, "cvd", cvd.bandwidth)
# 
# # SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS
# 
# # Extract data for risk areas
# cvd.areas <- polygonize_logrr(cvd.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), cvd.data)
# map_extracted_areas(cvd.lrr, cvd.areas, "cvd")
# 
# # Plot significant areas
# map_significant_areas(cvd.areas$lrr.poly, dem, basemap_type = "raster", title="CVD", bandwidth = cvd.bandwidth)
# 
# # Compare individual and neighborhood factors between risk areas
# compare_areas(cvd.areas$lrr.indiv, cvd.areas$lrr.ha, cov.indiv, cov.ha)
# pairwise_violin_plot(cvd.areas$lrr.ha, cov.ha.env, "cvd")
# pairwise_violin_plot(cvd.areas$lrr.ha, cov.ha.soc, "cvd")
# 
# # SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA
# 
# # Check if some areas need to be merged
# map_high_risk_areas(cvd.areas$lrr.poly, laus$extent, "cvd")
# # Drop polygon 9
# cvd.areas$lrr.poly[cvd.areas$lrr.poly$polyID ==9, "risk"] <- "Not significant"
# 
# #compare_high_risk_area(cvd.areas, cov.indiv, cov.ha, excluded_poly=c(9))
# compare_high_risk_area(cvd.areas, cov.indiv, cov.ha)
# 
# sink()



# # MULTIMORBIDITY ----------------------------------------------------------------
# 
# sink("../results/spatial_disease_risk/multimorbidity/f2/spatial_variation_risk_multimorbidity.txt")
# cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file
# 
# multi.data <- select_outcome_spatial(indiv, "multimorbidity", cov=cov.indiv)
# plot_case_control(multi.data, laus$extent, build, title='Multimorbidity')
# 
# # Create PPP
# multi.events <- create_ppp(multi.data, laus$extent, marks=multi.data$outcome, title="Multimorbidity - events")
# 
# # Log relative risk surface
# optimal_bandwidths(multi.events)
# multi.bandwidth <- 310 # Compromise of Scott's rule
# 
# cat(paste0("\nSelected bandwidth: ", multi.bandwidth, "\n"))
# 
# multi.lrr <- log_ratio_spatial_dens(multi.events, multi.bandwidth, nsim=999, seed=12345, outcome_name="multimorbidity")
# global_clustering(multi.lrr$smacpod_risk)
# 
# # Save rasters to create maps in QGIS
# save_raster_lrr(multi.lrr, "multimorbidity", multi.bandwidth)
# 
# # SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS
# 
# # Extract data for risk areas
# multi.areas <- polygonize_logrr(multi.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), multi.data)
# map_extracted_areas(multi.lrr, multi.areas, "multimorbidity")
# 
# # Plot significant areas
# map_significant_areas(multi.areas$lrr.poly, dem, basemap_type = "raster", title="Multimorbidity", bandwidth = multi.bandwidth)
# 
# # Compare individual and neighborhood factors between risk areas
# compare_areas(multi.areas$lrr.indiv, multi.areas$lrr.ha, cov.indiv, cov.ha)
# pairwise_violin_plot(multi.areas$lrr.ha, cov.ha.env, "multimorbidity")
# pairwise_violin_plot(multi.areas$lrr.ha, cov.ha.soc, "multimorbidity")
# 
# # SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA
# 
# # Check if some areas need to be merged
# map_high_risk_areas(multi.areas$lrr.poly, laus$extent, "multimorbidity")
# # Drop polygon 10,11,13
# multi.areas$lrr.poly[multi.areas$lrr.poly$polyID %in% c(10,11,13), "risk"] <- "Not significant"
# 
# compare_high_risk_area(multi.areas, cov.indiv, cov.ha)
# 
# sink()
# 
# 
# 
# 
# # POLYPHARMACY ----------------------------------------------------------------
# 
# sink("../results/spatial_disease_risk/polypharmacy/f2/spatial_variation_risk_polypharmacy.txt")
# cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file
# 
# pharm.data <- select_outcome_spatial(indiv, "polypharmacy", cov=cov.indiv)
# plot_case_control(pharm.data, laus$extent, build, title='Polypharmacy')
# 
# # Create PPP
# pharm.events <- create_ppp(pharm.data, laus$extent, marks=pharm.data$outcome, title="Polypharmacy - events")
# 
# # Log relative risk surface
# optimal_bandwidths(pharm.events)
# pharm.bandwidth <- 300 # Compromise of Scott's rule
# 
# cat(paste0("\nSelected bandwidth: ", pharm.bandwidth, "\n"))
# 
# pharm.lrr <- log_ratio_spatial_dens(pharm.events, pharm.bandwidth, nsim=999, seed=12345, outcome_name="polypharmacy")
# global_clustering(pharm.lrr$smacpod_risk)
# 
# # Save rasters to create maps in QGIS
# save_raster_lrr(pharm.lrr, "polypharmacy", pharm.bandwidth)
# 
# # SIGNIFICANT FACTORS BETWEEN HIGH-RISK, LOW-RISK, NOT SIGNIFICANT AREAS
# 
# # Extract data for risk areas
# pharm.areas <- polygonize_logrr(pharm.lrr$smacpod_risk, ha %>% dplyr::select(reli, all_of(cov.ha), geometry), pharm.data)
# map_extracted_areas(pharm.lrr, pharm.areas, "polypharmacy")
# 
# # Plot significant areas
# map_significant_areas(pharm.areas$lrr.poly, dem, basemap_type = "raster", title="Polypharmacy", bandwidth = pharm.bandwidth)
# 
# # Compare individual and neighborhood factors between risk areas
# compare_areas(pharm.areas$lrr.indiv, pharm.areas$lrr.ha, cov.indiv, cov.ha)
# pairwise_violin_plot(pharm.areas$lrr.ha, cov.ha.env, "polypharmacy")
# pairwise_violin_plot(pharm.areas$lrr.ha, cov.ha.soc, "polypharmacy")
# 
# # SIGNIFICANT FACTORS FOR EACH HIGH-RISK AREA
# 
# # Check if some areas need to be merged
# map_high_risk_areas(pharm.areas$lrr.poly, laus$extent, "polypharmacy")
# 
# compare_high_risk_area(pharm.areas, cov.indiv, cov.ha)
# 
# sink()


DBI::dbDisconnect(con)
