# This code computes the log relative risk surface of chronic diseases for the baseline period.

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

require(svglite)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
source('modeling/utils_spatial_variation_risk.R')

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


# LOAD DATASETS -----------------------------------------------------------

laus <- load_boundaries(con)

indiv <- load_participants(con, period_shrt="f1", extent=laus$extent, seed=12346)
# st_write(indiv, "../processed_data/events_f1_analysis.geojson", delete_dsn = TRUE)
indiv <- st_read("../processed_data/events_f1_analysis.geojson")

# Basemap
build <- st_read(dsn="/mnt/data/GEOSAN/RESEARCH PROJECTS/COMMUNE EN SANTE @ UNISANTE/commune-en-sante/qgis/layers/admin_boundaries.gpkg", layer="vd_buildings")
build <- build %>% filter(st_intersects(., laus$extent, sparse = FALSE))
dem <- raster("/mnt/data/GEOSAN/GEOSAN DB/data/MODELE NUMERIQUE HAUTEUR LAUSANNE/lausanne_dem_final_5m.tif")


# Filter CoLaus participants in Lausanne only
indiv <- indiv %>% filter(st_intersects(geometry, laus$extent, sparse = FALSE))
print(paste("Number of FU1 individuals in Lausanne:", nrow(indiv)))


PERIOD <- "f1"


# HYPERTENSION ----------------------------------------------------------------

sink("../results/spatial_disease_risk/hypertension/f1/spatial_variation_risk_hypertension.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

hyp.data <- select_outcome_spatial(indiv, "hypertension", cov=NULL, period=PERIOD)
plot_case_control(hyp.data, laus$extent, build, title='Hypertension', period=PERIOD)


# Create PPP
hyp.events <- create_ppp(hyp.data, laus$extent, marks=hyp.data$outcome, title="Hypertension - events")

# Log relative risk surface
optimal_bandwidths(hyp.events)
hyp.bandwidth <- 200 

cat(paste0("\nSelected bandwidth: ", hyp.bandwidth, "\n"))

hyp.lrr <- log_ratio_spatial_dens(hyp.events, hyp.bandwidth, nsim=999, seed=12345, outcome_name="hypertension", period=PERIOD)
global_clustering(hyp.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(hyp.lrr, "hypertension", hyp.bandwidth, period=PERIOD)

sink()



# OBESITY ----------------------------------------------------------------

sink("../results/spatial_disease_risk/obesity/f1/spatial_variation_risk_obesity.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

obes.data <- select_outcome_spatial(indiv, "obesity", cov=NULL, period=PERIOD)
plot_case_control(obes.data, laus$extent, build, title='Obesity', period=PERIOD)


# Create PPP
obes.events <- create_ppp(obes.data, laus$extent, marks=obes.data$outcome, title="Obesity - events")

# Log relative risk surface
optimal_bandwidths(obes.events)
obes.bandwidth <- 200

cat(paste0("\nSelected bandwidth: ", obes.bandwidth, "\n"))

obes.lrr <- log_ratio_spatial_dens(obes.events, obes.bandwidth, nsim=999, seed=12345, outcome_name="obesity", period=PERIOD)
global_clustering(obes.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(obes.lrr, "obesity", obes.bandwidth, period=PERIOD)

sink()


# DIABETES ----------------------------------------------------------------

sink("../results/spatial_disease_risk/diabetes/f1/spatial_variation_risk_diabetes.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

diab.data <- select_outcome_spatial(indiv, "diabetes", cov=NULL, period=PERIOD)
plot_case_control(diab.data, laus$extent, build, title='Diabetes', period=PERIOD)


# Create PPP
diab.events <- create_ppp(diab.data, laus$extent, marks=diab.data$outcome, title="Diabetes - events")

# Log relative risk surface
optimal_bandwidths(diab.events)
diab.bandwidth <- 200

cat(paste0("\nSelected bandwidth: ", diab.bandwidth, "\n"))

diab.lrr <- log_ratio_spatial_dens(diab.events, diab.bandwidth, nsim=999, seed=12345, outcome_name="diabetes", period=PERIOD)
global_clustering(diab.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(diab.lrr, "diabetes", diab.bandwidth, period=PERIOD)

sink()


# DYSLIPIDEMIA ----------------------------------------------------------------

sink("../results/spatial_disease_risk/dyslipidemia/f1/spatial_variation_risk_dyslipidemia.txt")
cat(paste0("Date:", Sys.Date(),'\n')) #Overwrite the file

dys.data <- select_outcome_spatial(indiv, "dyslipidemia", cov=NULL, period=PERIOD)
plot_case_control(dys.data, laus$extent, build, title='Dyslipidemia', period=PERIOD)


# Create PPP
dys.events <- create_ppp(dys.data, laus$extent, marks=dys.data$outcome, title="Dyslipidemia - events")

# Log relative risk surface
optimal_bandwidths(dys.events)
dys.bandwidth <- 200

cat(paste0("\nSelected bandwidth: ", dys.bandwidth, "\n"))

dys.lrr <- log_ratio_spatial_dens(dys.events, dys.bandwidth, nsim=999, seed=12345, outcome_name="dyslipidemia", period=PERIOD)
global_clustering(dys.lrr$smacpod_risk)

# Save rasters to create maps in QGIS
save_raster_lrr(dys.lrr, "dyslipidemia", dys.bandwidth, period=PERIOD)

sink()



DBI::dbDisconnect(con)