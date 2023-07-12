require(tidyverse)
require(sf)
require(ggplot2)
require(viridis)
require(classInt)
require(ggspatial)
require(RPostgreSQL)


setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


lausanne <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
lake <- st_read("../qgis/lake_border_buffered.geojson")

# Tolerance contours
tol <- st_read("../results/spatial_disease_risk/hypertension/f2/tolerance_contours_hypertension_200.geojson")

# GWR results
gwr <- st_read("../processed_data/hypertension_gwr_binomial.gpkg")



gwr_map <- function(gwr_df, var){
  
  # create title
  if (startsWith(var, "mgwr_")) {
    var_name <- gsub("mgwr_", "", var)
    title_name <- paste("MGWR -", var_name)
  } else{
    var_name <- gsub("gwr_", "", var)
    title_name <- paste("GWR -", var_name)
  }
  
  # Create the plot
  p <- ggplot() +
    geom_sf(data = lausanne, color = "grey", fill = NA, lwd=1.5) +
    geom_sf(data = lake, fill = "#c9e5f3", color = "#c9e5f3", alpha = 1) +
    # geom_sf(data = gwr, aes(fill = factor(ifelse(!!as.name(var) > 0, "Positive", ifelse(!!as.name(var) < 0, "Negative", "Zero")))),
    #         color = "grey", shape = 21, size = 2, alpha = 0.7) +
    # scale_fill_manual(values = c("Zero" = "white", "Positive" = "red", "Negative" = "blue")) +
    geom_sf(data = gwr, aes(fill = !!as.name(var)),
            color = "grey", shape = 21, size = 2, alpha = 0.7) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, na.value = "grey", guide = guide_colorbar(title = "", frame.colour="black")) +
    geom_sf(data = tol[tol$RISK == 0.01, ], linetype = "solid", color = "black") +
    geom_sf(data = tol[tol$RISK == 0.05, ], linetype = "dashed", color = "black") +
    scale_linetype_manual(values = c("0.01" = "solid", "0.05" = "dashed")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.title = element_text(size = 16),
          legend.position = "right",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 8),
          text = element_text(family = "Ubuntu Regular")) +
    labs(x = NULL, y = NULL,
         title = title_name)
  
  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/hypertension/gwr_binomial/map_", var, ".png"),
         bg = "white", width = 200, height = 150, units = "mm", dpi = 300)
  
}



tc_columns <- names(gwr)[grep("_TC$", names(gwr))]
lapply(tc_columns, function(var) gwr_map(gwr, var))
