require(tidyverse)
require(sf)
require(ggplot2)
require(viridis)
require(classInt)
require(ggspatial)
require(RPostgreSQL)
require(corrplot)

source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')
setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

# con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

ha <- read_sf(con, query="SELECT reli, INTDEN, GREEN_SP, NOISE, PM25, NO2, MEDREV, R_UNEMP, R_NN_POBL, R_NN_CH, geometry FROM geochronic.ha_characteristics WHERE st_intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))")
#names(ha)[names(ha) != c("reli", "geometry")] <- toupper(names(ha)[names(ha) != c("reli", "geometry")])

columns_to_convert <- c("intden", "green_sp", "noise", "pm25", "no2", "medrev", "r_unemp", "r_nn_pobl", "r_nn_ch")
names(ha)[names(ha) %in% columns_to_convert] <- toupper(names(ha)[names(ha) %in% columns_to_convert])


lausanne <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
lake_1 <- st_read("../qgis/lake_border_clean_1rst.geojson")
lake_2 <- st_read("../qgis/lake_border_clean_2nd.geojson")



# SPATIAL DISTRIBUTION ----------------------------------------------------


choropleth_map <- function(ha_df, ind_name, legend_name, title_name, class_type="pretty", save=TRUE){
  
  breaks <- classIntervals(ha_df %>% pull(!!as.name(ind_name)), n = 5, style = class_type)
  
  p <- ggplot() +
    geom_sf(data = lausanne, color = "grey", fill = NA, lwd=1.5) +
    geom_sf(data = lake_2, fill = "#c9e5f3", color = "#c9e5f3", alpha = 1) +
    geom_sf(data = lake_1, fill = "#9fd1ea", color = "#9fd1ea", alpha = 0.7) +
    geom_sf(data = ha_df, aes(fill = !!as.name(ind_name)), color = "white" , alpha=0.7) +
    scale_fill_viridis(discrete = F,
                       name = legend_name,
                       breaks = breaks$brks,
                       labels = breaks$brks,
                       direction = 1,
                       guide = guide_colourbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         title.hjust = 0.4,
                         label.hjust = 0.5)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.title = element_text(size = 16),
          legend.position = c(0.2, 0.05),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 8),
          text=element_text(family="Ubuntu Regular")) +
    labs(x = NULL, y = NULL,                                                         
         title = title_name,    
         #subtitle = "Source: Table QS502EW, Census 2011",                             
         #caption = "Contains OS data © Crown copyright and database right (2018)"
    ) +
    annotation_scale(location = "br", width_hint = 0.2, style="ticks") 
  
  ggsave(paste0("../results/env_characteristics/choropleth_", ind_name, ".png"), 
         bg="white", width=200, height=150, units=c("mm"), dpi=300)
  
  return(p)
  
}


cov <- c("INTDEN", "GREEN_SP", "NOISE", "PM25", "NO2", "MEDREV", "R_UNEMP", "R_NN_POBL", "R_NN_CH")

choropleth_map(ha, "INTDEN", "Intersection Density (-)", "Street connectivity within a 500-meters radius buffer")
choropleth_map(ha, "GREEN_SP", "Greenness (%)", "Proportion of green spaces within a 500-meters radius buffer")
choropleth_map(ha, "NOISE", "Noise (dB)", "Nihgttime Noise Exposure from Roadway and Railway Sources")
choropleth_map(ha, "PM25", "Concentration (ug/m3)", "Exposure to fine particulate matter PM2.5")
choropleth_map(ha, "NO2", "Concentration (ug/m3)", "Exposure to nitrogen dioxide (NO2)")
choropleth_map(ha, "MEDREV", "Income (kCHF)", "Median annual income per household")
choropleth_map(ha, "R_UNEMP", "Rate (%)", "Unemployment rate for population aged 15 and above")
choropleth_map(ha, "R_NN_POBL", "Rate (%)", "Population aged 15+ with compulsory education")
choropleth_map(ha, "R_NN_CH", "Rate (%)", "Proportion of foreign population")


# CORRELATION -------------------------------------------------------------

data_scaled <- ha %>% 
  st_drop_geometry() %>% 
  dplyr::select(all_of(cov)) %>% 
  filter(complete.cases(.)) %>%
  scale()

corr_matrix <- cor(data_scaled)
print(corr_matrix)

png("../results/env_characteristics/corrplot.png", bg="white", width=200, height=150, units=c("mm"), res=300)
corrplot(corr_matrix,tl.col = "black")
dev.off()



DBI::dbDisconnect(con)

