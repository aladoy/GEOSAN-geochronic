library(tidyverse)
library(sf)
library(ggplot2)
library(factoextra)
library(corrplot)

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")


ha <- read_sf(con, query="SELECT reli, PTOT, INTDEN, GREEN_SP, NOISE, PM25, NO2, MEDREV, R_UNEMP, R_NN_POBL, R_FFB, geometry FROM geochronic.ha_characteristics WHERE st_intersects(geometry, (SELECT geometry FROM lausanne_sectors_extent))")

ha_geom <- ha %>% dplyr::select(reli, geometry)

data_pca <- ha %>% 
  st_drop_geometry() %>%
  filter(complete.cases(.)) 
names(data_pca)[names(data_pca) != c("reli")] <- toupper(names(data_pca)[names(data_pca) != c("reli")])

data_pca <- scale(data_pca)

corr_matrix <- cor(data_pca)
corrplot(corr_matrix)

corr_matrix

pca <- prcomp(data_pca %>% dplyr::select(-reli), scale. = TRUE)

summary(pca)
print("According Kaiser criteria, keep the first four PC, which explained 75% of the variance.")

data_pca <- cbind(data_pca, pca$x[, 1:4])

pca_geo <- inner_join(data_pca, ha_geom, by="reli") %>% st_as_sf()

st_write(pca_geo, "pca_geo.gpkg")

fviz_pca_var(pca, col.var = "black")

# Extract the scores from the PCA results
scores <- as.data.frame(pca$x)

# Plot the scores using the first two principal components
ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point()