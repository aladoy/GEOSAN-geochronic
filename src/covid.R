
library(tidyverse)
library(sf)
library(RPostgreSQL)

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)")

# IMPORT DATA -------------------------------------------------------------

# Connect to GEOCOVID DB
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= rstudioapi::askForPassword("Database user"),rstudioapi::askForPassword("Database password"),dbname="geosan")

# Import Lausanne
laus <- st_read('data/laus_clip.shp')
laus <- laus %>% select(geometry)
plot(laus)

# Import COVID dataset
sql = "SELECT G.id_demande_study2, G.id_patient_study2, G.date_naissance, G.res_cov, G.res_cov_txt, G.note_geocoding, G.geometry FROM geocovid.s3_notfiltered_tests_geo G, 
(SELECT geometry FROM municipalities WHERE name='Lausanne' AND part='1') L
WHERE st_within(G.geometry, L.geometry)"
covid = read_sf(con, query=sql)
covid = st_filter(covid, laus, .predicate=st_within)
covid = covid %>% mutate(res_cov=as.factor(res_cov), res_cov_txt= as.factor(res_cov_txt))

# CoLaus
colaus = st_read('data/colaus/geocolaus3_urbain_3708.shp')


# DESCRIPTIVE -------------------------------------------------------------

summary(covid)
pos = covid %>% filter(res_cov==1)
neg = covid %>% filter(res_cov==0)

# CREATE GRID -------------------------------------------------------------

# 100x100m grid
laus.grid <- st_make_grid(laus, cellsize = 100, what="polygons")
# Intersects with city
laus.grid = st_intersection(laus, laus.grid)
# Add row number
laus.grid$ID =  1:nrow(laus.grid)

# Count number of positive and number of negative per grid
laus.grid <- laus.grid %>% mutate(nb_pos = lengths(st_contains(., pos)), nb_neg = lengths(st_contains(., neg)))
laus.grid <- laus.grid %>% mutate(nb_tests = nb_pos + nb_neg)
#laus.grid$nb_pos[laus.grid$nb_pos==0] <- 1
#laus.grid$nb_neg[laus.grid$nb_neg==0] <- 1
laus.grid <- laus.grid %>% mutate(incidence = nb_pos / nb_tests)
# remove cells where nb_tests = 0
laus.grid <- laus.grid %>% filter(nb_tests > 0)


# Plot prevalence

ggplot(laus.grid) + geom_sf(data=laus, fill='grey') + geom_sf(aes(fill=incidence), color=NA)
ggplot(laus.grid) + geom_sf(data=laus, fill='grey') + geom_sf(aes(fill=nb_pos), color=NA)
ggplot(laus.grid) + geom_sf(data=laus, fill='grey') + geom_sf(aes(fill=nb_tests), color=NA)

ggplot(covid) + geom_sf(data=laus, fill='grey') + geom_point(aes(geometry=geometry, fill=res_cov_txt), stat="sf_coordinates", size=0.5, shape=21)
#+ scale_colour_manual(name="",  values =c("NEGATIVE"="grey", "POSITIVE"="red"))
