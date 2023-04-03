# This code runs a logistic regression on CoLaus participants

library(tidyverse)
library(sf)
require(RPostgreSQL)
library(spatstat)
library(smacpod)
source('/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/geosan_funcs/password_utils.R')

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/src/")

#con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",askForPassword(),dbname="geosan")
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")



# FUNCTIONS ---------------------------------------------------------------

plot_case_control <- function(df){
  
  ggplot(df, aes(x = x, y = y, color = outcome)) +
    geom_point(size=0.5) +
    theme_bw() +
    scale_color_manual(values = c("red", "blue"))
  
  # ggplot() +
  #   geom_sf(data = filter(df, outcome == "control"), color = "blue", size = 0.5) +
  #   geom_sf(data = filter(df, outcome == "case"), color = "red", size = 0.5) +
  #   theme_void() 
}

create_ppp <- function(df, window, marks=NULL, title="Event locations"){
  
  df <- ppp(df$x, df$y, window = window, marks=marks)
  plot(df, main=title)
  
  return(df)
}

spatial_density <- function(ppp, title="Spatial density of events"){
  
  density <- spdensity(ppp, sigma=bw.scott, at="pixels", edges=TRUE,  diggle=TRUE)
  plot(density, main=title)
  
  return(density)
}

# IMPORT DATA -------------------------------------------------------------

data.all <- read_sf(con, query="SELECT *, ST_X(geometry) as coordx, ST_Y(geometry) as coordy FROM geochronic.f2_study_dataset")
data.laus <- read_sf(con, query="SELECT f.*, ST_X(f.geometry) as coordx, ST_Y(f.geometry) as coordy FROM geochronic.f2_study_dataset f, lausanne_sectors_extent l WHERE st_intersects(f.geometry, l.geometry)")

study_area.df <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")

# Add noise to points location to prevent duplicated points
data.laus$x <- jitter(data.laus$coordx)
data.laus$y <- jitter(data.laus$coordy)

# Test with diabetes
data.laus <- data.laus %>% mutate(outcome = factor(if_else(diabetes==1, "case", "control")))
# data <- data %>% dplyr::select(pt, outcome, reli, x, y, geometry) %>% arrange(outcome)

plot_case_control(data.laus)

# Create Window for the point pattern
study_area.window <- simplify.owin(as.owin(study_area.df$geometry), 50)

# Create ppp
events.ppp <- create_ppp(data.laus, study_area.window, marks=data.laus$outcome, title="Colaus Participants")
cases.ppp <- create_ppp(data.laus %>% filter(outcome=='case'), study_area.window, title="Cases")
controls.ppp <- create_ppp(data.laus %>% filter(outcome=='control'), study_area.window, title="Controls")

# Spatial density of case-control events
spatial_density(cases.ppp, title="Spatial density of cases")
spatial_density(controls.ppp, title="Spatial density of controls")

# Log relative risk
lrr =  relrisk(events.ppp, sigma=350, at="pixels", edge=TRUE, casecontrol=TRUE, relative=TRUE, diggle=TRUE, control="control")
plot(lrr, "Log relative risk of cases vs. controls")
contour(lrr, add=TRUE)
# Test for random labeling hypothesis
lrr.signif = logrr(events.ppp, sigma=350, at="pixels", edge=TRUE, casecontrol=TRUE, relative=TRUE, case="case", diggle=FALSE, nsim=999, level=0.9)
lrr.signif
plot(lrr.signif)

logrr.test(lrr.signif)


# Kernel Density ----------------------------------------------------------

# See Waller and Gotway, p. 140

cases.density <- density.ppp(cases.ppp, sigma=bw.scott, at="pixels", edges=TRUE)
controls.density <- density.ppp(controls.ppp, sigma=bw.scott, at="pixels", edges=TRUE)

par(mfrow = c(1, 2), mar = c(0,0,1.1,2))
plot(cases.density, main=paste('Cases (n= ', cases.ppp$n, ' )'))
plot(cases.ppp, add=TRUE, pch=1, cex=0.3, col='black', alpha=I(1/2))
plot(controls.density, main=paste('Controls (n= ', controls.ppp$n, ' )'))
plot(controls.ppp, add=TRUE, pch=1, cex=0.3, col='black', alpha=I(1/2))

plot(density(split(events.ppp), sigma=bw.scott, at="pixels", edges=TRUE), main = "")

contour(cases.density)

plot(spdensity(cases.ppp, sigma=bw.scott, at="pixels", edges=TRUE))

bw.scott(cases.ppp, isotropic=FALSE, d=NULL)
bw.scott(controls.ppp, isotropic=FALSE, d=NULL)

spat_risk = relrisk(events.ppp, sigma=350, at="pixels", edge=TRUE, casecontrol=TRUE, relative=TRUE, diggle=TRUE, control="control")
plot(spat_risk)
