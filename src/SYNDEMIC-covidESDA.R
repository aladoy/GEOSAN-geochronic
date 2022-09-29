require(tidyverse)
require(sf)
require(WVPlots)
require(nlme)
require(geoR)
require(PrevMap)
require(RPostgreSQL)

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)/GEOSAN-syndemic")

# IMPORT DATA -------------------------------------------------------------

# Connect to GEOSAN DB
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= rstudioapi::askForPassword("Database user"),rstudioapi::askForPassword("Database password"),dbname="geosan")

# COVID-19
sql_covid <- "SELECT * FROM syndemic.covid_lausregion_analysis"
covid.sf <- read_sf(con, query=sql_covid)


# Prevalence
# At reli point
sql_prev_pt <- "SELECT r.reli, COUNT(*) as n_test, SUM(s.res_cov) as n_pos, AVG(s.viral_load) as m_vr, AVG(s.age) as m_age, r.geometry FROM syndemic.covid_lausregion_analysis s 
INNER JOIN vd_reli_centroid r ON r.reli = s.reli where wave=2
GROUP BY r.reli, r.geometry"
prev.pt_sf <- read_sf(con, query=sql_prev_pt)

# At reli polygon
sql_prev_pl <- "SELECT r.reli, COUNT(*) as n_test, SUM(s.res_cov) as n_pos, AVG(s.viral_load) as m_vr, AVG(s.age) as m_age, r.geometry FROM syndemic.covid_lausregion_analysis s 
INNER JOIN vd_reli_polygon r ON r.reli = s.reli where wave=2
GROUP BY r.reli, r.geometry"
prev.pl_sf <- read_sf(con, query=sql_prev_pl)



# EMPIRICAL LOGIT PREVALENCE ----------------------------------------------

prev.pl_sf$logit <- log((prev.pl_sf$n_pos+0.5)/(prev.pl_sf$n_test-prev.pl_sf$n_pos+0.5))
prev.pl_sf$prev <- log((prev.pl_sf$n_pos+0.5)/(prev.pl_sf$n_test+0.5))

# Extract prevalence and logit per wave
extract_prev_waves <- function(con, wave){
  
  query = sqlInterpolate(con, "SELECT s.reli, COUNT(*) as n_test, SUM(s.res_cov) as n_pos, AVG(s.viral_load) as m_vr, AVG(s.age) as m_age 
                         FROM syndemic.covid_lausregion_analysis s 
                         WHERE wave=?w 
                         GROUP BY s.reli", 
                         w = wave)
  data <- tibble(dbGetQuery(con, query))
  data <- data %>% mutate(prev = log((n_pos+0.5)/(n_test+0.5)), logit = log((n_pos+0.5)/(n_test-n_pos+0.5)))
  colnames(data) <- paste0(colnames(data),"_w", wave)
  
  return(data)
}

# Aggregate to a single dataframe
all_prev.pl_sf <- prev.pl_sf %>% inner_join(extract_prev_waves(con, 1), by = c("reli"="reli_w1")) %>%
  left_join(extract_prev_waves(con, 2), by = c("reli"="reli_w2")) %>% 
  left_join(extract_prev_waves(con, 3), by = c("reli"="reli_w3")) %>% 
  left_join(extract_prev_waves(con, 4), by = c("reli"="reli_w4"))

# Save
all_prev.pl_sf %>% write_sf("processed_data/prev_reli_covid_waves.gpkg", driver='GPKG')


# SAMPLE SIZES ------------------------------------------------------------

stats_covid <- function(df, w=NULL){
  
  if(is.null(w)){
    data = df
    welcome <- "Statistics for the entire period"
  }
  else{
    data <- df %>% filter(wave == w)
    welcome <- paste("Statistics for wave", w)
  }
  
  n_test = data %>% nrow()
  n_pos = data %>% filter(res_cov==1) %>% nrow()
  perc_pos = n_pos/n_test * 100
  print(welcome)
  print(paste("Number of tests: ", n_test))
  print(paste("Number of cases: ", n_pos))
  print(paste("Percent positivity: ", round(perc_pos,2)))
}


stats_covid(covid.sf)
stats_covid(covid.sf, 1)
stats_covid(covid.sf, 2)
stats_covid(covid.sf, 3)
stats_covid(covid.sf, 4)



# LINEAR MODEl ------------------------------------------------------------

ggplot(prev.sf, aes(x=iqmd)) + geom_histogram()

# Add RELI coordinates and remove geometry
prev <- prev.sf %>% mutate(reli_x=st_coordinates(prev.sf)[,1], reli_y=st_coordinates(prev.sf)[,2]) %>% st_drop_geometry()
# Compute Empirical logit
prev$logit <- log((prev$n_pos+0.5)/(prev$n_test-prev$n_pos+0.5))

prev.sf$logit <- log((prev$n_pos+0.5)/(prev$n_test-prev$n_pos+0.5))

var <- variog(coords = coords, data= prev$logit)
plot(var)

# Variogram should fall outside grey area if residual spatial correlation
spat.corr.diagnostic(logit ~ 1,
                     coords=~reli_x+reli_y,
                     data=as.data.frame(prev),
                     likelihood = "Gaussian",
                     lse.variogram = TRUE)

ggplot(prev.sf) + geom_sf() + geom_point(
  aes(color = logit, geometry = geometry),
  stat = "sf_coordinates"
) +
  scale_color_viridis_c(option = "C") +
  theme(legend.position = "bottom")

ggplot(prev, aes(x=logit)) + geom_histogram()
ggplot(prev, aes(x=log(iqmd), y=logit)) + geom_point() + stat_smooth()

### Standard linear model
model <- glm(cbind(n_pos,n_test-n_pos)~log(iqmd), family=binomial, data=prev)
summary(model)

Z.hat <- resid(model)
coords <- unique(prev[,c("reli_x","reli_y")])
data.variogram <- data.frame(Z.hat=Z.hat,
                             x=coords[,1],
                             y=coords[,2])
variogram.Z <- variog(coords = coords, data= Z.hat, uvec=seq(0,50000,length=100))
plot(variogram.Z)

beta.hat <- coef(model)
data.variogram <- data.frame(Z.hat=Z.hat,
                             utm_x=coords[,1],
                             utm_y=coords[,2])

plot.elev <- ggplot(loaloa, aes(x = ELEVATION, 
                                y = e.logit)) + geom_point() +
  labs(x="Elevation",y="Empirical logit")+
  stat_smooth(method = "gam", formula = y ~ s(x),se=FALSE)+
  stat_smooth(method = "lm", formula = y ~ x + I((x-700)*(x>700)),
              col="green",lty="dashed",se=FALSE)

model <- glm(cbind(n_pos,n_test-n_pos) ~ log(iqmd), data=prev, family = binomial)
logLik(model)

# Explanatory variables
sql_mgis <- "SELECT m.reli, m.iqmd FROM vd_mgis_ha m INNER JOIN (SELECT * FROM vd_reli_point WHERE reli IN (?list)) r ON r.reli=m.reli"
query <- sqlInterpolate(con, sql_mgis,
                        list = SQL(paste(prev.sf$reli, sep="",collapse=",")))
mgis.sf <- dbGetQuery(con, query)


# Add explanatory variables and fill by median value if NaN
prev.sf <- prev.sf %>% left_join(mgis.sf, by='reli')
prev.sf <- prev.sf %>% mutate(iqmd=replace_na(iqmd, median(prev.sf$iqmd, na.rm = TRUE)))
prev.sf <- prev.sf %>% mutate(iqmd=replace(iqmd, iqmd==0, 1))

DBI::dbDisconnect(con)