require(WVPlots)
require(nlme)
require(geoR)
require(PrevMap)
require(RPostgreSQL)
require(sf)
require(tidyverse)


setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)/GEOSAN-syndemic")

# IMPORT DATA -------------------------------------------------------------

# All data from CoLaus (Baseline, F1, F2, F3) have been integrated in the GEOSAN DB (schema:syndemic).
# Pedro Marquès-Vidal sent us the non-geo data, and Marco Vieira the individual's home address + geographic coordinates.
# Since not all data sent by Pedro have been geocoded, some data have a null geometry in the DB.
# All metadata information is stored in the DB tables directly and can be access through: 
# SELECT column_name, data_type, col_description('syndemic.colaus_b'::regclass, ordinal_position)
# FROM information_schema.columns
# WHERE table_schema = 'syndemic' AND table_name = 'colaus_b';


# Connect to GEOSAN DB
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= rstudioapi::askForPassword("Database user"),rstudioapi::askForPassword("Database password"),dbname="geosan")

# CoLaus data in Lausanne région
sql_colaus <- "SELECT * FROM syndemic.cf2_lausregion_analysis"
f2.sf <- read_sf(con, query=sql_colaus)

# RELI grid (centroid and polygons)
sql_reli_pt <- "SELECT DISTINCT r.reli, r.geometry FROM vd_reli_centroid r, syndemic.cf2_lausregion_analysis c WHERE r.reli=c.reli"
reli.pt_sf <- read_sf(con, query=sql_reli_pt)
sql_reli_poly <- "SELECT DISTINCT r.reli, r.geometry FROM vd_reli_polygon r, syndemic.cf2_lausregion_analysis c WHERE r.reli=c.reli"
reli.pl_sf <- read_sf(con, query=sql_reli_poly)


# Dataframe for outcome
f2_outcomes.sf <- f2.sf %>% mutate(
  cvd_ev = f2cvd,
  cvd_hist = case_when((cvdbase==1|cvdbase_adj==1)~1, (cvdbase==0|cvdbase_adj==0)~0, (is.na(cvdbase)&is.na(cvdbase_adj))~NaN),
  ht_t = case_when(f2hypdr==1~1,f2hypdr%in%c(2,9)~0,is.na(f2hypdr)~NaN),
  ht_m = f2hta,
  hc_t = case_when(f2hctld==1~1,f2hctld%in%c(0,9)~0,is.na(f2hctld)~NaN),
  diab_t = case_when(f2dbtld==1~1,f2dbtld%in%c(0,9)~0,is.na(f2dbtld)~NaN),
  diab_m = f2diab,
  obesity = case_when(f2bmi_cat2==3~1,f2bmi_cat2%in%c(0,1,2)~0,is.na(f2bmi_cat2)~NaN),
  overweight = case_when(f2bmi_cat2%in%c(2,3)~1,f2bmi_cat2%in%c(0,1)~0,is.na(f2bmi_cat2)~NaN),
  consult_all = f2care1,
  consult_er = f2care1b,
  hosp = f2care2,
  polypharm = polypharm,
  health_sr = f2quest1,
  gaf_l = f2gaf_l,
  gaf_w = f2gaf_w,
  gaf_c = f2gaf_c,
  stai_s = f2sts_tot,
  stai_t = f2stt_tot
)
f2_outcomes.sf <- f2_outcomes.sf %>% mutate(cvd_all = if_else((cvd_ev==1|cvd_hist==1),1,0))
f2_outcomes.sf <- f2_outcomes.sf %>% dplyr::select(pt, reli, cvd_ev, cvd_hist, cvd_all, ht_t, ht_m, hc_t, diab_t, diab_m, overweight, obesity, polypharm, consult_all, consult_er, hosp, health_sr, gaf_l, gaf_w, gaf_c, stai_s, stai_t, geometry)


count <- function(df, var){
  nrow <- df %>% filter(!is.na(eval(as.symbol(var)))) %>% nrow()
  n <- df %>% filter(eval(as.symbol(var))==1) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
  missing <- df %>% filter(is.na(eval(as.symbol(var)))) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
  perc <- round(n/nrow, 2) * 100
  
  print(paste("Statistics for", var))
  print(paste("Count:", n))
  print(paste("Proportion:", perc, "%"))
  print(paste("Missing:", missing))
}

count(f2_outcomes.sf,"cvd_ev")
count(f2_outcomes.sf,"cvd_hist")
count(f2_outcomes.sf,"cvd_all")
count(f2_outcomes.sf,"ht_t")
count(f2_outcomes.sf,"ht_m")
count(f2_outcomes.sf,"hc_t")
count(f2_outcomes.sf,"diab_t")
count(f2_outcomes.sf,"diab_m")
count(f2_outcomes.sf,"overweight")
count(f2_outcomes.sf,"obesity")
count(f2_outcomes.sf,"polypharm")
count(f2_outcomes.sf,"polypharm")
count(f2_outcomes.sf,"consult_all")
mean(f2_outcomes.sf$consult_all, na.rm=TRUE)
sd(f2_outcomes.sf$consult_all, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(consult_all)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$consult_er, na.rm=TRUE)
sd(f2_outcomes.sf$consult_er, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(consult_er)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$hosp, na.rm=TRUE)
sd(f2_outcomes.sf$hosp, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(hosp)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$gaf_l, na.rm=TRUE)
sd(f2_outcomes.sf$gaf_l, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(gaf_l)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$gaf_w, na.rm=TRUE)
sd(f2_outcomes.sf$gaf_w, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(gaf_w)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$gaf_c, na.rm=TRUE)
sd(f2_outcomes.sf$gaf_c, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(gaf_c)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$stai_s, na.rm=TRUE)
sd(f2_outcomes.sf$stai_s, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(stai_s)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()
mean(f2_outcomes.sf$stai_t, na.rm=TRUE)
sd(f2_outcomes.sf$stai_t, na.rm=TRUE)
f2_outcomes.sf %>% filter(is.na(stai_t)) %>% st_drop_geometry() %>% summarise(n()) %>% pull()



# Compute prevalence for binary by RELI
reli_outcomes.sf <- reli.pl_sf %>% 
  inner_join(f2_outcomes.sf %>% st_drop_geometry(), by='reli') %>% 
  group_by(reli) %>% 
  summarise(n=n(), 
            cvd_ev=sum(cvd_ev, na.rm=TRUE), 
            cvd_hist=sum(cvd_hist, na.rm=TRUE),
            cvd_all=sum(cvd_hist, na.rm=TRUE), 
            ht_t=sum(ht_t, na.rm=TRUE), 
            ht_m=sum(ht_m, na.rm=TRUE),
            hc_t=sum(hc_t, na.rm=TRUE),
            diab_t=sum(diab_t, na.rm=TRUE),
            diab_m=sum(diab_m, na.rm=TRUE),
            overweight=sum(overweight, na.rm=TRUE),
            obesity=sum(obesity, na.rm=TRUE),
            polypharm=sum(polypharm, na.rm=TRUE),
            consult_all=median(consult_all, na.rm=TRUE),
            consult_er=median(consult_er, na.rm=TRUE),
            hosp=median(hosp, na.rm=TRUE),
            health_sr=median(health_sr, na.rm=TRUE),
            gaf_l=median(gaf_l, na.rm=TRUE),
            gaf_w=median(gaf_w, na.rm=TRUE),
            gaf_c=median(gaf_c, na.rm=TRUE),
            stai_s=median(stai_s, na.rm=TRUE),
            stai_t=median(stai_t, na.rm=TRUE),
  )

reli_outcomes_ratio.sf <- reli_outcomes.sf %>% mutate_at(vars(cvd_ev:polypharm),~if_else(is.na(.x), NaN, log((.x+0.5)/(n-.x+0.5))))

# Create logit 
reli_outcomes_ratio.sf <- reli_outcomes_ratio.sf %>% rename_at(vars(cvd_ev:polypharm), ~paste0(., "_logit"))
reli_outcomes_ratio.sf <- reli_outcomes_ratio.sf %>% dplyr::select(reli, n:polypharm_logit, geometry)
                                                         
reli_outcomes_ratio.sf %>% st_write("processed_data/prev_reli_colaus_outcomes.gpkg", driver='GPKG')


DBI::dbDisconnect(con)