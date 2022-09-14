require(tidyverse)
require(sf)
require(RPostgreSQL)
require(nngeo)

setwd("/mnt/data/GEOSAN/RESEARCH PROJECTS/SYNDEMIC @ LASIG (EPFL)/GEOSAN-syndemic")

# Connect to GEOSAN DB
con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= rstudioapi::askForPassword("Database user"),rstudioapi::askForPassword("Database password"),dbname="geosan")

# Extract swiss hectometric grid
reli.sf <- st_read(con, query="SELECT * FROM reli_polygon")
reli.sf  <- reli.sf %>% rownames_to_column() %>% mutate(rowname=as.integer(rowname))

# COLAUS
# Extract data within PALM municipalities
sql_f2 <- "SELECT f2.pt, f2.geometry FROM syndemic.colaus_f2 f2 WHERE NOT st_isempty(f2.geometry)"
f2.sf <- read_sf(con, query=sql_f2)
# Add RELI
f2.sf <- f2.sf %>% st_join(reli.sf , left=TRUE, on=st_within)
# Extract data with missing RELI
f2_noreli <- f2.sf %>% filter(is.na(reli)) %>% select(pt, geometry)
#Find closest reli for each row in the dataframe
nearest<- st_nn(f2_noreli,reli.sf,k=1,returnDist = F) 
#Add the nearest neighbor (rowname) to dataframe
f2_noreli <- f2_noreli %>% mutate(nearest=sapply(nearest , "[[", 1))
#Add the corresponding RELI
f2_noreli <- f2_noreli %>% left_join(reli.sf %>% st_drop_geometry(), by=c('nearest'='rowname')) %>% select(-c(nearest)) %>% st_drop_geometry()
# Fill this info in main df
f2.sf <- f2.sf %>% left_join(f2_noreli, by='pt', suffix=c("","_no"))
f2.sf <- f2.sf %>% mutate(reli=if_else(!is.na(reli_no), reli_no, reli), b21btot=if_else(!is.na(b21btot_no), b21btot_no, b21btot)) %>% select(-c(rowname, reli_no, b21btot_no))

# UPDATE TABLE IN POSTGIS ----------------------------------------------
#Add new columns
dbGetQuery(con, "ALTER TABLE syndemic.colaus_f2 ADD COLUMN reli BIGINT")
dbGetQuery(con, "ALTER TABLE syndemic.colaus_f2 ADD COLUMN reli_b21btot BIGINT")

#Function to update Postgres table
update_postgres_table <- function(tabledb, id, reli, ptot){
  sql1<-"UPDATE ?table SET reli = ?new_col WHERE pt = ?id"
  query1 <- sqlInterpolate(con, sql1,
                 table = SQL(tabledb),
                 new_col = SQL(reli),
                 id = SQL(id)
  )
  dbGetQuery(con, query1)
  
  
  sql2<-"UPDATE ?table SET reli_b21btot = ?new_col WHERE pt = ?id"
  query2 <- sqlInterpolate(con, sql2,
                          table = SQL(tabledb),
                          new_col = SQL(ptot),
                          id = SQL(id)
  )
  dbGetQuery(con, query2)
}

#Map for every rows (Normal if an error appears, still working)
pmap(list("syndemic.colaus_f2", f2.sf$pt, f2.sf$reli,  f2.sf$b21btot), update_postgres_table)

# To visually verify that all points were reasonably close from a RELI
# f2_noreli %>% inner_join(f2.sf %>% select(pt, geometry), by='pt') %>% st_write("f2_noreli.shp")
