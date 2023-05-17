# LOAD COLAUS PARTICIPANTS
load_participants <- function(con, period="f2"){
  
  if(period=="b"){
    dataset <- read_sf(con, query="SELECT * FROM geochronic.colaus_b")
  }
  else if(period=="f1"){
    dataset <- read_sf(con, query="SELECT * FROM geochronic.colaus_f1")
  }
  else if(period=="f2"){
    # Geocoded CoLaus participants with RELI (to match withe contextual characteristics)
    dataset <- read_sf(con, query="SELECT * FROM geochronic.f2_geo_vaud")
  }
  
  return(dataset)
}

# MUMBER DISTINCT VAL PER OUTCOME
count_distinct_val <- function(var, data = data){
  data %>% 
    st_drop_geometry() %>% 
    group_by(!!as.name(var)) %>% 
    summarise(n())
}

# WRITE TEXTS IN FILE
write <- function(text, file_res){
  cat(paste(text, "\n\n"), file=file_res, append=TRUE)
}

# WRITE FUNCTION OUTPUTS TEXT IN FILE
capture <- function(result, file_res){
  capture.output(result, file=file_res, append=TRUE)
}

count_unique_combinations <- function(outcome, data){
  list_name <- paste0("outcomes.", outcome)
  print(data %>% st_drop_geometry() %>% select(all_of(!!as.name(list_name)), !!as.name(outcome)) %>% group_by_all() %>% summarise(n()), n=Inf)
}

print_final_stats <- function(var, data){
  
  subset <- data %>% st_drop_geometry() %>% select(pt, !!as.name(var))
  
  n = subset %>% nrow()
  cases <- subset %>% filter(!!as.name(var)==1) %>% nrow()
  nan <- subset %>% filter(is.nan(!!as.name(var))) %>% nrow()
  
  write("")
  write(paste("Statistics for", str_to_upper(var)))
  print(paste("Cases:", cases, "(", round(100*(cases/n),2), "%)")) %>% capture()
  print(paste("Missing values:", nan, "(", round(100*(nan/n),2), "%)")) %>% capture()
  
}