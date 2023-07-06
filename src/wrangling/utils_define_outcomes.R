# LOAD COLAUS PARTICIPANTS
load_participants <- function(con, period="f2"){
  # Geocoded participants
  
  if(period=="b"){
    dataset <- read_sf(con, query="SELECT * FROM geochronic.colaus_b b WHERE NOT ST_IsEmpty(b.geometry)")
  }
  else if(period=="f1"){
    dataset <- read_sf(con, query="SELECT * FROM geochronic.colaus_f1 f1 INNER JOIN (SELECT pt, cvdbase_adj FROM  geochronic.colaus_b) b USING (pt) WHERE NOT ST_IsEmpty(f1.geometry)")
  }
  else if(period=="f2"){
    dataset <- read_sf(con, query="SELECT * FROM geochronic.colaus_f2 f2 INNER JOIN (SELECT pt, cvdbase_adj FROM  geochronic.colaus_b) b USING (pt) WHERE NOT ST_IsEmpty(f2.geometry)")
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
  
  if (length(list_name) == 1) {
    print(data %>% st_drop_geometry() %>% dplyr::select(!!as.name(outcome)) %>% group_by_all() %>% summarise(n()), n=Inf)
  } else {
    print(data %>% st_drop_geometry() %>% dplyr::select(all_of(!!as.name(list_name)), !!as.name(outcome)) %>% group_by_all() %>% summarise(n()), n=Inf)
  }
  
}

print_final_stats <- function(var, data=data, file_res=file_res){
  
  subset <- data %>% st_drop_geometry() %>% dplyr::select(pt, !!as.name(var))
  
  n = subset %>% nrow()
  cases <- subset %>% filter(!!as.name(var)==1) %>% nrow()
  controls <- subset %>% filter(!!as.name(var)==0) %>% nrow()
  nan <- subset %>% filter(is.nan(!!as.name(var))) %>% nrow()
  
  write("", file_res)
  write(paste("Statistics for", str_to_upper(var)), file_res)
  print(paste("Cases:", cases, "(", round(100*(cases/n),2), "%)")) %>% capture(file_res)
  print(paste("Controls:", controls, "(", round(100*(controls/n),2), "%)")) %>% capture(file_res)
  print(paste("Missing values:", nan, "(", round(100*(nan/n),2), "%)")) %>% capture(file_res)
  
}