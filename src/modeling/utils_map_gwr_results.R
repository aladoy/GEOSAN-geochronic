require(ggspatial)

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

# LOAD BASEMAP LAYERS
lausanne <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
lake_1 <- st_read("../qgis/lake_border_clean_1rst.geojson")
lake_2 <- st_read("../qgis/lake_border_clean_2nd.geojson")


# PLOT BASEMAP
plot_basemap <- function(){
  
  p <- ggplot() +
    geom_sf(data = lausanne, color = "grey", fill = NA, lwd=1.5) +
    geom_sf(data = lake_2, fill = "#c9e5f3", color = "#c9e5f3", alpha = 1) +
    geom_sf(data = lake_1, fill = "#9fd1ea", color = "#9fd1ea", alpha = 0.7) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.title = element_text(size = 16),
          legend.position = "right",
          legend.title = element_text(size = 12),
          legend.margin = margin(0, 5, 0, 0),
          legend.text = element_text(size = 12),
          text = element_text(family = "Ubuntu Regular")) +
    labs(x = NULL, y = NULL) +
    annotation_scale(location = "br", width_hint = 0.2, style="ticks") 
  
  return(p)
  
}


plot_tol_contours <- function(p, tol){
  
  p <- p + 
    geom_sf(data = tol[tol$RISK == 0.01, ], linetype = "solid", color = "black", size=1) +
    geom_sf(data = tol[tol$RISK == 0.05, ], linetype = "dashed", color = "black", size=1) +
    scale_linetype_manual(values = c("0.01" = "solid", "0.05" = "dashed"))
  
  return(p)
}


extract_title_name <- function(var){
  
  if (startsWith(var, "mgwr_")) {
    var_name <- gsub("mgwr_|_TC", "", var)
    title_name <- paste("MGWR -", var_name)
  } else{
    var_name <- gsub("gwr_|_TC", "", var)
    title_name <- paste("GWR -", var_name)
  }
  
  return(list(var_name, title_name))
}


map_association <- function(df, var, outcome, tol){
  
  var_name <-  extract_title_name(var)[[1]]
  title_name <-  extract_title_name(var)[[2]]
  
  # Create the plot
  p <- plot_basemap() +
    geom_sf(data = df, aes(fill = factor(ifelse(!!as.name(var) > 0, "Positive", ifelse(!!as.name(var) < 0, "Negative", "Not significant")))),
            color = "grey", shape = 21, size = 2, alpha = 0.7) +
    scale_fill_manual(values = c("Not significant" = "white", "Positive" = "red", "Negative" = "blue"), 
                      guide=guide_legend(title="Spatial association"))
  # geom_sf(data = gwr, aes(fill = !!as.name(var)),
  #         color = "grey", shape = 21, size = 2, alpha = 0.7) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      midpoint = 0, na.value = "grey", guide = guide_colorbar(title = "", frame.colour="black")) +
  
  p <- plot_tol_contours(p, tol) +
    labs(title=title_name)
  
  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/", outcome, "/", var_name, "/map_spatial_association_", var, ".png"),
         bg = "white", width = 200, height = 150, units = "mm", dpi = 300)
  
}


map_coefficients <- function(df, var, outcome, tol, type="estimates"){
  
  var_name <-  extract_title_name(var)[[1]]
  title_name <-  extract_title_name(var)[[2]]
  
  if(type=="estimates"){
    legend_title <- "Coeff. estimate"
    filename <- paste0("map_estimates_", var, ".png")
  }else if(type=="t-values"){
    legend_title <- "T-values"
    filename <- paste0("map_tvalues_", var, ".png")
    var <- paste0(var, "_Tvalues")
  }
  
  
  p <- plot_basemap() +
    geom_sf(data = df, aes(fill = !!as.name(var)),
            color = "grey", shape = 21, size = 2, alpha = 0.7) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, na.value = "grey", guide = guide_colorbar(title = legend_title, frame.colour="black"))
  
  p <- plot_tol_contours(p, tol) +
    labs(title=title_name)
  
  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/",outcome,"/", var_name, "/", filename),
         bg = "white", width = 200, height = 150, units = "mm", dpi = 300)
  
}


map_condition_number <- function(df, outcome, tol, model="gwr"){
  
  if(model=="gwr"){
    var <- paste0(model, "_CN")
    title_name <- paste0("GWR Condition Number - ", outcome)
  }else if(model=="mgwr"){
    var <- paste0(model, "_CN")
    title_name <-  paste0("MGWR Condition Number - ", outcome)
  }
  
  # Create the plot
  p <- plot_basemap() +
    geom_sf(data = df, aes(fill = !!as.name(var)),
            color = "grey", shape = 21, size = 2, alpha = 0.7) +
    scale_fill_viridis_c(name = "CN [-]", guide = guide_colorbar(frame.colour = "black"))
  
  p <- plot_tol_contours(p, tol) +
    labs(title=title_name)
  
  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/", outcome, "/map_CN_", model, ".png"),
         bg = "white", width = 200, height = 150, units = "mm", dpi = 300)
  
}