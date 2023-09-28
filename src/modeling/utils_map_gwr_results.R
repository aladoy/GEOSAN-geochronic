require(ggspatial)

con <- dbConnect(drv=RPostgreSQL::PostgreSQL(),host = "localhost",user= "aladoy",rstudioapi::askForPassword(),dbname="geosan")

# LOAD BASEMAP LAYERS
lausanne <- read_sf(con, query="SELECT * FROM lausanne_sectors_extent")
lake_1 <- st_read("../qgis/lake_border_clean_1rst.geojson")
lake_2 <- st_read("../qgis/lake_border_clean_2nd.geojson")
labels <- st_read("../qgis/labels.gpkg")

# PLOT BASEMAP
plot_basemap <- function(){
  
  p <- ggplot() +
    geom_sf(data = lausanne, color = "grey", fill = NA, lwd=1) +
    geom_sf(data = lake_2, fill = "#c9e5f3", color = "#c9e5f3", alpha = 1) +
    geom_sf(data = lake_1, fill = "#9fd1ea", color = "#9fd1ea", alpha = 0.7) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA, size=1),
          panel.background = element_blank(),
          plot.title = element_text(size = 14, hjust=0.5, margin = margin(5, 0, 5, 0)),
          plot.margin = margin(0, 0, 0, 0),
          legend.position = c(0.15, 0.78),
          legend.box="vertical",
          legend.title = element_text(size = 10),
          legend.key.height = unit(1, "lines"),
          legend.margin = margin(0, 5, 0, 0),
          legend.text = element_text(size = 9),
          text = element_text(family = "Ubuntu Regular")) +
    labs(x = NULL, y = NULL) +
    annotation_scale(location = "bl", width_hint = 0.2, style="ticks") +
    annotate("text", x = 2536430, y = 1151250, label = "Lake Geneva", fontface = "italic", color = "#487388", size=2.5, angle = -20)

  return(p)

}


plot_tol_contours <- function(p, tol){
  
  p <- p + 
    geom_sf(data = tol[tol$RISK == 0.01, ], aes(linetype = "99% Tol. Interval"), color = "black", size = 1) +
    geom_sf(data = tol[tol$RISK == 0.05, ], aes(linetype = "95% Tol. Interval"), color = "black", size = 1) +
    scale_linetype_manual(values = c("99% Tol. Interval" = "solid", "95% Tol. Interval" = "longdash"),
                          guide = guide_legend(title = "High-risk areas",
                                               override.aes = list(color = "black", size = 1),
                                               title.position = "top")) 
  
  return(p)
}


add_labels <- function(p){
  
  p <- p +
    geom_sf_label(data = labels, aes(label = Number, fontface = "bold"), color = "black",
                  size = 3, hjust = 0.5, vjust = 0.5,
                  show.legend = FALSE, alpha=0.7,
                  label.size = NA)

  return(p)  
}


extract_title_name <- function(var){
  
  if (startsWith(var, "mgwr_")) {
    var_name <- gsub("mgwr_|_TC", "", var)
  } else{
    var_name <- gsub("gwr_|_TC", "", var)
  }
  
  title_name <- switch(var_name,
                       "intercept" = "Intercept",
                       "INTDEN" = "Street connectivity",
                       "GREEN_SP" = "Greenness",
                       "NOISE" = "Nighttime noise",
                       "PM25" = "PM2.5 exposure",
                       "NO2" = "NO2 exposure",
                       "MEDREV" = "Median income",
                       "R_UNEMP" = "Unemployment rate",
                       "R_NN_POBL" = "Compulsory education",
                       "R_NN_CH" = "Foreign population",
                       "Unknown variable")
  
  return(list(var_name = var_name, title_name = title_name))
}


map_association <- function(df, var, outcome, tol, model="GWR"){
  
  var_name <-  extract_title_name(var)[[1]]
  title_name <-  extract_title_name(var)[[2]]
  
  # Create the plot
  p <- plot_basemap() +
    geom_sf(data = df, aes(fill = factor(ifelse(!!as.name(var) > 0, "Positive", ifelse(!!as.name(var) < 0, "Negative", "Not significant")))),
            color = "grey", shape = 21, size = 1.5, alpha = 0.7) +
    scale_fill_manual(values = c("Not significant" = "white", "Positive" = "red", "Negative" = "blue"),
                      guide=guide_legend(title=paste0("Association (", model, ")")))
  # geom_sf(data = gwr, aes(fill = !!as.name(var)),
  #         color = "grey", shape = 21, size = 2, alpha = 0.7) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      midpoint = 0, na.value = "grey", guide = guide_colorbar(title = "", frame.colour="black")) +
  
  p <- plot_tol_contours(p, tol)
  p <- add_labels(p) +
    labs(title=title_name)
  
  p <- p + guides(fill = guide_legend(title = paste0("Association (", model, ")"), order = 1),
                  linetype = guide_legend(title = "High-risk areas", order=2))

  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/", outcome, "/", var_name, "/map_spatial_association_", var, ".png"),
         bg = "#f5f5f2", width = 150, height = 120, units = "mm", dpi = 300)
  
}


map_coefficients <- function(df, var, outcome, tol, type="estimates", model="GWR"){
  
  var_name <-  extract_title_name(var)[[1]]
  title_name <-  extract_title_name(var)[[2]]
  
  if(type=="estimates"){
    legend_title <- paste0("Coeff. estimate (", model, ")")
    filename <- paste0("map_estimates_", var, ".png")
  }else if(type=="t-values"){
    legend_title <- paste0("T-values (", model, ")")
    filename <- paste0("map_tvalues_", var, ".png")
    var <- paste0(var, "_Tvalues")
  }
  
  
  p <- plot_basemap() +
    geom_sf(data = df, aes(fill = !!as.name(var)),
            color = "grey", shape = 21, size = 1.5, alpha = 0.7) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, na.value = "grey", 
                         guide = guide_colorbar(title = legend_title),
                         expand = c(0, 0))
  
  p <- plot_tol_contours(p, tol)
  
  p <- add_labels(p) +
    labs(title=title_name)
  
  p <- p + guides(fill = guide_colorbar(title = legend_title, label.theme = element_text(size = 8), order=1,  frame.colour="black"), linetype = guide_legend(title = "High-risk areas", order=2))
  
  
  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/",outcome,"/", var_name, "/", filename),
         bg = "#f5f5f2", width = 150, height = 120, units = "mm", dpi = 300)
  
}


map_condition_number <- function(df, outcome, tol, model="mgwr"){
  
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
            color = "grey", shape = 21, size = 1.5, alpha = 0.7) +
    scale_fill_viridis_c(name = "CN [-]", guide = guide_colorbar(frame.colour = "black"))
  
  p <- plot_tol_contours(p, tol)
  p <- add_labels(p) +
    labs(title=title_name)
  
  print(p)
  
  # Save the plot
  ggsave(paste0("../results/regression_models/", outcome, "/map_CN_", model, ".png"),
         bg = "#f5f5f2", width = 150, height = 120, units = "mm", dpi = 300)
  
}