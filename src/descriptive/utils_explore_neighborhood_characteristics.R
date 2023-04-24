# This code runs a logistic regression on CoLaus participants

library(tidyverse)
library(sf)

library(rgeoda)



# CLUSTERING FOR CONTINUOUS DATA ------------------------------------------

compute_corr_matrix <- function(df){
  
  corr.matrix <- df %>% cor(use = "pairwise.complete.obs") 
  corr.matrix %>% corrplot(type="upper", tl.cex = 0.7, tl.col="black")
  
  return(corr.matrix)
  
}

print_highly_correlated_vars <- function(corr.matrix, threshold=0.7){
  
  corr.high <- corr.matrix > threshold
  corr.high.ind <- which(corr.high & upper.tri(corr.matrix), arr.ind=TRUE)
  var1_names <- rownames(corr.matrix)[corr.high.ind[,1]]
  var2_names <- colnames(corr.matrix)[corr.high.ind[,2]]
  corr.high.df <- data.frame(var1 = var1_names, var2 = var2_names, cor = corr.matrix[corr.high & upper.tri(corr.matrix)])
  
  print(corr.high.df)
  
}

perc <- function(x, denominator){
  
  res = 100*(x/denominator)
  return(res)
  
}


create_queen_weights <- function(sf){
  
  sf <- sf %>% rownames_to_column(var = "swid")
  
  # Create spatial weights with the spdep package (for Global Moran's I)
  w.spdep <- poly2nb(sf, row.names="swid", queen=TRUE)
  
  # Create spatial weights with the rgeoda package (for Local Moran's I)
  w.rgeoda <- queen_weights(sf, order=1, include_lower_order = TRUE, precision_threshold = 0)
  
  return(list(spdep=w.spdep, rgeoda=w.rgeoda))
  
}


create_distance_weights <- function(sf, dist){
  
  sf <- sf %>% rownames_to_column(var = "swid")
  
  # Create spatial weights with the spdep package (for Global Moran's I)
  w.spdep <- dnearneigh(st_centroid(sf), 0, dist, row.names=NULL)
  
  # Create spatial weights with the rgeoda package (for Local Moran's I)
  w.rgeoda <- distance_weights(st_centroid(sf), dist)
  
  return(list(spdep=w.spdep, rgeoda=w.rgeoda))
  
}


compute_global_moran <- function(w, sf, var_name){
  
  var <- sf %>% pull(var_name)
  
  I = moran.test(var, listw = nb2listw(w, style="W", zero.policy = TRUE), zero.policy = TRUE, na.action=na.omit)
  
  return(I)
}


compute_local_moran <- function(w, sf, var_name, nperms=999, cutoff=0.05, ncpu=9, adjust='none'){
  
  var <- sf %>% select(!!as.name(var_name))
  lisa <- local_moran(w, var, permutations = nperms, significance_cutoff = cutoff, cpu_threads = ncpu, seed=123456789)
  res <- sf %>% mutate(lisa_I=lisa_values(lisa), lisa_pval=lisa_pvalues(lisa), lisa_clusters=lisa_clusters(lisa))
  
  if(adjust == 'fdr'){
    
    fdr <- lisa_fdr(lisa, cutoff)
    # print(paste("FDR p-value threshold:", fdr))
    # res <- res %>% mutate(lisa_clusters = factor(lisa_clusters(lisa, cutoff = fdr)))
    
    
    res <- res %>% mutate(lisa_pval_adj=p.adjust(lisa_pval, method = 'fdr', n = length(lisa_pval)))
    res$lisa_clusters[res$lisa_pval_adj>0.05] <- 0
    res <- res %>% mutate(lisa_clusters = factor(lisa_clusters))
    
  }
  
  colors <- c("0" = "#eeeeee", "1" = "#FF0000", "2" = "#0000FF", "3"="#a7adf9", "4" = "#f4ada8", "5" = "#464646", "6" ="#999999")
  labels <- c("0" = "NS", "1" = "HH", "2" = "LL", "3" = "LH", "4" = "HL", "5" = "Undefined", "6" = "Isolated")
  
  res <- res %>%
    mutate(lisa_labels = recode_factor(lisa_clusters, !!!labels)) %>%
    select(reli, !!as.name(var_name), lisa_I, lisa_pval, lisa_clusters, lisa_labels)
  
  res$lisa_labels <- factor(res$lisa_labels, levels = c("NS","HH","LL","LH","HL","Undefined","Isolated"))
  
  print(res %>% st_drop_geometry() %>% group_by(lisa_labels) %>% summarise(n()))
  
  return(res)
}


plot_lisa <- function(lisa.sf, boundary.sf, var_name, global_moran, interactive=TRUE){
  
  colors <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8", "#464646", "#999999")
  # labels <- c("NS", "HH", "LL", "LH", "HL", "Undefined", "Isolated")
  
  if(interactive==TRUE){
    tmap_mode("view")
    tmap_options(check.and.fix = TRUE)
  }
  else{
    tmap_mode("plot")
  }
  
  tm_shape(boundary.sf) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(lisa.sf) +
    tm_polygons(
      col = "lisa_labels",
      alpha=0.7,
      palette = colors,
      title = ""
    ) +
    tm_legend(text.size = 0.8) +
    tm_scale_bar(width=0.1) +
    #tm_compass(type="arrow") +
    tm_credits(paste0("I = ", round(global_moran$estimate[[1]],3), " (p-value=", global_moran$p.value, ")"),
               bg.color = "white",
               bg.alpha = 0.7
    ) +
    tm_layout(
      title = paste(var_name),
      fontfamily = "Open Sans",
      legend.outside = FALSE,
      legend.format = list(text.separator = "-")
    )
}






# # Kernel Density ----------------------------------------------------------
# 
# # See Waller and Gotway, p. 140
# 
# cases.density <- density.ppp(cases.ppp, sigma=bw.scott, at="pixels", edges=TRUE)
# controls.density <- density.ppp(controls.ppp, sigma=bw.scott, at="pixels", edges=TRUE)
# 
# par(mfrow = c(1, 2), mar = c(0,0,1.1,2))
# plot(cases.density, main=paste('Cases (n= ', cases.ppp$n, ' )'))
# plot(cases.ppp, add=TRUE, pch=1, cex=0.3, col='black', alpha=I(1/2))
# plot(controls.density, main=paste('Controls (n= ', controls.ppp$n, ' )'))
# plot(controls.ppp, add=TRUE, pch=1, cex=0.3, col='black', alpha=I(1/2))
# 
# plot(density(split(events.ppp), sigma=bw.scott, at="pixels", edges=TRUE), main = "")
# 
# contour(cases.density)
# 
# plot(spdensity(cases.ppp, sigma=bw.scott, at="pixels", edges=TRUE))
# 
# bw.scott(cases.ppp, isotropic=FALSE, d=NULL)
# bw.scott(controls.ppp, isotropic=FALSE, d=NULL)
# 
# spat_risk = relrisk(events.ppp, sigma=350, at="pixels", edge=TRUE, casecontrol=TRUE, relative=TRUE, diggle=TRUE, control="control")
# plot(spat_risk)
