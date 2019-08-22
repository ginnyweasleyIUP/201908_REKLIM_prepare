#################################################
## Analysis of the mean and var differences #####
#################################################

##TODO: Nur die letzten 1000 Jahre einschließen, sonst bringt Varianz nichts!!! 
#       verwende nur die Liste an samples, die mit Carla zusammen ausgesucht wurden!!!
#       Finde besseren NAmen als Pfusch

DATA_compare_mean_var <- data.frame(
  mean_isot_sim = numeric(519),
  mean_isot_sisal = numeric(519),
  diff_mean = numeric(519),
  var_isot_sim =numeric(519),
  var_isot_sisal =numeric(519),
  diff_var =numeric(519)
)

DATA_pfusch <-data.frame(
  mean_isot_sim = numeric(212),
  mean_isot_sisal = numeric(212),
  diff_mean = numeric(212),
  var_isot_sim = numeric(212),
  var_isot_sisal = numeric(212),
  diff_var = numeric(212)
)

for (ii in 1:519){
  #s286 <- sample_tb %>% filter(entity_id == 286)
  #lines(s286$interp_age, movavg(s286$d18O_measurement,10), xlim=c(-49,1100), type = "l", col = "#B2182B", lwd = 2)
  entity_data <- sample_tb %>% filter(entity_id == ii)
  DATA_compare_mean_var$mean_isot_sisal[ii] = mean(entity_data$d18O_measurement)
  DATA_compare_mean_var$var_isot_sisal[ii] = cov(entity_data$d18O_measurement, entity_data$d18O_measurement)
  pos = site_tb$site_id[ii]
  DATA_compare_mean_var$mean_isot_sim[ii] = mean(CAVES$yearly_data$isot[[pos]])
  DATA_compare_mean_var$var_isot_sim[ii] = cov(CAVES$yearly_data$isot[[pos]],CAVES$yearly_data$isot[[pos]])
  
  DATA_compare_mean_var$diff_mean[ii] <- DATA_compare_mean_var$mean_isot_sim[ii]-DATA_compare_mean_var$mean_isot_sisal[ii]
  DATA_compare_mean_var$diff_var[ii] <- DATA_compare_mean_var$var_isot_sim[ii]-DATA_compare_mean_var$var_isot_sisal[ii]
}

for (ii in 1:212){
  DATA_pfusch$mean_isot_sim[ii] = mean(CAVES$yearly_data$isot[[ii]])
  DATA_pfusch$var_isot_sim[ii] = cov(CAVES$yearly_data$isot[[ii]],CAVES$yearly_data$isot[[ii]])
  entity_range = site_tb%>% filter(site_id == ii)
  nn = 0
  mean = 0
  var = 0
  for (jj in entity_range$entity_id){
    if(is.na(DATA_compare_mean_var$mean_isot_sisal[jj])){
      next
    }else if(is.na(DATA_compare_mean_var$var_isot_sisal[jj])){
      next
    }
    mean = mean + DATA_compare_mean_var$mean_isot_sisal[jj]
    var = var + DATA_compare_mean_var$var_isot_sisal[jj]
    nn = nn+1
  }
  DATA_pfusch$mean_isot_sisal[ii] = mean/nn
  DATA_pfusch$var_isot_sisal[ii] = var/nn
  
  DATA_pfusch$diff_mean[ii] =  DATA_pfusch$mean_isot_sim[ii] - DATA_pfusch$mean_isot_sisal[ii]
  DATA_pfusch$diff_var[ii] = DATA_pfusch$var_isot_sim[ii] - DATA_pfusch$var_isot_sisal[ii]
}


#################################################
## NOW PLOT MAPS TO IT###########################
#################################################

#Here I only plot the pfusched data, as it is easier to plot. 
#I hope this is mathematically correct, and I will try se see if there is a better system to it. 

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)
library(stringr)
library(rgdal)
library(sp)

layer <- ogrListLayers("Nat_Earth_Data/ne_10m_coastline.shp")
coastline_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_coastline.shp", verbose = FALSE, layer= layer)
land_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_land.shp", verbose = FALSE)
ocean_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_ocean.shp", verbose = FALSE)

#MAP 1: MEAN DIFFERENCE##########################

world_map <- map_data('world')

MEAN_MAP <- data_frame(
  value = DATA_pfusch$diff_mean,
  x_lon = CAVES$site_info$longitude,
  y_lat = CAVES$site_info$latitude
)

plot <- ggplot() + geom_polygon(data = land_map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
  coord_fixed(1.3) + 
  geom_point(data = MEAN_MAP, aes(x = x_lon, y = y_lat, color = value)) + 
  scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Mean Diff.") +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Difference in mean between the d18O in local prec \nfrom simulation to d18O measured in calcite inside cave")+
  xlab("")+ ylab("")+
  ylim(-60,80) + 
  xlim(-170, 180) +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

plot %>% ggsave(filename = paste('map_mean_diff', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

#MAP 2: VAR DIFFERENCE###########################

VAR_MAP <- data_frame(
  value = DATA_pfusch$diff_var,
  x_lon = CAVES$site_info$longitude,
  y_lat = CAVES$site_info$latitude
)

plot <- ggplot() + geom_polygon(data = land_map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
  coord_fixed(1.3) + 
  geom_point(data = VAR_MAP, aes(x = x_lon, y = y_lat, color = value)) + 
  scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Mean Diff.") +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Difference in variance between the d18O in local prec \nfrom simulation to d18O measured in calcite inside cave")+
  xlab("")+ ylab("")+
  ylim(-60,80) + 
  xlim(-170, 180) +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

plot %>% ggsave(filename = paste('map_var_diff', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')
