#################################################
##Verwende nur last 1000 years+min50 sample######
#################################################

#verwende hierfür sample_final für Daten
#verwende sites_entities_used für site-entity Tabelle

tab_length = dim(sites_entities_used)[1]

DATA_last1000 <-data.frame(
  mean_isot_sim = numeric(tab_length),
  mean_isot_sisal = numeric(tab_length),
  diff_mean = numeric(tab_length),
  var_isot_sim = numeric(tab_length),
  var_isot_sisal = numeric(tab_length),
  diff_var = numeric(tab_length)
)

for (ii in 1:tab_length){
  site = sites_entities_used$site_id[ii]
  entity = sites_entities_used$entity_id[ii]
  #s286 <- sample_tb %>% filter(entity_id == 286)
  #lines(s286$interp_age, movavg(s286$d18O_measurement,10), xlim=c(-49,1100), type = "l", col = "#B2182B", lwd = 2)
  entity_data = sample_final %>% filter(entity_id == entity)
  DATA_last1000$mean_isot_sisal[ii] = mean(entity_data$d18O_measurement)
  DATA_last1000$var_isot_sisal[ii] = cov(entity_data$d18O_measurement, entity_data$d18O_measurement)
  DATA_last1000$mean_isot_sim[ii] = mean(CAVES$yearly_data$isot[[site]])
  DATA_last1000$var_isot_sim[ii] = cov(CAVES$yearly_data$isot[[site]],CAVES$yearly_data$isot[[site]])
  
  DATA_last1000$diff_mean[ii] <- DATA_last1000$mean_isot_sim[ii]-DATA_last1000$mean_isot_sisal[ii]
  DATA_last1000$diff_var[ii] <- DATA_last1000$var_isot_sim[ii]/DATA_last1000$var_isot_sisal[ii]
  remove(site,entity, entity_data)
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
  long = CAVES$site_info$longitude[sites_entities_used$site_id],
  lat = CAVES$site_info$latitude[sites_entities_used$site_id],
  value = DATA_last1000$diff_mean
)

plot <- STACYmap(ptlyr = MEAN_MAP, 
                 zoom = c(-180,-60,180,73), 
                 legend_names = list(pt = "Mean diff. in d18O level"))+ 
  ggtitle("Diff in mean d18O level btw simulated P and measured calcite in cave") +
  theme(plot.title = element_text(hjust = 0.5))

plot %>% ggsave(filename = paste('map_mean_diff_last1000_2', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

png(file = "Plots/Hist_mean_diff_last1000.png", width = 400, height = 400)
hist(MEAN_MAP$value, breaks = 8, xlab = "Difference in d18O (simulation - record)", main = "Histogramm mean difference in d18O level")
dev.off()

#MAP 2: VAR DIFFERENCE###########################

var_mask <- DATA_last1000$diff_var
var_mask[var_mask > 7] = NA
var_mask[!is.na(var_mask)] = 1

VAR_MAP <- data_frame(
  long = CAVES$site_info$longitude[sites_entities_used$site_id],
  lat = CAVES$site_info$latitude[sites_entities_used$site_id],
  value = var_mask*DATA_last1000$diff_var
)

plot <- STACYmap(ptlyr = VAR_MAP, 
                 zoom = c(-180,-60,180,73),
                 colorscheme = RColorBrewer::brewer.pal(9, 'YlOrRd')[1:9],
                 legend_names = list(pt = "Ratio in Var. Sim/Record"))+
  ggtitle("Diff in var of d18O btw simulated P and measured calcite in cave")+
  theme(plot.title = element_text(hjust = 0.5))

plot %>% ggsave(filename = paste('map_var_diff_last1000_2', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

png(file = "Plots/Hist_var_diff_last1000.png", width = 400, height = 400)
hist(DATA_last1000$diff_var, breaks = 8, xlab = "Difference in var. of d18O (simulation/record)", main = "Histogramm variance ratio of d18O level")
dev.off()

png(file = "Plots/Hist_var_diff_last1000_log.png", width = 400, height = 400)
hist(log(DATA_last1000$diff_var), breaks = 8, xlab = "Difference in var. of d18O (simulation/record)", main = "Histogramm variance ratio of d18O level")
dev.off()


#################################################
## Plot DIFF MEAN OVER TIME #####################
#################################################

# Try for first cave

DATA_last1000_mean <- vector(mode = "list")

for (ii in sites_entities_used$entity_id){
  
  name = paste0("ENTITY", ii)
  entity_data = sample_final %>% filter(entity_id == ii)
  
  site <- entity_data$site_id[1]
  
  DATA_last1000_mean[[name]]$value_record_age <- entity_data$interp_age
  DATA_last1000_mean[[name]]$value_record_d18O <- entity_data$d18O_measurement
  DATA_last1000_mean[[name]]$value_sim_d18O <- SubsampleTimeseriesBlock_highresNA(ts(data = rev(CAVES$yearly_data$isot[[site]]), 
                                                                                     start = -50, 
                                                                                     end = 1100), 
                                                                                  entity_data$interp_age)
  
  remove(name,entity_data, site)
  
  
  
}

plot(DATA_last1000_mean[[1]]$value_record_age, DATA_last1000_mean[[1]]$value_sim_d18O-DATA_last1000_mean[[1]]$value_record_d18O, type = "l", ylim = c(-5,5))
lines(DATA_last1000_mean[[2]]$value_record_age, DATA_last1000_mean[[2]]$value_sim_d18O-DATA_last1000_mean[[2]]$value_record_d18O, col = "red")
lines(DATA_last1000_mean[[3]]$value_record_age, DATA_last1000_mean[[3]]$value_sim_d18O-DATA_last1000_mean[[3]]$value_record_d18O, col = "blue")
lines(DATA_last1000_mean[[4]]$value_record_age, DATA_last1000_mean[[4]]$value_sim_d18O-DATA_last1000_mean[[4]]$value_record_d18O, col = "green")
lines(DATA_last1000_mean[[5]]$value_record_age, DATA_last1000_mean[[5]]$value_sim_d18O-DATA_last1000_mean[[5]]$value_record_d18O, col = "yellow")
lines(DATA_last1000_mean[[6]]$value_record_age, DATA_last1000_mean[[6]]$value_sim_d18O-DATA_last1000_mean[[6]]$value_record_d18O, col = "darkblue")
lines(DATA_last1000_mean[[7]]$value_record_age, DATA_last1000_mean[[7]]$value_sim_d18O-DATA_last1000_mean[[7]]$value_record_d18O, col = "cyan")
