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
                 legend_names = list(pt = TeX("Mean diff. in $\\delta ^{18}O$ level")))+ 
  ggtitle(TeX("Diff. in mean $\\delta ^{18}O$ in sim.-record >50samples, period > 600y")) +
  theme(plot.title = element_text(hjust = 0.5))

plot %>% ggsave(filename = paste('map_mean_diff_last1000_3', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

png(file = "Plots/Hist_mean_diff_last1000.png", width = 400, height = 400)
hist(MEAN_MAP$value, 
     breaks = 9,
     border = "black", 
     prob = TRUE,
     xlab = TeX("Diff in $\\delta^{18}O$ level (sim. - record)"),
     main = "")
lines(density(MEAN_MAP$value, na.rm = T),
      lwd = 2, 
      col = "black")
dev.off()

# hist(beaver1$temp, # histogram
#      col="peachpuff", # column color
#      border="black",
#      prob = TRUE, # show densities instead of frequencies
#      xlab = "temp",
#      main = "Beaver #1")
# lines(density(beaver1$temp), # density plot
#       lwd = 2, # thickness of line
#       col = "chocolate3"

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

#################################################
## Aufgeteilt auf Kontinente ####################

continents_used_entity <- c()
country_used_entity <- c()

for (entity in entities_used){
  site <- sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id)
  continents_used_entity = c(continents_used_entity, as.character(sisal_data$Continent[site]))
  country_used_entity = c(country_used_entity, as.character(sisal_data$Land[site]))
}
remove(site)

## Change Group because closer together

continents_used_entity[6] = "Europe"
continents_used_entity[22] <- "Europe"
continents_used_entity[4] <- continents_used_entity[7] <- continents_used_entity[9] <- continents_used_entity[15] <- continents_used_entity[20] <- continents_used_entity[21] <- continents_used_entity[29] <- "America"

for (continent in c("Africa", "Antarctica", "Asia", "Australia", "Europe", "MiddleEast", "America")){
  n = 0
  for (ii in 1:length(entities_used)){
    if(continents_used_entity[ii] == continent){
      n = n+1
    }
  }
  
  if(n == 0){
    next
  }
  
  colorscheme = RColorBrewer::brewer.pal(n, 'Spectral')
  
  png(file = paste0("Plots/Mean_Diff_Continents/", "Mean_Diff", "_in_", continent, ".png"), width = 600, height = 500)
  plot.new()
  par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
  plot(1, 
       type = "n",
       xlim = c(0,1150), 
       ylim = c(-7.5,7.5), 
       xlab = "years before 2000 CE", 
       ylab = "Diff in mean d18O (Sim-Record)", 
       main = paste0("Difference level of d18O for ", continent, " region" ))
  c = 1
  jj = 1
  legend_text = c()
  for (entity in entities_used){
    if (continents_used_entity[jj] == continent){
      name = paste0("ENTITY", entity)
      lines(DATA_last1000_mean[[name]]$value_record_age, 
            DATA_last1000_mean[[name]]$value_sim_d18O - DATA_last1000_mean[[name]]$value_record_d18O, 
            col = colorscheme[c],
            lwd = 2)  
      legend_text = c(legend_text, paste0("entity-", 
                                          entity, 
                                          "\n", 
                                          sisal_data$Land[sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id)],
                                          "\n"))
      c = c+1
    }
    jj = jj+1
    
  }
  legend("topright",
         inset=c(-0.3,0),
         legend_text,
         lty = numeric(n)+1, lwd = numeric(n)+2,
         col = colorscheme)
  dev.off()
}
remove(n, colorscheme, name, c, legend_text, jj)

#################################################
## Aufgeteilt nach Mean Temperature #############

# Mean Temp < 15 Grad, 15<T<25, >25 Grad

mean_temp_mask = numeric(length(entities_used))
temp_mask_counter = numeric(3)
ii = 1

for (entity in entities_used){
  site <- sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id)
  print(CAVES$climate_info$mean_temp[site])
  if(CAVES$climate_info$mean_temp[site] < 12.5){
    mean_temp_mask[ii] <- 1
    temp_mask_counter[1] = temp_mask_counter[1] + 1
  }else if (CAVES$climate_info$mean_temp[site] < 22.5){
    mean_temp_mask[ii] <- 2
    temp_mask_counter[2] = temp_mask_counter[2] + 1
  } else {
    mean_temp_mask[ii] <- 3
    temp_mask_counter[3] = temp_mask_counter[3] + 1
  }
  ii = ii+1  
}
remove(ii)

temp_names = c("T < 12.5°C", "12.5°C < T < 22.5°C", "T > 22.5°C")

for (ii in 1:3){
  colorscheme = RColorBrewer::brewer.pal(temp_mask_counter[ii], 'Spectral')
  
  png(file = paste0("Plots/Mean_Diff_Temp/", "Mean_Diff", "_T_", ii, ".png"), width = 600, height = 500)
  plot.new()
  par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
  plot(1, 
       type = "n",
       xlim = c(0,1150), 
       ylim = c(-7.5,7.5), 
       xlab = "years before 2000 CE", 
       ylab = "Diff in mean d18O (Sim-Record)", 
       main = paste0("Difference level of d18O for mean ", temp_names[ii] ))
  c = 1
  jj = 1
  legend_text = c()
  for (entity in entities_used){
    if (mean_temp_mask[jj] == ii){
      name = paste0("ENTITY", entity)
      lines(DATA_last1000_mean[[name]]$value_record_age, 
            DATA_last1000_mean[[name]]$value_sim_d18O - DATA_last1000_mean[[name]]$value_record_d18O, 
            col = colorscheme[c],
            lwd = 2)  
      legend_text = c(legend_text, paste0("entity-", entity))
      c = c+1
    }
    jj = jj+1
  }
  legend("topright",
         inset=c(-0.3,0),
         legend_text,
         lty = numeric(temp_mask_counter[ii])+1, lwd = numeric(temp_mask_counter[ii])+2,
         col = colorscheme)
  dev.off()
}

remove(mean_temp_mask, temp_mask_counter, temp_names)

#################################################
## Aufgeteilt nach Mean Prec ####################

# Mean Prec < 3e-5, 3e-5<P<5e-5, >5e-5 

mean_prec_mask = numeric(length(entities_used))
prec_mask_counter = numeric(3)
ii = 1

for (entity in entities_used){
  site <- sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id)
  if(CAVES$climate_info$mean_prec[site] < 3e-5){
    mean_prec_mask[ii] <- 1
    prec_mask_counter[1] = prec_mask_counter[1] + 1
  }else if (CAVES$climate_info$mean_prec[site] < 5e-5){
    mean_prec_mask[ii] <- 2
    prec_mask_counter[2] = prec_mask_counter[2] + 1
  } else {
    mean_prec_mask[ii] <- 3
    prec_mask_counter[3] = prec_mask_counter[3] + 1
  }
  ii = ii+1  
}
remove(ii)

prec_names = c("P < 3e-5 kg/m²/s", "5e-5 kg/m²/s < P < 3e-5 kg/m²/s", "P > 5e-5 kg/m²/s")

for (ii in 1:3){
  colorscheme = RColorBrewer::brewer.pal(prec_mask_counter[ii], 'Spectral')
  
  png(file = paste0("Plots/Mean_Diff_Prec/", "Mean_Diff", "_T_", ii, ".png"), width = 600, height = 500)
  plot.new()
  par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
  plot(1, 
       type = "n",
       xlim = c(0,1150), 
       ylim = c(-7.5,7.5), 
       xlab = "years before 2000 CE", 
       ylab = "Diff in mean d18O (Sim-Record)", 
       main = paste0("Difference level of d18O for mean ", prec_names[ii] ))
  c = 1
  jj = 1
  legend_text = c()
  for (entity in entities_used){
    if (mean_prec_mask[jj] == ii){
      name = paste0("ENTITY", entity)
      lines(DATA_last1000_mean[[name]]$value_record_age, 
            DATA_last1000_mean[[name]]$value_sim_d18O - DATA_last1000_mean[[name]]$value_record_d18O, 
            col = colorscheme[c],
            lwd = 2)  
      legend_text = c(legend_text, paste0("entity-", entity))
      c = c+1
    }
    jj = jj+1
  }
  legend("topright",
         inset=c(-0.3,0),
         legend_text,
         lty = numeric(prec_mask_counter[ii])+1, lwd = numeric(prec_mask_counter[ii])+2,
         col = colorscheme)
  dev.off()
}

remove(mean_prec_mask, prec_mask_counter, prec_names)

