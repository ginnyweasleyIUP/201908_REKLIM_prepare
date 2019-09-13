#################################################
## Tile World Maps ##############################
#################################################

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
library(latex2exp)
library(tidyverse)

setwd("/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/")
#basedir<-"/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/"

#As an example, let's only try it for site 27 and 104


geom_spoke_legend <- function(r, x, y, n = 5, scaled = c(2,8), 
                              arrow = NULL, label = NULL) {
  
  dd <- data.frame(breaks = scales::pretty_breaks(n)(r))
  dd$radius <- scales::rescale(dd$breaks, to = scaled, from = range(r, na.rm = T, finite = T))
  dd$xpos <- seq(0, by = round(max(scaled)), length.out = length(dd$breaks))
  dd$xpos <- dd$xpos + x
  dd$ypos <- rep(y, length(dd$breaks))
  
  
  
  g <- list(
    geom_spoke(data = dd,
               aes(x = xpos, y = ypos + 2, radius = radius),
               angle = pi/4, arrow = arrow,
               inherit.aes = F),
    geom_text(data = dd,
              aes(x = xpos, y = ypos, label = format(breaks)),
              hjust = 0, vjust = 0, size = 3,
              inherit.aes = F)
  )
  
  if (!is.null(label)) {
    dd1 <- dd[1,]
    dd1$label <- label
    
    g <- c(g,
           geom_text(data = dd1,
                     aes(x = xpos, y = ypos-2, label = label),
                     hjust = 0, vjust = 0, size = 3,
                     inherit.aes = F))
  }
  
  g
}

#################################################

sites_used_short = c(27,104)

#################################################
## Read in Wind Data ############################
#################################################

# SIM_DATA_past1000_wind_raw <- list(
#   RAW = list(
#     westerly_raw = "array",
#     southerly_raw = "array",
#     prec_raw = "array"
#   )
# )

#################################################
## FILL DATA ####################################
#################################################

# prec <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_precipitation.nc")
# SIM_DATA_past1000_wind_raw$RAW$prec_raw <- as.array(ncdf4::ncvar_get(prec)[,1:72,1:13790])
# ncdf4::nc_close(prec)
# remove(prec)
# 
# wind_u <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_westerly_u.nc")
# SIM_DATA_past1000_wind_raw$RAW$westerly_raw <- as.array(ncdf4::ncvar_get(wind_u))
# ncdf4::nc_close(wind_u)
# remove(wind_u)
# 
# wind_v <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_southerly_v.nc")
# SIM_DATA_past1000_wind_raw$RAW$southerly_raw <- as.array(ncdf4::ncvar_get(wind_v))
# ncdf4::nc_close(wind_v)
# remove(wind_v)

#################################################
## Random Plots #################################
#################################################

# Temp_map <-data.frame(
#   value = as.vector(SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN),
#   x_lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),73),
#   y_lat = rep(seq(88.5, -88.5, length.out = 73), each = 96)
# )
# 
# Wind_map_27 <- data.frame(
#   radius = as.vector(SIM_DATA_past1000_wind$CAVE27$WIND_MEAN_STRENGTH),
#   angle = as.vector(SIM_DATA_past1000_wind$CAVE27$WIND_MEAN_ANGLE),
#   x_lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
#   y_lat = rep(seq(88.5, -88.5, length.out = 72), each = 96)
# )
# 
# West_wind_27 <- data.frame(
#   radius = as.vector(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[, , 1]),
#   angle = numeric(6912),#+pi/2,
#   x_lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
#   y_lat = rep(seq(88.5, -88.5, length.out = 72), each = 96)
# )
# 
plot <- ggplot() +  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +coord_fixed(1.3) +
  geom_tile(data = Temp_map, aes(x = x_lon, y = y_lat, fill = value), size = 50) + #(Non significant)
  scale_fill_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Temp") +
  geom_spoke(data = Wind_map_27, aes(x=x_lon, y=y_lat, angle = angle, radius = radius), arrow = arrow(length = unit(.04, 'inches')),
             show.legend = c(F,T)) +
  scale_size(name = "arrows", range = c(0, 10), guide = "legend") +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5)

#geom_spoke_legend(wind.dt$mean_wind, x = 186, y = 15.5, 
                  #arrow = arrow(length = unit(.05, 'inches')),
                  #label = "Mean windspeed")

plot

# ggplot(Wind_map_27,
#        aes(x = x_lon ,
#            y = y_lat,
#            fill = angle,
#            angle = angle,
#            radius = scales::rescale(radius, c(.2, .8)))) +
#   geom_raster() +
#   geom_spoke(arrow = arrow(length = unit(.05, 'inches'))) +
#   scale_fill_distiller(palette = "RdYlGn") +
#   coord_equal(expand = 0) +
#   theme(legend.position = 'bottom',
#         legend.direction = 'horizontal')

#################################################
## YEARLY AND SEASONAL MEANS ####################
#################################################

SIM_DATA_past1000_wind <- vector(mode = "list")

#Simulation starts at 1.June -> 1:3 = JJA

print("Start Calculation of Wind Strengths")

for (ii in sites_used){
  
  name = paste0("CAVE", ii)
  
  print(name)
  
  SIM_DATA_past1000_wind[[name]] <- list(
    WIND_MEAN_STRENGTH = array(dim = c(96,72)),
    WIND_MEAN_STRENGTH_PREC = array(dim = c(96,72)),
    WIND_MEAN_ANGLE = array(dim = c(96,72)),
    WIND_MEAN_ANGLE_PREC = array(dim = c(96,72))
  )
  
  msk_summer = rep(c(1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA), 1149) #JJA
  msk_autumn = rep(c(NA,NA,NA,1,1,1,NA,NA,NA,NA,NA,NA), 1149) #SON
  msk_winter = rep(c(NA,NA,NA,NA,NA,1,1,1,NA,NA,NA,NA), 1149) #DJF
  msk_spring = rep(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1), 1149) #MAM
  
  
  
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_summer <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_autumn <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_winter <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_spring <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_summer <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_autumn <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_winter <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_spring <- array(dim = c(96,72))
  
  lon_raw <- CAVES$site_info$longitude[ii]
  if(lon_raw<0){
    lon_raw = lon_raw+360
  }
  
  lon_cave = ceiling(lon_raw/360*96)
  lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
  
  for (lon in 1:96){
    print(paste0("   lon: ", lon))
    for (lat in 1:72){
      
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH[lon,lat] = mean(sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]^2 + 
                                                                               SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,]^2), na.rm = T)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE[lon,lat] = mean(atan2(SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,], SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]), na.rm = TRUE)
      
      #precipitation weighted --> PREC OF THE CAVE!!!
      #lon_cave = ceiling(lon_raw/360*96)
      #lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
      #SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_PREC[lon,lat] = sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,] * 
      #                                                                        sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]^2 + 
      #                                                                               SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,]^2), na.rm = TRUE) / sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,], na.rm = TRUE)
      #SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_PREC[lon,lat] = sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,]*
      #                                                                     atan(- SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,]/SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]), na.rm = TRUE) / sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,], na.rm = TRUE)
      
      
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_summer[lon,lat] = mean(msk_summer*sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]^2 + 
                                                                               SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788]^2), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_summer[lon,lat] = mean(msk_summer*atan2(SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788], 
                                                                                      SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_autumn[lon,lat] = mean(msk_autumn*sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]^2 + 
                                                                                                 SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788]^2), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_autumn[lon,lat] = mean(msk_autumn*atan2(SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788], 
                                                                                             SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_winter[lon,lat] = mean(msk_winter*sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]^2 + 
                                                                                                 SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788]^2), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_winter[lon,lat] = mean(msk_winter*atan2(SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788], 
                                                                                             SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_spring[lon,lat] = mean(msk_spring*sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]^2 + 
                                                                                                 SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788]^2), na.rm = TRUE)
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_spring[lon,lat] = mean(msk_spring*atan2(SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,1:13788], 
                                                                                             SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,1:13788]), na.rm = TRUE)
      
      
    }
  }
  
  
  
}

print("Wind Strength Calc done!!!")

remove(msk_summer, msk_autumn, msk_winter, msk_spring)

# summer_wind_27 <- data.frame(
#   lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
#   lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
#   angle = as.vector(SIM_DATA_past1000_wind$CAVE27$WIND_MEAN_ANGLE_spring),
#   radius = as.vector(SIM_DATA_past1000_wind$CAVE27$WIND_MEAN_STRENGTH_spring)
# )

# plot <- ggplot() +  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +coord_fixed(1.3) +
#   geom_tile(data = Temp_map, aes(x = x_lon, y = y_lat, fill = value), size = 50) + #(Non significant)
#   scale_fill_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Temp") +
#   geom_spoke(data = summer_wind_27, aes(x=lon, y=lat, angle = angle, radius = radius), size = .2, arrow = arrow(length = unit(.03, 'inches')), alpha = 0.5) +
#   geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5)
# 
# plot <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN[49:96,1:73], SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN[1:48,1:73]),
#                  ptlyr = SITE_MAP,
#                  fldlyr = summer_wind_27,
#                  zoom = c(-180, -60, 180, 73),
#                  #legend_names = list(grid = "Corr T-I"),
#                  graticules = TRUE)+#,
#                  #centercolor = list(grid = "white", pt = "white")) +
#   ggtitle(paste0("Corr. of global temp w/ local isot-comp, p<0.1, site ", ii)) +
#   theme(plot.title = element_text(h = 0.5))
# 
# plot


#########################
## TO DO AM DONNERSTAG

# * dann teleconnection-wise alle significanten Punkte mit einbeziehen
# * STACYmap aus Github --> branch einarbeiten?!?



#--> Create list with assigned names for Cave sites used, where matrix of precipitation weighted average wind size is saved

#################################################
## POINT CORRELATION FOR SITES USED WITH WIND ###
#################################################

for (ii in sites_used){

  CORR_DATA_TI = array(dim = c(96,73))
  CORR_DATA_TI_P = array(dim = c(96,73))
  CORR_DATA_PI = array(dim = c(96,73))
  CORR_DATA_PI_P = array(dim = c(96,73))

  lon_raw <- CAVES$site_info$longitude[ii]
  if(lon_raw<0){
    lon_raw = lon_raw+360
  }

  lon_cave = ceiling(lon_raw/360*96)
  lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
  
  print(paste0("Start with CAVE ", ii))
  print(" --> staring Correlation calculation")

  for (lon in 1:96){
    print(paste0("  lon: ", lon))
    for (lat in 1:73){

      if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,]))){
        COR_TI = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon_cave, lat_cave,],
                          SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,      lat,], na.rm = TRUE)
        COR_PI = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon_cave, lat_cave,],
                          SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,      lat,], na.rm = TRUE)

        CORR_DATA_TI[lon,lat] = COR_TI$estimate[[1]]
        CORR_DATA_TI_P[lon,lat] = COR_TI$p.value

        CORR_DATA_PI[lon,lat] = COR_PI$estimate[[1]]
        CORR_DATA_PI_P[lon,lat] = COR_PI$p.value
      }else{
        CORR_DATA_TI[lon,lat] = NA
        CORR_DATA_TI_P[lon,lat] = NA

        CORR_DATA_PI[lon,lat] = NA
        CORR_DATA_PI_P[lon,lat] = NA
      }



    }
  }
  
  print(" --> staring plotting for season: ")

  plotting_mask_ti <- CORR_DATA_TI_P
  plotting_mask_ti[plotting_mask_ti>0.05] = NA
  plotting_mask_ti[plotting_mask_ti<0.05] = 1

  plotting_mask_pi <- CORR_DATA_PI_P
  plotting_mask_pi[plotting_mask_pi>0.05] = NA
  plotting_mask_pi[plotting_mask_pi<0.05] = 1

  SITE_MAP = data.frame(
    x_lon = CAVES$site_info$longitude[ii],
    y_lat = CAVES$site_info$latitude[ii],
    cell_columns = c(1)
  )
  
  for (season in c("summer", "autumn", "winter", "spring")){
    
    print(paste0("  ", season))

    WIND_TI = data.frame(
      lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
      lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
      angle = as.vector(plotting_mask_ti[1:96, 1:72]*SIM_DATA_past1000_wind[[paste0("CAVE", ii)]][[paste0("WIND_MEAN_ANGLE_", season)]]),
      radius = as.vector(plotting_mask_ti[1:96, 1:72]*SIM_DATA_past1000_wind[[paste0("CAVE", ii)]][[paste0("WIND_MEAN_STRENGTH_", season)]])
    )
    
    WIND_PI = data.frame(
      lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
      lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
      angle = as.vector(plotting_mask_pi[1:96, 1:72]*SIM_DATA_past1000_wind[[paste0("CAVE", ii)]][[paste0("WIND_MEAN_ANGLE_", season)]]),
      radius = as.vector(plotting_mask_pi[1:96, 1:72]*SIM_DATA_past1000_wind[[paste0("CAVE", ii)]][[paste0("WIND_MEAN_STRENGTH_", season)]])
    )
    
    plot_ti <- STACYmap(gridlyr = rbind(CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                        CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                        ptlyr = SITE_MAP,
                        fldlyr = WIND_TI,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr T-I"),
                        graticules = TRUE,
                        centercolor = list(grid = "white", pt = "white")) +
      ggtitle(paste0("Corr. global T w/ local isot-comp incl wind, p<0.05, site ", ii)) +
      theme(plot.title = element_text(h = 0.5))
    
    plot_ti %>% ggsave(filename = paste(paste0('Map_Cor_site', str_pad(ii, 3, pad = "0"),'_TI_p01_', season), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots_Wind',
                       width = 15, height = 10, units = 'cm', dpi = 'print')
    
    plot_pi <- STACYmap(gridlyr = rbind(CORR_DATA_PI[49:96,1:73]*plotting_mask_pi[49:96,1:73],
                                        CORR_DATA_PI[1:48,1:73]*plotting_mask_pi[1:48,1:73]),
                        ptlyr = SITE_MAP,
                        fldlyr = WIND_PI,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr T-I"),
                        graticules = TRUE,
                        centercolor = list(grid = "white", pt = "white")) +
      ggtitle(paste0("Corr. global P w/ local isot-comp incl wind, p<0.05, site ", ii)) +
      theme(plot.title = element_text(h = 0.5))
    
    plot_pi %>% ggsave(filename = paste(paste0('Map_Cor_site', str_pad(ii, 3, pad = "0"),'_PI_p01_', season), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots_Wind',
                       width = 15, height = 10, units = 'cm', dpi = 'print')
  }

}

