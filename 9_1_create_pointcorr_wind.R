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


#################################################

sites_used_short = c(27,104)

#################################################
## Read in Wind Data ############################
#################################################

SIM_DATA_past1000_wind_raw <- list(
  RAW = list(
    westerly_raw = "array",
    southerly_raw = "array",
    prec_raw = "array"
  )
)

#################################################
## FILL DATA ####################################
#################################################

prec <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_precipitation.nc")
SIM_DATA_past1000_wind_raw$RAW$prec_raw <- as.array(ncdf4::ncvar_get(prec)[,1:72,1:13790])
ncdf4::nc_close(prec)
remove(prec)

wind_u <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_westerly_u.nc")
SIM_DATA_past1000_wind_raw$RAW$westerly_raw <- as.array(ncdf4::ncvar_get(wind_u))
ncdf4::nc_close(wind_u)
remove(wind_u)

wind_v <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_southerly_v.nc")
SIM_DATA_past1000_wind_raw$RAW$southerly_raw <- as.array(ncdf4::ncvar_get(wind_v))
ncdf4::nc_close(wind_v)
remove(wind_v)

#################################################
## Create List for all caves for pw-average #####
#################################################

#Example Lists:
#mylist <- vector(mode = "list")
#mylist[["CAVE1"]] = c(1,2,3,4,5)

SIM_DATA_past1000_wind <- vector(mode = "list")

for (ii in sites_used_short){
  name = paste0("CAVE", ii)
  SIM_DATA_past1000_wind[[name]] <- list(
    WIND_MEAN_STRENGTH = array(dim = c(96,72)),
    WIND_MEAN_STRENGTH_PREC = array(dim = c(96,72)),
    WIND_MEAN_ANGLE = array(dim = c(96,72)),
    WIND_MEAN_ANGLE_PREC = array(dim = c(96,72))
  )
  
  lon_raw <- CAVES$site_info$longitude[ii]
  if(lon_raw<0){
    lon_raw = lon_raw+360
  }
  
  lon_cave = ceiling(lon_raw/360*96)
  lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
  
  for (lon in 1:96){
    print(lon)
    for (lat in 1:72){
      
      #print(paste0(ii, " lon: ", lon, ", lat: ", lat))
      
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH[lon,lat] = mean(sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]^2 + 
                                                                               SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,]^2))
      SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE[lon,lat] = mean(atan2(SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,], SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]), na.rm = TRUE)
      
      #precipitation weighted --> PREC OF THE CAVE!!!
      #lon_cave = ceiling(lon_raw/360*96)
      #lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
      #SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_PREC[lon,lat] = sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,] * 
      #                                                                        sqrt(SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]^2 + 
      #                                                                               SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,]^2), na.rm = TRUE) / sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,], na.rm = TRUE)
      #SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_PREC[lon,lat] = sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,]*
      #                                                                     atan(- SIM_DATA_past1000_wind_raw$RAW$southerly_raw[lon,lat,]/SIM_DATA_past1000_wind_raw$RAW$westerly_raw[lon,lat,]), na.rm = TRUE) / sum(SIM_DATA_past1000_wind_raw$RAW$prec_raw[lon_cave,lat_cave,], na.rm = TRUE)
      
    }
  }
}

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
# plot <- ggplot() +  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +coord_fixed(1.3) + 
#   geom_tile(data = Temp_map, aes(x = x_lon, y = y_lat, fill = value), size = 50) + #(Non significant)
#   scale_fill_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Temp") +
#   geom_spoke(data = Wind_map_27, aes(x=x_lon, y=y_lat, angle = angle, radius = radius), arrow = arrow(length = unit(.04, 'inches'))) +
#   geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5)
# 
# plot

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
## SEASONAL MEANS ###############################
#################################################

#Simulation starts at 1.June -> 1:3 = JJA

for (ii in sites_used_short){
  
  msk_summer = rep(c(1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA), 1149) #JJA
  msk_autumn = rep(c(NA,NA,NA,1,1,1,NA,NA,NA,NA,NA,NA), 1149) #SON
  msk_winter = rep(c(NA,NA,NA,NA,NA,1,1,1,NA,NA,NA,NA), 1149) #DJF
  msk_spring = rep(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1), 1149) #MAM
  
  name = paste0("CAVE", ii)
  
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_summer <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_autumn <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_winter <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_STRENGTH_spring <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_summer <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_autumn <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_winter <- array(dim = c(96,72))
  SIM_DATA_past1000_wind[[name]]$WIND_MEAN_ANGLE_spring <- array(dim = c(96,72))
  
  for (lon in 1:96){
    print(lon)
    for (lat in 1:72){
      
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

remove(msk_summer, msk_autumn, msk_winter, msk_spring)

summer_wind_27 <- data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind$CAVE27$WIND_MEAN_ANGLE_spring),
  radius = as.vector(SIM_DATA_past1000_wind$CAVE27$WIND_MEAN_STRENGTH_spring)
)

plot <- ggplot() +  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +coord_fixed(1.3) +
  geom_tile(data = Temp_map, aes(x = x_lon, y = y_lat, fill = value), size = 50) + #(Non significant)
  scale_fill_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Temp") +
  geom_spoke(data = summer_wind_27, aes(x=lon, y=lat, angle = angle, radius = radius), size = .2, arrow = arrow(length = unit(.03, 'inches')), alpha = 0.5) +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5)

plot <- STACYmap(gridlyr = rbing(SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_PREC_MEAN[49:96,1:73], SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_PREC_MEAN[1:48,1:73]),
                 ptlyr = SITE_MAP,
                 fldlyr = summer_wind_27,
                 zoom)

# plot_ti <- STACYmap(gridlyr = rbind(CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
#                     ptlyr = SITE_MAP,
#                     fldlyr = rbind()
#                     zoom = c(-180, -60, 180, 73),
#                     legend_names = list(grid = "Corr T-I"),
#                     graticules = TRUE,
#                     centercolor = list(grid = "white", pt = "white")) + 
#   ggtitle(paste0("Corr. of global temp w/ local isot-comp, p<0.1, site ", ii)) +
#   theme(plot.title = element_text(h = 0.5))

plot


#########################
## TO DO AM DONNERSTAG

# * DJF, MAM, JJA, SON - Mittel anschauen und sehen ob das anders wird
# * vergleichen warum alles immer nach Osten geht, weil ja auf der westerly plot Karte auch viel nach Westen geht?!?
# * dann teleconnection-wise alle significanten Punkte mit einbeziehen
# * STACYmap aus Github --> branch einarbeiten?!?
# * in STACYmaps einbauen -> radius als Option mit übergeben



#--> Create list with assigned names for Cave sites used, where matrix of precipitation weighted average wind size is saved
  
####
# What needs to happen to get the wind strength right:
#   * for each cave, the yearly average has to be calculated seperately, because the wind direction is only important if
#     in the same month there is precipitation at that site. We need a monthly local prec weighted average of wind strength and direction 
#   * A snapshot in time has to be mapped onto the correlation map --> maybe a yearly average is not needed, but instead a total 1150y average?

####
# What needs to happen to decide which wind strength grid box is included?
#   * all arrays that point towards either the grid box or the neighboring grid boxes are accepted --> create mask that puts all others to NA
#   * Try to plot after that? 
#   * all arrays are divided by radius and then only arrays with a specific "strength" are accepted --> create mask
#   * plot original strength and that divided by the radius


#################################################
##POINT CORRELATION FOR SITES USED ##############
#################################################

# for (ii in sites_used_short){
#   
#   CORR_DATA_TI = array(dim = c(96,73))
#   CORR_DATA_TI_P = array(dim = c(96,73))
#   CORR_DATA_PI = array(dim = c(96,73))
#   CORR_DATA_PI_P = array(dim = c(96,73))
#   
#   lon_raw <- CAVES$site_info$longitude[ii]
#   if(lon_raw<0){
#     lon_raw = lon_raw+360
#   }
#   
#   lon_cave = ceiling(lon_raw/360*96)
#   lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
#   
#   for (lon in 1:96){
#     for (lat in 1:73){
#       
#       if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,]))){
#         COR_TI = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon_cave, lat_cave,], 
#                           SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,      lat,], na.rm = TRUE)
#         COR_PI = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon_cave, lat_cave,], 
#                           SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,      lat,], na.rm = TRUE)
#         
#         CORR_DATA_TI[lon,lat] = COR_TI$estimate[[1]]
#         CORR_DATA_TI_P[lon,lat] = COR_TI$p.value
#         
#         CORR_DATA_PI[lon,lat] = COR_PI$estimate[[1]]
#         CORR_DATA_PI_P[lon,lat] = COR_PI$p.value
#       }else{
#         CORR_DATA_TI[lon,lat] = NA
#         CORR_DATA_TI_P[lon,lat] = NA
#         
#         CORR_DATA_PI[lon,lat] = NA
#         CORR_DATA_PI_P[lon,lat] = NA
#       }
#       
#       
#       
#     }
#   }
#   
#   plotting_mask_ti <- CORR_DATA_TI_P
#   plotting_mask_ti[plotting_mask_ti>0.1] = NA
#   plotting_mask_ti[plotting_mask_ti<0.1] = 1
#   
#   plotting_mask_pi <- CORR_DATA_PI_P
#   plotting_mask_pi[plotting_mask_ti>0.1] = NA
#   plotting_mask_pi[plotting_mask_ti<0.1] = 1
#   
#   SITE_MAP = data.frame(
#     x_lon = CAVES$site_info$longitude[ii],
#     y_lat = CAVES$site_info$latitude[ii],
#     cell_columns = c(1)
#   )
#   
#   plot_ti <- STACYmap(gridlyr = rbind(CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
#                                       CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
#                       ptlyr = SITE_MAP,
#                       zoom = c(-180, -60, 180, 73),
#                       legend_names = list(grid = "Corr T-I"), 
#                       graticules = TRUE,
#                       centercolor = list(grid = "white", pt = "white")) + 
#     ggtitle(paste0("Corr. of global temp w/ local isot-comp, p<0.1, site ", ii)) +
#     theme(plot.title = element_text(h = 0.5))
#   
#   plot_ti %>% ggsave(filename = paste(paste0('Map_Cor_TI_p01_', 'site', ii), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots_Wind', 
#                      width = 15, height = 10, units = 'cm', dpi = 'print')
#   
#   plot_pi <- STACYmap(gridlyr = rbind(CORR_DATA_PI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
#                                       CORR_DATA_PI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
#                       ptlyr = SITE_MAP,
#                       zoom = c(-180, -60, 180, 73),
#                       legend_names = list(grid = "Corr T-I"), 
#                       graticules = TRUE,
#                       centercolor = list(grid = "white", pt = "white")) + 
#     ggtitle(paste0("Corr. of global prec w/ local isot-comp, p<0.1, site ", ii)) +
#     theme(plot.title = element_text(h = 0.5))
#   
#   plot_pi %>% ggsave(filename = paste(paste0('Map_Cor_PI_p01_', 'site', ii), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots_Wind', 
#                      width = 15, height = 10, units = 'cm', dpi = 'print')
# }
# 
