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
SIM_DATA_past1000_wind_raw$RAW$prec_raw <- as.array(ncdf4::ncvar_get(prec))
ncdf4::nc_close(prec)
remove(prec)

wind_u <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_westerly_u.nc")
SIM_DATA_past1000_wind_raw$RAW$westerly_raw <- as.array(ncdf4::ncvar_get(wind_u))
ncdf4::nc_close(wind_u)
remove(wind_u)

wind_v <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_westerly_u.nc")
SIM_DATA_past1000_wind_raw$RAW$southerly_raw <- as.array(ncdf4::ncvar_get(wind_v))
ncdf4::nc_close(wind_v)
remove(wind_v)

#################################################
## Create List for all caves for pw-average #####
#################################################

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

for (ii in sites_used_short){
  
  CORR_DATA_TI = array(dim = c(96,73))
  CORR_DATA_TI_P = array(dim = c(96,73))
  CORR_DATA_PI = array(dim = c(96,73))
  CORR_DATA_PI_P = array(dim = c(96,73))
  
  lon_raw = CAVES$site_info$longitude[ii]
  if(lon_raw<0){
    lon_raw = lon_raw+360
  }
  
  lon_cave = ceiling(lon_raw/360*96)
  lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
  
  for (lon in 1:96){
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
  
  plotting_mask_ti <- CORR_DATA_TI_P
  plotting_mask_ti[plotting_mask_ti>0.1] = NA
  plotting_mask_ti[plotting_mask_ti<0.1] = 1
  
  plotting_mask_pi <- CORR_DATA_PI_P
  plotting_mask_pi[plotting_mask_ti>0.1] = NA
  plotting_mask_pi[plotting_mask_ti<0.1] = 1
  
  SITE_MAP = data.frame(
    x_lon = CAVES$site_info$longitude[ii],
    y_lat = CAVES$site_info$latitude[ii],
    cell_columns = c(1)
  )
  
  plot_ti <- STACYmap(gridlyr = rbind(CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                      CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                      ptlyr = SITE_MAP,
                      zoom = c(-180, -60, 180, 73),
                      legend_names = list(grid = "Corr T-I"), 
                      graticules = TRUE,
                      centercolor = list(grid = "white", pt = "white")) + 
    ggtitle(paste0("Corr. of global temp w/ local isot-comp, p<0.1, site ", ii)) +
    theme(plot.title = element_text(h = 0.5))
  
  plot_ti %>% ggsave(filename = paste(paste0('Map_Cor_TI_p01_', 'site', ii), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots_Wind', 
                     width = 15, height = 10, units = 'cm', dpi = 'print')
  
  plot_pi <- STACYmap(gridlyr = rbind(CORR_DATA_PI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                      CORR_DATA_PI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                      ptlyr = SITE_MAP,
                      zoom = c(-180, -60, 180, 73),
                      legend_names = list(grid = "Corr T-I"), 
                      graticules = TRUE,
                      centercolor = list(grid = "white", pt = "white")) + 
    ggtitle(paste0("Corr. of global prec w/ local isot-comp, p<0.1, site ", ii)) +
    theme(plot.title = element_text(h = 0.5))
  
  plot_pi %>% ggsave(filename = paste(paste0('Map_Cor_PI_p01_', 'site', ii), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots_Wind', 
                     width = 15, height = 10, units = 'cm', dpi = 'print')
}

