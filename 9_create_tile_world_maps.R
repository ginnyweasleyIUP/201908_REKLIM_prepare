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

# Here I will try to generate world maps, that show
#   * mean temp
#   * mean prec
#   * mean isotope composition of rainfall
#   * PET
#   * correlation of temp with isotope
#   * correlation of prec with isotope

setwd("/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/")
basedir<-"/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/"

# 0) Data_Structure:

SIM_DATA_past1000 <- list(
  GLOBAL_STUFF = list(
    GLOBAL_lon_raw = "array",
    GLOBAL_lon_corrected = "array",
    GLOBAL_lat_raw = "array",
    GLOBAL_ls_mask = "array"
  ),
  GLOBAL_DATA_RAW = list(
    GLOBAL_DATA_TEMP = "data.frame",
    GLOBAL_DATA_PREC = "data.frame",
    GLOBAL_DATA_ISOT = "data.frame"
  ),
  GLOBAL_DATA_YEARLY = list(
    GLOBAL_DATA_TEMP_YEARLY = array(dim = c(96,73,1150)),
    GLOBAL_DATA_PREC_YEARLY = array(dim = c(96,73,1150)),
    GLOBAL_DATA_ISOT_YEARLY = array(dim = c(96,73,1150))
  ),
  GLOBAL_DATA_MEAN = list(
    GLOBAL_DATA_TEMP_MEAN = array(dim = c(96,73)),
    GLOBAL_DATA_PREC_MEAN = array(dim = c(96,73)),
    GLOBAL_DATA_ISOT_MEAN = array(dim = c(96,73))
  )
)

# 1) Load data
#system(paste0("cdo copy ",basedir,"full_sim_data/xnapa_surface_temperature.nc ",basedir, "temp.nc"))
#system(paste0("cdo sellonlatbox,-180,180,-90,90 ", basedir, "temp.nc ", basedir, "temp_rotated.nc"))

temp <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_surface_temperature.nc")
SIM_DATA_past1000$GLOBAL_DATA_RAW$GLOBAL_DATA_TEMP <- as.array(ncdf4::ncvar_get(temp))
SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw <- as.array(temp$dim$longitude$vals)
SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lat_raw <- as.array(temp$dim$latitude$vals)
ncdf4::nc_close(temp)
remove(temp)

SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_corrected <- as.array(c(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw[SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw<=180],
                                                                  SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw[SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw>180]-360))

prec <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_precipitation.nc")
SIM_DATA_past1000$GLOBAL_DATA_RAW$GLOBAL_DATA_PREC <- as.array(ncdf4::ncvar_get(prec))
ncdf4::nc_close(prec)
remove(prec)

isot <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_isotopes.nc")
SIM_DATA_past1000$GLOBAL_DATA_RAW$GLOBAL_DATA_ISOT <- as.array(ncdf4::ncvar_get(isot, isot$var[[3]]))
ncdf4::nc_close(isot)
remove(isot)

ls_mask <- ncdf4::nc_open("landseamask_preindustrial_hadcm3.nc")
SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_ls_mask <- as.array(ncdf4::ncvar_get(ls_mask))
ncdf4::nc_close(ls_mask)
remove(ls_mask)

# 2) Create yearly data

#mean(x, na.rm=TRUE) --> ignores NAs

for (lon in 1:96){ #--> nachgeschaute Dimensionen...!
  for (lat in 1:73){
    for (ii in 1:1150){
      pos_start = 12*(jj-1)+1
      pos_stop  = 12*(jj-1)+12
      
      SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,ii] = mean(SIM_DATA_past1000$GLOBAL_DATA_RAW$GLOBAL_DATA_TEMP[lon,lat,pos_start:pos_stop], na.rm = TRUE)
      SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,ii] = mean(SIM_DATA_past1000$GLOBAL_DATA_RAW$GLOBAL_DATA_PREC[lon,lat,pos_start:pos_stop], na.rm = TRUE)
      SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,ii] = mean(SIM_DATA_past1000$GLOBAL_DATA_RAW$GLOBAL_DATA_ISOT[lon,lat,pos_start:pos_stop], na.rm = TRUE)
      remove(pos_start, pos_stop)
    }
    SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN[lon,lat] = mean(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat, 1:1150], na.rm = TRUE)
    SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_PREC_MEAN[lon,lat] = mean(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat, 1:1150], na.rm = TRUE)
    SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_ISOT_MEAN[lon,lat] = mean(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat, 1:1150], na.rm = TRUE)

  }
}
remove(ii,jj)

layer <- ogrListLayers("/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_coastline.shp")
coastline_map <- readOGR(dsn ="/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_coastline.shp", verbose = FALSE, layer= layer)
land_map <- readOGR(dsn ="/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_land.shp", verbose = FALSE)
ocean_map <- readOGR(dsn ="/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_ocean.shp", verbose = FALSE) %>% fortify()
remove(layer)

plotting_ls_mask <- SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_ls_mask
plotting_ls_mask[plotting_ls_mask == 0] <- NA
plotting_temp_mask <- SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN
plotting_temp_mask[plotting_temp_mask> -40] <- 1
plotting_temp_mask[plotting_temp_mask< -40] <- NA

GLOBAL_MEAN_TEMP_MAP <- data.frame(
  value = array(SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN*plotting_ls_mask*plotting_temp_mask),
  x_lon = rep(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_corrected, length(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lat_raw)),
  y_lat = rep(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lat_raw, each = length(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_corrected))
)

plot <- STACYmap()


plot <- ggplot() +
  geom_tile(data = subset(GLOBAL_MEAN_TEMP_MAP, !is.na(value)), aes(x = x_lon, y = y_lat, fill = value), size = 1) +
  scale_fill_gradient2(midpoint = 0.5, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = TeX("$T_{mean}$")) +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.1, color = "black", fill = NA, alpha = 0.5) +
  ggtitle(TeX("Yearly local $T_{mean}$ for past 1000y"))+
  coord_fixed(1.3) + 
  xlab("")+ ylab("")+
  ylim(-60,80) +
  xlim(-170, 180) +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))


plot %>% ggsave(filename = paste('Map_blabla', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')


