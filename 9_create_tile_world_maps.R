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
#basedir<-"/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/"

# 0) Data_Structure:

SIM_DATA_past1000 <- list(
  GLOBAL_STUFF = list(
    GLOBAL_lon_raw = "array",
    GLOBAL_lon_corrected = "array",
    GLOBAL_lat_raw = "array",
    GLOBAL_ls_mask = "array"
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
  ),
  GLOBAL_CORRELATION = list(
    CORR_TEMP_PREC = array(dim = c(96,73)),
    CORR_TEMP_PREC_P = array(dim = c(96,73)),
    CORR_TEMP_ISOT = array(dim = c(96,73)),
    CORR_TEMP_ISOT_P = array(dim = c(96,73)),
    CORR_PREC_ISOT = array(dim = c(96,73)),
    CORR_PREC_ISOT_P = array(dim = c(96,73))
  ),
  GLOBAL_REGRESSION = list(
    REG_TEMP_PREC_alpha = array(dim = c(96,73)),
    REG_TEMP_PREC_beta = array(dim = c(96,73)),
    REG_TEMP_PREC_rsquared = array(dim = c(96,73)),
    REG_TEMP_ISOT_alpha = array(dim = c(96,73)),
    REG_TEMP_ISOT_beta = array(dim = c(96,73)),
    REG_TEMP_ISOT_rsquared = array(dim = c(96,73)),
    REG_PREC_ISOT_alpha = array(dim = c(96,73)),
    REG_PREC_ISOT_beta = array(dim = c(96,73)),
    REG_PREC_ISOT_rsquared = array(dim = c(96,73))
  )
)

# 1) Load data
#system(paste0("cdo copy ",basedir,"full_sim_data/xnapa_surface_temperature.nc ",basedir, "temp.nc"))
#system(paste0("cdo sellonlatbox,-180,180,-90,90 ", basedir, "temp.nc ", basedir, "temp_rotated.nc"))

temp <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_surface_temperature.nc")
temp_raw <- as.array(ncdf4::ncvar_get(temp))
SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw <- as.array(temp$dim$longitude$vals)
SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lat_raw <- as.array(temp$dim$latitude$vals)
ncdf4::nc_close(temp)
remove(temp)

SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_corrected <- as.array(c(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw[SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw<=180],
                                                                  SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw[SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_raw>180]-360))

prec <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_precipitation.nc")
prec_raw <- as.array(ncdf4::ncvar_get(prec))
ncdf4::nc_close(prec)
remove(prec)

isot <- ncdf4::nc_open("/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/05_HadCM3/xnapa/xnapa_isotopes.nc")
isot_raw <- as.array(ncdf4::ncvar_get(isot, isot$var[[3]]))
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
      pos_start = 12*(ii-1)+1
      pos_stop  = 12*(ii-1)+12
      
      SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,ii] = mean(temp_raw[lon,lat,pos_start:pos_stop], na.rm = TRUE)
      SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,ii] = mean(prec_raw[lon,lat,pos_start:pos_stop], na.rm = TRUE)
      SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,ii] = mean(isot_raw[lon,lat,pos_start:pos_stop], na.rm = TRUE)
      remove(pos_start, pos_stop)
    }
    SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN[lon,lat] = mean(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat, 1:1150], na.rm = TRUE)
    SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_PREC_MEAN[lon,lat] = mean(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat, 1:1150], na.rm = TRUE)
    SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_ISOT_MEAN[lon,lat] = mean(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat, 1:1150], na.rm = TRUE)

  }
}
remove(lon,lat,ii)
remove(temp_raw, prec_raw, isot_raw)

layer <- ogrListLayers("/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_coastline.shp")
coastline_map <- readOGR(dsn ="/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_coastline.shp", verbose = FALSE, layer= layer)
land_map <- readOGR(dsn ="/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_land.shp", verbose = FALSE)
ocean_map <- readOGR(dsn ="/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/naturalearth_10m_physical/ne_10m_ocean.shp", verbose = FALSE) %>% fortify()
remove(layer)

plotting_ls_mask <- SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_ls_mask
plotting_ls_mask[plotting_ls_mask == 0] <- NA
plotting_temp_mask <- SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN
plotting_temp_mask[plotting_temp_mask> -38] <- 1
plotting_temp_mask[plotting_temp_mask< -38] <- NA

GLOBAL_MEAN_TEMP_MAP <- data.frame(
  x_lon = rep(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_corrected, length(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lat_raw)),
  y_lat = rep(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lat_raw, each = length(SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_lon_corrected)),
  value = array(SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN*plotting_ls_mask*plotting_temp_mask)
)

plot <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_temp_mask[49:96,1:73],SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_temp_mask[1:48,1:73]),
                 #projection = "+proj=robin +lon_0=120",
                 zoom = c(-180, -60, 180, 73),
                 legend_names = list(grid = TeX("$T_{mean}")))



# plot <- ggplot() +
#   geom_tile(data = subset(GLOBAL_MEAN_TEMP_MAP, !is.na(value)), aes(x = x_lon, y = y_lat, fill = value), size = 1) +
#   scale_fill_gradient2(midpoint = 0.5, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = TeX("$T_{mean}$")) +
#   geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.1, color = "black", fill = NA, alpha = 0.5) +
#   ggtitle(TeX("Yearly local $T_{mean}$ for past 1000y"))+
#   coord_fixed(1.3) + 
#   xlab("")+ ylab("")+
#   ylim(-60,80) +
#   xlim(-170, 180) +
#   theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
#         axis.title.x = element_text(size=11),
#         axis.title.y = element_text(size=11),
#         legend.text = element_text(size=11))
# 
# 
# plot %>% ggsave(filename = paste('Map_blabla', 'png', sep = '.'), plot = ., path = 'Plots', 
#                 width = 15, height = 10, units = 'cm', dpi = 'print')

#################################################
##CALCULATE CORRELATIONS ########################
#################################################

for (lon in 1:96){
  for (lat in 1:73){
    COR1 = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,], SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,], na.rm = TRUE)
    SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC[lon,lat]   = COR1$estimate[[1]]
    SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC_P[lon,lat] = COR1$p.value
    
    if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,]))){
      COR2 = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,], SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,], na.rm = TRUE)
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT[lon,lat]   = COR2$estimate[[1]]
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT_P[lon,lat] = COR2$p.value
    }else{
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT[lon,lat]   = NA
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT_P[lon,lat] = NA
    }
    
    
    if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,]))){
      COR3 = cor.test(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,], SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,], na.rm = TRUE)
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT[lon,lat]   = COR3$estimate[[1]]
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT_P[lon,lat] = COR3$p.value
    }else{
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT[lon,lat]   = NA
      SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT_P[lon,lat] = NA
    }
    
    
    
  }
}



#################################################
##PLOT CORRELATIONS #############################
#################################################

#TODO : Insert plotting_mask with p-values!!!
# AUsserdem finde raus, wierum der p-value richtig ist!!!

plotting_mask_tp <- SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC_P
plotting_mask_tp[plotting_mask_tp>0.1] = NA
plotting_mask_tp[plotting_mask_tp<0.1] = 1

plotting_mask_ti <- SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT_P
plotting_mask_ti[plotting_mask_tp<0.1] = NA
plotting_mask_ti[plotting_mask_tp>0.1] = 1

plotting_mask_pi <- SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT_P
plotting_mask_pi[plotting_mask_tp>0.1] = NA
plotting_mask_pi[plotting_mask_tp<0.1] = 1


plot_cor_tp <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_tp[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_tp[1:48,1:73]),
                 #projection = "+proj=robin +lon_0=120",
                 zoom = c(-180, -60, 180, 73),
                 legend_names = list(grid = TeX("Corr T-P")))
plot_cor_ti <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                 #projection = "+proj=robin +lon_0=120",
                 zoom = c(-180, -60, 180, 73),
                 legend_names = list(grid = TeX("Corr T-I")))
plot_cor_pi <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_pi[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_pi[1:48,1:73]),
                 #projection = "+proj=robin +lon_0=120",
                 zoom = c(-180, -60, 180, 73),
                 legend_names = list(grid = TeX("Corr P-I")))

plot_cor_tp %>% ggsave(filename = paste('Map_Cor_Temp_Prec_p01', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')
plot_cor_ti %>% ggsave(filename = paste('Map_Cor_Temp_Isot_p01', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')
plot_cor_pi %>% ggsave(filename = paste('Map_Cor_Prec_Isot_p01', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

remove(plotting_mask_pi, plotting_mask_ti, plotting_mask_tp)
remove(plot_cor_pi, plot_cor_ti, plot_cor_tp)

#################################################
##CALCULATE REGRESSIONS##########################
#################################################

for (lon in 1:96){ #--> nachgeschaute Dimensionen...!
  for (lat in 1:73){
    REG1 = lm(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,] ~ SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])
    REG2 = lm(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,] ~ SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])
    REG3 = lm(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,] ~ SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,])
    
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_alpha = REG1$coefficients[[2]]
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_beta = REG1$coefficients[[1]]
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_rsquared = summary(REG1)$r.squared
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_alpha = REG2$coefficients[[2]]
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_beta = REG2$coefficients[[1]]
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_rsquared = summary(REG2)$r.squared
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_alpha = REG3$coefficients[[2]]
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_beta = REG3$coefficients[[1]]
    SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_rsquared = summary(REG3)$r.squared
    
  }
}


#################################################
##PLOT REGRESSIONS ##############################
#################################################

