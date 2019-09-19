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

plotting_ls_mask <- SIM_DATA_past1000$GLOBAL_STUFF$GLOBAL_ls_mask
plotting_ls_mask[plotting_ls_mask == 0] <- NA
plotting_temp_mask <- SIM_DATA_past1000$GLOBAL_DATA_MEAN$GLOBAL_DATA_TEMP_MEAN
plotting_temp_mask[plotting_temp_mask> -38] <- 1
plotting_temp_mask[plotting_temp_mask< -38] <- NA

## Create Seasonal Data?!?

SIM_DATA_past1000_seasonal <- list(
  WINTER_world = list(
    temp = array(dim = c(96,73,1150)),
    prec = array(dim = c(96,73,1150)),
    isot = array(dim = c(96,73,1150))
  ),
  SPRING_world = list(
    temp = array(dim = c(96,73,1150)),
    prec = array(dim = c(96,73,1150)),
    isot = array(dim = c(96,73,1150))
  ),
  SUMMER_world = list(
    temp = array(dim = c(96,73,1150)),
    prec = array(dim = c(96,73,1150)),
    isot = array(dim = c(96,73,1150))
  ),
  AUTUMN_world = list(
    temp = array(dim = c(96,73,1150)),
    prec = array(dim = c(96,73,1150)),
    isot = array(dim = c(96,73,1150))
  )
)

winter_mask = c(1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA)
spring_mask = c(NA,NA,NA,1,1,1,NA,NA,NA,NA,NA,NA)
summer_mask = c(NA,NA,NA,NA,NA,NA,1,1,1,NA,NA,NA)
autumn_mask = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1)

for (lon in 1:96){ #--> nachgeschaute Dimensionen...!
  for (lat in 1:73){
    
    print(paste0(lon, " ", lat))
    
    for (ii in 1:1150){
      
      
      
      pos_start = 12*(ii-1)+1
      pos_stop  = 12*(ii-1)+12
      
      SIM_DATA_past1000_seasonal$WINTER_world$temp[lon,lat,ii] = mean(temp_raw[lon,lat,pos_start:pos_stop]*winter_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$WINTER_world$prec[lon,lat,ii] = mean(prec_raw[lon,lat,pos_start:pos_stop]*winter_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$WINTER_world$isot[lon,lat,ii] = mean(isot_raw[lon,lat,pos_start:pos_stop]*winter_mask, na.rm = TRUE)
      
      SIM_DATA_past1000_seasonal$SPRING_world$temp[lon,lat,ii] = mean(temp_raw[lon,lat,pos_start:pos_stop]*spring_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$SPRING_world$prec[lon,lat,ii] = mean(prec_raw[lon,lat,pos_start:pos_stop]*spring_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$SPRING_world$isot[lon,lat,ii] = mean(isot_raw[lon,lat,pos_start:pos_stop]*spring_mask, na.rm = TRUE)
      
      SIM_DATA_past1000_seasonal$SUMMER_world$temp[lon,lat,ii] = mean(temp_raw[lon,lat,pos_start:pos_stop]*summer_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$SUMMER_world$prec[lon,lat,ii] = mean(prec_raw[lon,lat,pos_start:pos_stop]*summer_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$SUMMER_world$isot[lon,lat,ii] = mean(isot_raw[lon,lat,pos_start:pos_stop]*summer_mask, na.rm = TRUE)
      
      SIM_DATA_past1000_seasonal$AUTUMN_world$temp[lon,lat,ii] = mean(temp_raw[lon,lat,pos_start:pos_stop]*autumn_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$AUTUMN_world$prec[lon,lat,ii] = mean(prec_raw[lon,lat,pos_start:pos_stop]*autumn_mask, na.rm = TRUE)
      SIM_DATA_past1000_seasonal$AUTUMN_world$isot[lon,lat,ii] = mean(isot_raw[lon,lat,pos_start:pos_stop]*autumn_mask, na.rm = TRUE)
      remove(pos_start, pos_stop)
    }
  }
}

remove(winter_mask, summer_mask, autumn_mask, spring_mask)


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

# p muss kleiner als 0.05 damit significance passt!!!

plotting_mask_tp <- SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC_P
plotting_mask_tp[plotting_mask_tp>0.1] = NA
plotting_mask_tp[plotting_mask_tp<0.1] = 1

plotting_mask_ti <- SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT_P
plotting_mask_ti[plotting_mask_ti>0.1] = NA
plotting_mask_ti[plotting_mask_ti<0.1] = 1

plotting_mask_pi <- SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT_P
plotting_mask_pi[plotting_mask_pi>0.1] = NA
plotting_mask_pi[plotting_mask_pi<0.1] = 1

SITES_USED_MAP = data.frame(
  x_lon = CAVES$site_info$longitude[sites_used],
  y_lat = CAVES$site_info$latitude[sites_used],
  cell_columns = numeric(length = length(sites_used))
)


plot_cor_tp <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_tp[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_PREC[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_tp[1:48,1:73]),
                        ptlyr = SITES_USED_MAP,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr T-P"), 
                        graticules = TRUE,
                        centercolor = list(grid = "white", pt = "white")) +
  ggtitle("Local Temp-Prec Correlation, p<0.1") +
  theme(plot.title = element_text(h = 0.5))
plot_cor_ti <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_TEMP_ISOT[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                        ptlyr = SITES_USED_MAP,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr T-I"),
                        centercolor = list(grid = "white", pt = "white")) +
  ggtitle("Local Temp-Isotopic Composition Correlation, p<0.1") +
  theme(plot.title = element_text(h = 0.5))
plot_cor_pi <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_pi[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_CORRELATION$CORR_PREC_ISOT[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_pi[1:48,1:73]),
                        ptlyr = SITES_USED_MAP,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr P-I"),
                        centercolor = list(grid = "white", pt = "white")) +
  ggtitle("Local Prec-Isotopic Composition Correlation, p<0.1") +
  theme(plot.title = element_text(h = 0.5))

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

#Regressionsanalyse macht eigentlich erst Sinn bei längeren Zeiträumen
#bei einem Zeitraum von nur 1000 Jahren ändert sich nicht genug, als dass es eine 

for (lon in 1:96){ #--> nachgeschaute Dimensionen...!
  for (lat in 1:73){
    if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])) & 
       !any(is.infinite(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])) &
       !any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,])) & 
       !any(is.infinite(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,]))){
      REG1 = lm(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,] ~ SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_alpha[lon,lat] = REG1$coefficients[[2]]
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_beta[lon,lat] = REG1$coefficients[[1]]
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_rsquared[lon,lat] = summary(REG1)$r.squared
    }else{
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_alpha[lon,lat] = NA
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_beta[lon,lat] = NA
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_rsquared[lon,lat] = NA
    }
    
    if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,])) & 
       !any(is.infinite(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,])) &
       !any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])) & 
       !any(is.infinite(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,]))){
      REG2 = lm(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,] ~ SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_TEMP_YEARLY[lon,lat,])
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_alpha[lon,lat] = REG2$coefficients[[2]]
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_beta[lon,lat] = REG2$coefficients[[1]]
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_rsquared[lon,lat]= summary(REG2)$r.squared
    }else{
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_alpha[lon,lat] = NA
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_beta[lon,lat] = NA
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_rsquared[lon,lat] = NA
    }
    
    
    if(!any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,])) & 
       !any(is.infinite(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,])) &
       !any(is.na(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,])) &
       !any(is.infinite(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,]))){
      REG3 = lm(SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_ISOT_YEARLY[lon,lat,] ~ SIM_DATA_past1000$GLOBAL_DATA_YEARLY$GLOBAL_DATA_PREC_YEARLY[lon,lat,])
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_alpha[lon,lat] = REG3$coefficients[[2]]
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_beta[lon,lat] = REG3$coefficients[[1]]
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_rsquared[lon,lat] = summary(REG3)$r.squared
    }else{
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_alpha[lon,lat] = NA
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_beta[lon,lat] = NA
      SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_rsquared[lon,lat] = NA
    }
  }
}


#################################################
##PLOT REGRESSIONS ##############################
#################################################

plotting_mask_tp <- SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_rsquared
plotting_mask_tp[plotting_mask_tp<0.5] = NA
plotting_mask_tp[plotting_mask_tp>0.5] = 1

plotting_mask_ti <- SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_rsquared
plotting_mask_ti[plotting_mask_ti<0.5] = NA
plotting_mask_ti[plotting_mask_ti>0.5] = 1

plotting_mask_pi <- SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_rsquared
plotting_mask_pi[plotting_mask_pi<0.5] = NA
plotting_mask_pi[plotting_mask_pi>0.5] = 1

SITES_USED_MAP = data.frame(
  x_lon = CAVES$site_info$longitude[sites_used],
  y_lat = CAVES$site_info$latitude[sites_used],
  cell_columns = numeric(length = length(sites_used))
)


plot_reg_tp <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_alpha[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_tp[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_PREC_alpha[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_tp[1:48,1:73]),
                        ptlyr = SITES_USED_MAP,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr T-P"), 
                        graticules = TRUE,
                        centercolor = list(grid = "white", pt = "white")) +
  ggtitle("Local Temp-Prec Regression, r2>0.5") +
  theme(plot.title = element_text(h = 0.5))
plot_reg_ti <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_alpha[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_REGRESSION$REG_TEMP_ISOT_alpha[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                        ptlyr = SITES_USED_MAP,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr T-I"),
                        centercolor = list(grid = "white", pt = "white")) +
  ggtitle("Local Temp-Isotopic Composition Regression, r2>0.5") +
  theme(plot.title = element_text(h = 0.5))
plot_reg_pi <- STACYmap(gridlyr = rbind(SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_alpha[49:96,1:73]*plotting_ls_mask[49:96,1:73]*plotting_mask_pi[49:96,1:73],
                                        SIM_DATA_past1000$GLOBAL_REGRESSION$REG_PREC_ISOT_alpha[1:48,1:73]*plotting_ls_mask[1:48,1:73]*plotting_mask_pi[1:48,1:73]),
                        ptlyr = SITES_USED_MAP,
                        zoom = c(-180, -60, 180, 73),
                        legend_names = list(grid = "Corr P-I"),
                        centercolor = list(grid = "white", pt = "white")) +
  ggtitle("Local Prec-Isotopic Composition Regression, r2>0.5") +
  theme(plot.title = element_text(h = 0.5))

plot_reg_tp %>% ggsave(filename = paste('Map_Reg_Temp_Prec_p01', 'png', sep = '.'), plot = ., path = 'Plots', 
                       width = 15, height = 10, units = 'cm', dpi = 'print')
plot_reg_ti %>% ggsave(filename = paste('Map_Reg_Temp_Isot_p01', 'png', sep = '.'), plot = ., path = 'Plots', 
                       width = 15, height = 10, units = 'cm', dpi = 'print')
plot_reg_pi %>% ggsave(filename = paste('Map_Reg_Prec_Isot_p01', 'png', sep = '.'), plot = ., path = 'Plots', 
                       width = 15, height = 10, units = 'cm', dpi = 'print')

remove(plotting_mask_pi, plotting_mask_ti, plotting_mask_tp)
remove(plot_reg_pi, plot_reg_ti, plot_reg_tp)

#################################################
##POINT CORRELATION FOR SITES USED ##############
#################################################

for (ii in sites_used){
  
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
  
  plot_ti %>% ggsave(filename = paste(paste0('Map_Cor_TI_p01_', 'site', ii), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots', 
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
  
  plot_pi %>% ggsave(filename = paste(paste0('Map_Cor_PI_p01_', 'site', ii), 'png', sep = '.'), plot = ., path = 'Plots/Site_Corr_Plots', 
                     width = 15, height = 10, units = 'cm', dpi = 'print')
}

