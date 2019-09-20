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

# SIM_DATA_past1000_wind_raw <- list(
#   RAW = list(
#     westerly_raw = "array",
#     southerly_raw = "array",
#     prec_raw = "array"
#   )
# )

#################################################
## YEARLY AND SEASONAL MEANS ####################
#################################################

SIM_DATA_past1000_wind <- vector(mode = "list")

#Simulation starts at 1.Dez -> 1:3 = DJF

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
  
  msk_winter = rep(c(1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA), 1149) #DJF
  msk_spring = rep(c(NA,NA,NA,1,1,1,NA,NA,NA,NA,NA,NA), 1149) #MAM
  msk_summer = rep(c(NA,NA,NA,NA,NA,1,1,1,NA,NA,NA,NA), 1149) #JJA
  msk_autumn = rep(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1), 1149) #SON
  
  
  
  
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

#################################################
## POINT CORRELATION FOR SITES USED WITH WIND ###
#################################################

WIND_WINTER = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_winter")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_winter")]])
)
WIND_SPRING = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_spring")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_spring")]])
)
WIND_SUMMER = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_summer")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_summer")]])
)
WIND_AUTUMN = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_autumn")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_autumn")]])
)

GLOBAL_FIELD_ALPHA <- 0.4
GLOBAL_ARROW_SIZE <- 0.015
GLOBAL_FONT_SIZE <- 8



for (ii in sites_used){

  lon_raw <- CAVES$site_info$longitude[ii]
  if(lon_raw<0){
    lon_raw = lon_raw+360
  }

  lon_cave = ceiling(lon_raw/360*96)
  lat_cave = ceiling((CAVES$site_info$latitude[ii]+90)/180*73)
  
  print(paste0("Start with CAVE ", ii))
  print(" --> staring Correlation calculation")
  
  name = paste0("SITE", ii)
  
  CORR_DATA_TI_Winter = array(dim = c(96,73))
  CORR_DATA_TI_P_Winter = array(dim = c(96,73))
  CORR_DATA_PI_Winter = array(dim = c(96,73))
  CORR_DATA_PI_P_Winter = array(dim = c(96,73))
  
  CORR_DATA_TI_Spring = array(dim = c(96,73))
  CORR_DATA_TI_P_Spring = array(dim = c(96,73))
  CORR_DATA_PI_Spring = array(dim = c(96,73))
  CORR_DATA_PI_P_Spring = array(dim = c(96,73))
  
  CORR_DATA_TI_Summer = array(dim = c(96,73))
  CORR_DATA_TI_P_Summer = array(dim = c(96,73))
  CORR_DATA_PI_Summer = array(dim = c(96,73))
  CORR_DATA_PI_P_Summer = array(dim = c(96,73))
  
  CORR_DATA_TI_Autumn = array(dim = c(96,73))
  CORR_DATA_TI_P_Autumn = array(dim = c(96,73))
  CORR_DATA_PI_Autumn = array(dim = c(96,73))
  CORR_DATA_PI_P_Autumn = array(dim = c(96,73))
  
  for (lon in 1:96){
    #print(paste0("  lon: ", lon))
    for (lat in 1:73){
        
      if(TRUE){
        COR_TI_Winter = cor.test(SIM_DATA_past1000_seasonal$WINTER_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$WINTER_world$temp[lon,lat,], na.rm  = TRUE)
        COR_PI_Winter = cor.test(SIM_DATA_past1000_seasonal$WINTER_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$WINTER_world$prec[lon,lat,], na.rm  = TRUE)
        COR_TI_Spring = cor.test(SIM_DATA_past1000_seasonal$SPRING_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$SPRING_world$temp[lon,lat,], na.rm  = TRUE)
        COR_PI_Spring = cor.test(SIM_DATA_past1000_seasonal$SPRING_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$SPRING_world$prec[lon,lat,], na.rm  = TRUE)
        COR_TI_Summer = cor.test(SIM_DATA_past1000_seasonal$SUMMER_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$SUMMER_world$temp[lon,lat,], na.rm  = TRUE)
        COR_PI_Summer = cor.test(SIM_DATA_past1000_seasonal$SUMMER_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$SUMMER_world$prec[lon,lat,], na.rm  = TRUE)
        COR_TI_Autumn = cor.test(SIM_DATA_past1000_seasonal$AUTUMN_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$AUTUMN_world$temp[lon,lat,], na.rm  = TRUE)
        COR_PI_Autumn = cor.test(SIM_DATA_past1000_seasonal$AUTUMN_world$isot[lon_cave,lat_cave,],
                          SIM_DATA_past1000_seasonal$AUTUMN_world$prec[lon,lat,], na.rm  = TRUE)
        
        CORR_DATA_TI_Winter[lon,lat] = COR_TI_Winter$estimate[[1]]
        CORR_DATA_TI_P_Winter[lon,lat] = COR_TI_Winter$p.value
        
        CORR_DATA_PI_Winter[lon,lat] = COR_PI_Winter$estimate[[1]]
        CORR_DATA_PI_P_Winter[lon,lat] = COR_PI_Winter$p.value
        
        CORR_DATA_TI_Spring[lon,lat] = COR_TI_Spring$estimate[[1]]
        CORR_DATA_TI_P_Spring[lon,lat] = COR_TI_Spring$p.value
        
        CORR_DATA_PI_Spring[lon,lat] = COR_PI_Spring$estimate[[1]]
        CORR_DATA_PI_P_Spring[lon,lat] = COR_PI_Spring$p.value
        
        CORR_DATA_TI_Summer[lon,lat] = COR_TI_Summer$estimate[[1]]
        CORR_DATA_TI_P_Summer[lon,lat] = COR_TI_Summer$p.value
        
        CORR_DATA_PI_Summer[lon,lat] = COR_PI_Summer$estimate[[1]]
        CORR_DATA_PI_P_Summer[lon,lat] = COR_PI_Summer$p.value
        
        CORR_DATA_TI_Autumn[lon,lat] = COR_TI_Autumn$estimate[[1]]
        CORR_DATA_TI_P_Autumn[lon,lat] = COR_TI_Autumn$p.value
        
        CORR_DATA_PI_Autumn[lon,lat] = COR_PI_Autumn$estimate[[1]]
        CORR_DATA_PI_P_Autumn[lon,lat] = COR_PI_Autumn$p.value
      }else{
        CORR_DATA_TI_Winter[lon,lat] = NA
        CORR_DATA_TI_P_Winter[lon,lat] = NA
        
        CORR_DATA_PI_Winter[lon,lat] = NA
        CORR_DATA_PI_P_Winter[lon,lat] = NA
        
        CORR_DATA_TI_Spring[lon,lat] = NA
        CORR_DATA_TI_P_Spring[lon,lat] = NA
        
        CORR_DATA_PI_Spring[lon,lat] = NA
        CORR_DATA_PI_P_Spring[lon,lat] = NA
        
        CORR_DATA_TI_Summer[lon,lat] = NA
        CORR_DATA_TI_P_Summer[lon,lat] = NA
        
        CORR_DATA_PI_Summer[lon,lat] = NA
        CORR_DATA_PI_P_Summer[lon,lat] = NA
        
        CORR_DATA_TI_Autumn[lon,lat] = NA
        CORR_DATA_TI_P_Autumn[lon,lat] = NA
        
        CORR_DATA_PI_Autumn[lon,lat] = NA
        CORR_DATA_PI_P_Autumn[lon,lat] = NA
       
      }
    }
  }
  
  plotting_mask_ti_winter <- CORR_DATA_TI_P_Winter
  plotting_mask_ti_winter[plotting_mask_ti_winter>0.05] = NA
  plotting_mask_ti_winter[plotting_mask_ti_winter<0.05] = 1
  
  plotting_mask_pi_winter <- CORR_DATA_PI_P_Winter
  plotting_mask_pi_winter[plotting_mask_pi_winter>0.05] = NA
  plotting_mask_pi_winter[plotting_mask_pi_winter<0.05] = 1
  
  plotting_mask_ti_spring <- CORR_DATA_TI_P_Spring
  plotting_mask_ti_spring[plotting_mask_ti_spring>0.05] = NA
  plotting_mask_ti_spring[plotting_mask_ti_spring<0.05] = 1
  
  plotting_mask_pi_spring <- CORR_DATA_PI_P_Spring
  plotting_mask_pi_spring[plotting_mask_pi_spring>0.05] = NA
  plotting_mask_pi_spring[plotting_mask_pi_spring<0.05] = 1
  
  plotting_mask_ti_summer <- CORR_DATA_TI_P_Summer
  plotting_mask_ti_summer[plotting_mask_ti_summer>0.05] = NA
  plotting_mask_ti_summer[plotting_mask_ti_summer<0.05] = 1
  
  plotting_mask_pi_summer <- CORR_DATA_PI_P_Summer
  plotting_mask_pi_summer[plotting_mask_pi_summer>0.05] = NA
  plotting_mask_pi_summer[plotting_mask_pi_summer<0.05] = 1
  
  plotting_mask_ti_autumn <- CORR_DATA_TI_P_Autumn
  plotting_mask_ti_autumn[plotting_mask_ti_autumn>0.05] = NA
  plotting_mask_ti_autumn[plotting_mask_ti_autumn<0.05] = 1
  
  plotting_mask_pi_autumn <- CORR_DATA_PI_P_Autumn
  plotting_mask_pi_autumn[plotting_mask_pi_autumn>0.05] = NA
  plotting_mask_pi_autumn[plotting_mask_pi_autumn<0.05] = 1
  
  remove(CORR_DATA_TI_P_Autumn, CORR_DATA_PI_P_Autumn, 
         CORR_DATA_TI_P_Winter, CORR_DATA_PI_P_Winter,
         CORR_DATA_TI_P_Spring, CORR_DATA_PI_P_Spring,
         CORR_DATA_TI_P_Summer, CORR_DATA_PI_P_Summer)
  
  print(" --> staring plotting for season: ")

  SITE_MAP = data.frame(
    lon = CAVES$site_info$longitude[ii],
    lat = CAVES$site_info$latitude[ii],
    cell_columns = c(1)
  )
  
  # winter
  plot_ti_winter <- STACYmap(gridlyr = rbind(CORR_DATA_TI_Winter[49:96,1:73]*plotting_mask_ti_winter[49:96,1:73],
                                             CORR_DATA_TI_Winter[1:48,1:73]*plotting_mask_ti_winter[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_WINTER,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. winter-T w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_pi_winter <- STACYmap(gridlyr = rbind(CORR_DATA_PI_Winter[49:96,1:73]*plotting_mask_pi_winter[49:96,1:73],
                                             CORR_DATA_PI_Winter[1:48,1:73]*plotting_mask_pi_winter[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_WINTER,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. winter-P w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  #spring
  plot_ti_spring <- STACYmap(gridlyr = rbind(CORR_DATA_TI_Spring[49:96,1:73]*plotting_mask_ti_spring[49:96,1:73],
                                             CORR_DATA_TI_Spring[1:48,1:73]*plotting_mask_ti_spring[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_SPRING,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. spring-T w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_pi_spring <- STACYmap(gridlyr = rbind(CORR_DATA_PI_Spring[49:96,1:73]*plotting_mask_pi_spring[49:96,1:73],
                                             CORR_DATA_PI_Spring[1:48,1:73]*plotting_mask_pi_spring[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_SPRING,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. spring-P w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  #summer
  plot_ti_summer <- STACYmap(gridlyr = rbind(CORR_DATA_TI_Summer[49:96,1:73]*plotting_mask_ti_summer[49:96,1:73],
                                             CORR_DATA_TI_Summer[1:48,1:73]*plotting_mask_ti_summer[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_SUMMER,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. summer-T w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_pi_summer <- STACYmap(gridlyr = rbind(CORR_DATA_PI_Summer[49:96,1:73]*plotting_mask_pi_summer[49:96,1:73],
                                             CORR_DATA_PI_Summer[1:48,1:73]*plotting_mask_pi_summer[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_SUMMER,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. summer-P w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  #autumn
  plot_ti_autumn <- STACYmap(gridlyr = rbind(CORR_DATA_TI_Autumn[49:96,1:73]*plotting_mask_ti_autumn[49:96,1:73],
                                             CORR_DATA_TI_Autumn[1:48,1:73]*plotting_mask_ti_autumn[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_AUTUMN,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. autumn-T w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_pi_autumn <- STACYmap(gridlyr = rbind(CORR_DATA_PI_Autumn[49:96,1:73]*plotting_mask_pi_autumn[49:96,1:73],
                                             CORR_DATA_PI_Autumn[1:48,1:73]*plotting_mask_pi_autumn[1:48,1:73]),
                             ptlyr = SITE_MAP,
                             fldlyr = WIND_AUTUMN,
                             zoom = c(-180, -60, 180, 73),
                             legend_names = list(grid = "Corr T-I"),
                             graticules = TRUE,
                             centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. autumn-P w/ I-comp incl wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  ggpubr::ggarrange(plot_ti_winter, plot_pi_winter,
                    plot_ti_spring, plot_pi_spring,
                    plot_ti_summer, plot_pi_summer, 
                    plot_ti_autumn, plot_pi_autumn,
                    ncol = 2, nrow = 4) %>%
    ggsave(file=paste0("Plots/A4_Field_Plots/Plots_site",ii,".png"), width = 210, height = 297, units = "mm")

  remove(plot_ti_winter, plot_pi_winter,
         plot_ti_spring, plot_pi_spring,
         plot_ti_summer, plot_pi_summer, 
         plot_ti_autumn, plot_pi_autumn, 
         plotting_mask_ti_winter, plotting_mask_pi_winter,
         plotting_mask_ti_spring, plotting_mask_pi_spring,
         plotting_mask_ti_summer, plotting_mask_pi_summer, 
         plotting_mask_ti_autumn, plotting_mask_pi_autumn,
         CORR_DATA_TI_Winter, CORR_DATA_PI_Winter,
         CORR_DATA_TI_Spring, CORR_DATA_PI_Spring,
         CORR_DATA_TI_Summer, CORR_DATA_PI_Summer,
         CORR_DATA_TI_Autumn, CORR_DATA_PI_Autumn)
  
}

remove(ii,lat,lat_cave, lon, lon_cave, lon_raw, name)



