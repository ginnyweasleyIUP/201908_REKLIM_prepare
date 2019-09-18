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
## YEARLY AND SEASONAL MEANS ####################
#################################################

# Assume Wind strength calculation already exist, otherwise do script 9_1 again without plotting!!!

## 1) Plot 4 Plots for site 136 and site 141 for Poster

#--> changes to before --> Wind will be plotted everywhere, will be greyer than before


#################################################
## CALCULATE POINT CORRELATION WITH MASKS #######
#################################################

POINT_CORR_DATA_past1000 <- vector(mode = "list")


#################################################
## POINT CORRELATION FOR SITES USED WITH WIND ###
#################################################

for (ii in sites_used){
  
  name = paste0("CAVE", ii)
  
  POINT_CORR_DATA_past1000[[name]] <- list(
    CORR_DATA_TI = array(dim = c(96,73)),
    CORR_DATA_TI_P = array(dim = c(96,73)),
    CORR_DATA_PI = array(dim = c(96,73)),
    CORR_DATA_PI_P = array(dim = c(96,73)),
    plotting_mask_ti = array(dim = c(96,73)),
    plotting_mask_pi = array(dim = c(96,73))
  )

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

        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[lon,lat] = COR_TI$estimate[[1]]
        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI_P[lon,lat] = COR_TI$p.value

        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[lon,lat] = COR_PI$estimate[[1]]
        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI_P[lon,lat] = COR_PI$p.value
      }else{
        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[lon,lat] = NA
        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI_P[lon,lat] = NA

        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[lon,lat] = NA
        POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI_P[lon,lat] = NA
      }
    }
  }

  plotting_mask_ti <- POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI_P
  plotting_mask_ti[plotting_mask_ti>0.1] = NA
  plotting_mask_ti[plotting_mask_ti<0.1] = 1
  
  POINT_CORR_DATA_past1000[[name]]$plotting_mask_ti <- plotting_mask_ti

  plotting_mask_pi <- POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI_P
  plotting_mask_pi[plotting_mask_pi>0.1] = NA
  plotting_mask_pi[plotting_mask_pi<0.1] = 1
  
  POINT_CORR_DATA_past1000[[name]]$plotting_mask_pi <- plotting_mask_pi

}

remove(plotting_mask_pi, plotting_mask_ti, CORR_DATA_PI, CORR_DATA_PI_P, CORR_DATA_TI, CORR_DATA_TI_P, COR_PI, COR_TI)

## 2 Create pdf for more plots 

GLOBAL_ARROW_SIZE <- 0.025
GLOBAL_POINT_SIZE <- 4
GLOBAL_FIELD_ALPHA <- 0.2

WIND_summer = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_", "summer")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_", "summer")]])
)
WIND_autumn = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_", "autumn")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_", "autumn")]])
)
WIND_winter = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_", "winter")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_", "winter")]])
)
WIND_spring = data.frame(
  lon = rep(c(seq(0, 179, length.out = 48),seq(-179, 0, length.out = 48)),72),
  lat = rep(seq(88.5, -88.5, length.out = 72), each = 96),
  angle = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_ANGLE_", "spring")]]),
  radius = as.vector(SIM_DATA_past1000_wind[[1]][[paste0("WIND_MEAN_STRENGTH_", "spring")]])
)

plot_list = vector(mode = "list")

for (ii in 1:10){
  
  name = paste0("CAVE", sites_used[ii])
  
  SITE_MAP = data.frame(
    x_lon = CAVES$site_info$longitude[sites_used[ii]],
    y_lat = CAVES$site_info$latitude[sites_used[ii]],
    cell_columns = c(1)
  )
  
  print(paste0(ii, " ", name, " ", season))
      
  plot_t_summer <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_summer,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. T w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_t_autumn <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_autumn,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. T w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_t_winter <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_winter,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. T w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_t_spring <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_TI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_spring,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. T w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_p_summer <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_summer,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. P w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_p_autumn <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_autumn,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. P w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_p_winter <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_winter,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. P w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
  
  plot_p_spring <- STACYmap(gridlyr = rbind(POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[49:96,1:73]*plotting_mask_ti[49:96,1:73],
                                            POINT_CORR_DATA_past1000[[name]]$CORR_DATA_PI[1:48,1:73]*plotting_mask_ti[1:48,1:73]),
                            ptlyr = SITE_MAP,
                            fldlyr = WIND_spring,
                            zoom = c(-180, -60, 180, 73),
                            legend_names = list(grid = "Corr T-I"),
                            graticules = TRUE,
                            centercolor = list(grid = "white", pt = "white")) +
    ggtitle(paste0("Corr. P w/ local isot-comp incl ", season, "-wind, p<0.05, site ", ii)) +
    theme(plot.title = element_text(h = 0.5),
          axis.text = element_blank())
      
    assign(paste0("plot_", name), ggpubr::ggarrange(plot_t_summer, plot_t_autumn, plot_t_winter, plot_t_spring, plot_p_summer, plot_p_autumn, plot_p_winter, plot_p_spring,
                                       ncol = 8, nrow = 1))

}
pdf(file = "trya4.pdf", paper = "a4")
par(mfrow = c(10,1))
plot_CAVE4
plot_CAVE7
plot_CAVE17
plot_CAVE27
plot_CAVE39
plot_CAVE41
plot_CAVE42
plot_CAVE63
plot_CAVE80
plot_CAVE85
dev.off()


# pdf(file = paste(outdir,"s136-e286-s141-e305_temp-prec.png",sep=""), paper = "a4")
# #par(mfrow=c(1,2), mai = c(0.6,0.6,0.5,0.6))
# par(mfrow=c(1,2), mai = c(.6, 0, 0.5, 0), mar=c(3, 2,3,2))
# plot(time_caves, movavg(CAVES$yearly_data$temp[[136]],30),
#      type = "l", 
#      lwd = 2.5,
#      xlab = "",
#      ylab = "T",
#      ylim = c(11, 28),
#      xlim = c(-49,1020),
#      col = "#B2182B",
#      main = "Local T and P at site 136 in Mexico")
# par(new = TRUE)
# plot(time_caves, movavg(CAVES$yearly_data$prec[[136]],30), 
#      type = "l",
#      lwd = 2.5,
#      xaxt = "n", 
#      yaxt = "n",
#      ylab = "",
#      ylim = c(2.3e-5, 3.7e-5),
#      xlim = c(-49,1020),
#      xlab = "", 
#      col = "#2166AC")#, 
# #axis(side = 4)
# #mtext("P", side = 4, line = 2)
# mtext("T", side = 2, line = 2)
# mtext("Time (years BP)", side = 1, line = 2)
# plot(time_caves, movavg(CAVES$yearly_data$temp[[141]],30),
#      type = "l", 
#      lwd = 2.5,
#      xlab = "",
#      ylab = "",
#      ylim = c(11,28),
#      xlim = c(-49,1020),
#      col = "#B2182B",
#      main = "Local T and P at site 141 in Turkey")
# par(new = TRUE)
# plot(time_caves, movavg(CAVES$yearly_data$prec[[141]],30), 
#      type = "l",
#      lwd = 2.5,
#      xaxt = "n", 
#      yaxt = "n",
#      ylab = "",
#      ylim = c(2.3e-5, 3.7e-5),
#      xlim = c(-49,1020),
#      xlab = "", 
#      col = "#2166AC")#, 
# #lty = 2)
# axis(side = 4)
# mtext("P", side = 4, line = 2)
# #mtext("T", side = 2, line = 2)
# mtext("Time (years BP)", side = 1, line = 2)
# legend("topright", legend = c("T", "P"), col = c("#B2182B", "#2166AC"), lty = c(1,1), lwd = c(2,2))
# dev.off()
# 
# 
# 
# 
