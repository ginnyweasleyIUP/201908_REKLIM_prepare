#################################################
##PLOTTING OF MAPS AND GRAPHS####################
#################################################

#source("0_init_library.R")
#source("0_init_source.R")

##Create and analyse dataset first, if this didn't happen by now.
#source("1_create_dataset_001.R")
#source("2_analysis_dataset_002.R")

#################################################

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)
library(stringr)
library(rgdal)
library(sp)


#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#https://egallic.fr/en/maps-with-r/


##MAP PLOT#######################################

#ToDo: Wrap in functions so easier and tidier plots...

#################################################
# Which maps do I want? 
# 
# 4 plots within cave
#     1) Corr between Temp and Isot
#         a) nach p ausgegraut (p<0.1, 0.05, cor>0.1, 0.2)
#         b) p nicht ausgegraut
#     2) Corr btw Prec and Isot
#         a) nach p ausgegraut (p<0.1, 0.05, cor >0.1, 0.2)
#         b) p nicht ausgegraut
#     3) Reg btw Temp and Isot
#         a) mit r^2 ausgegraut (r^2>0.5, r^2>0.4, r^2>0.3)
#         b) ohne ausgrauen
#     4) Reg btw Prec and Isot
#         a) mit r^2 ausgegraut (r^2>0.5, r^2>0.4, r^2>0.3)
#         b) ohne ausgrauen
#
#     Zusätzlich
#       - Jahresmitteltemperatur
#       - Jahresmittelniederschlag
#       - Jahresmittelisotp.
#
#     Nicht als Map, sondern als Plot
#       - mean_temp vs mean isot
#       - mean prec vs mean isot
#
# 4 plots btw caves
#     1) Corr btw isotopy globally
#     2)??
#     3)??
#     4)??
#


layer <- ogrListLayers("Nat_Earth_Data/ne_10m_coastline.shp")
coastline_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_coastline.shp", verbose = FALSE, layer= layer)
land_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_land.shp", verbose = FALSE)
ocean_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_ocean.shp", verbose = FALSE)

# ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
#   coord_fixed(1.3) + 
#   geom_polygon(data = land_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
#   geom_tile(data = temp_map, aes(x = xmin, y = ymin, fill = mean_temp, width = 5, height = 3.3)) +
#   scale_fill_distiller(palette = "RdBu") +
#   geom_point(data = labs1, aes(x = long, y = lat), color = "white", size = 0.5) + 
#   geom_point(data = labs2, aes(x = long, y = lat), color = "white", size = 0.5)

##MAPS FOR WITHIN CAVE###########################

#1) Corr between Temp and Iso ###################
###a) p ausgegraut###############################

map <- map_data('world')

index_p <- CAVES$corr_data$temp_isot_p<0.05
index_c <- abs(CAVES$corr_data$temp_isot)>0.2
noT <- sum(index_p*index_c, na.rm = TRUE)

index_plot  <- as.logical(index_p*index_c)
index_nplot <- as.logical(abs(index_p*index_c-1))
noT <- sum(index_plot, na.rm = TRUE)
noF <- sum(index_nplot, na.rm =TRUE)

SIG_corr_map <-data.frame(
  value = CAVES$corr_data$temp_isot[index_plot],
  x_lon = CAVES$site_info$longitude[index_plot],
  y_lat = CAVES$site_info$latitude[index_plot]
)

NOT_corr_map <-data.frame(
  value = CAVES$corr_data$temp_isot[index_nplot],
  x_lon = CAVES$site_info$longitude[index_nplot],
  y_lat = CAVES$site_info$latitude[index_nplot]
)

plot <- ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
  coord_fixed(1.3) + 
  geom_point(data = NOT_corr_map, aes(x = x_lon, y = y_lat), color = "darkgrey", size = 0.2) + #(Non significant)
  geom_point(data = SIG_corr_map, aes(x = x_lon, y = y_lat, color = value)) + #(Significant with Color)
  scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Corr.") +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Correlation between local temperature and local d18O \nin precipitation, p<0.05")+
  ylim(-60,80) + 
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))


plot %>% ggsave(filename = paste('map_cor_temp_isot_p005_c02', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

#1) Corr between Prec and Iso ###################
###a) p ausgegraut###############################

map <- map_data('world')

index_p <- CAVES$corr_data$prec_isot_p<0.05
index_c <- abs(CAVES$corr_data$prec_isot)>0.2
noT <- sum(index_p*index_c, na.rm = TRUE)

index_plot  <- as.logical(index_p*index_c)
index_nplot <- as.logical(abs(index_p*index_c-1))
noT <- sum(index_plot, na.rm = TRUE)
noF <- sum(index_nplot, na.rm =TRUE)

SIG_corr_map <-data.frame(
  value = CAVES$corr_data$prec_isot[index_plot],
  x_lon = CAVES$site_info$longitude[index_plot],
  y_lat = CAVES$site_info$latitude[index_plot]
)

NOT_corr_map <-data.frame(
  value = CAVES$corr_data$prec_isot[index_nplot],
  x_lon = CAVES$site_info$longitude[index_nplot],
  y_lat = CAVES$site_info$latitude[index_nplot]
)

plot <- ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
  coord_fixed(1.3) + 
  geom_point(data = NOT_corr_map, aes(x = x_lon, y = y_lat), color = "darkgrey", size = 0.2) + #(Non significant)
  geom_point(data = SIG_corr_map, aes(x = x_lon, y = y_lat, color = value)) + #(Significant with Color)
  scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Corr.") +
  #scale_color_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red", space = "Lab" )
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Correlation between amount of local precipitation \nand local d18O in precipitation, p<0.05")+
  ylim(-60,80) + 
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))


plot %>% ggsave(filename = paste('map_cor_prec_isot_p005_c02', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

#3) Reg between Prec and Iso #######################

map <- map_data('world')

index_p <- CAVES$reg_data$temp_isot_rsquared>0.5
index_c <- abs(CAVES$corr_data$prec_isot)>0.2
noT <- sum(index_p*index_c, na.rm = TRUE)

index_plot  <- as.logical(index_p*index_c)
index_nplot <- as.logical(abs(index_p*index_c-1))
noT <- sum(index_plot, na.rm = TRUE)
noF <- sum(index_nplot, na.rm =TRUE)

SIG_corr_map <-data.frame(
  value = CAVES$corr_data$prec_isot[index_plot],
  x_lon = CAVES$site_info$longitude[index_plot],
  y_lat = CAVES$site_info$latitude[index_plot]
)

NOT_corr_map <-data.frame(
  value = CAVES$corr_data$prec_isot[index_nplot],
  x_lon = CAVES$site_info$longitude[index_nplot],
  y_lat = CAVES$site_info$latitude[index_nplot]
)

plot <- ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
  coord_fixed(1.3) + 
  geom_point(data = NOT_corr_map, aes(x = x_lon, y = y_lat), color = "darkgrey", size = 0.2) + #(Non significant)
  geom_point(data = SIG_corr_map, aes(x = x_lon, y = y_lat, color = value)) + #(Significant with Color)
  scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Corr.") +
  #scale_color_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red", space = "Lab" )
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Correlation between amount of local precipitation \nand local d18O in precipitation, p<0.05")+
  ylim(-60,80) + 
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))


plot %>% ggsave(filename = paste('map_reg_prec_isot_p005_c02', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')



#################################################

# map <- map_data('world')
# map <- load_natural_earth_data()
# 
# labs1 <- data.frame(
#   long = CAVES$site_info$site_lon[1:20],
#   lat = CAVES$site_info$site_lat[1:20],
#   names = CAVES$site_info$site_name[1:20],
#   stringsAsFactors = FALSE
# )
# 
# labs2 <- data.frame(
#   long = CAVES$site_info$site_lon[20:40],
#   lat = CAVES$site_info$site_lat[20:40],
#   names = CAVES$site_info$site_name[20:40],
#   stringsAsFactors = FALSE
# )
# 
# marker = list(color = brewer.pal(10, "RdBu"))
# 
# breaks <- c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38)
# lesCouleurs <- data.frame(
#   val = levels(cut(range(breaks), breaks = breaks)),
#   col = brewer.pal(10, "RdBu")
# )
# 
# temp_map<- data.frame(
#   mean_temp = CAVES$site_info$mean_temp,
#   xmin = CAVES$site_info$long_start,
#   xmax = CAVES$site_info$long_stor,
#   ymin = CAVES$site_info$lat_start,
#   ymax = CAVES$site_info$lat_stop
# )
# 
# temp_map$interval =cut(temp_map$mean_temp, breaks = breaks)
# 
# prec_map <-data.frame(
#   mean_prec = CAVES$site_info$mean_prec,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# isot_map <-data.frame(
#   value = CAVES$site_info$mean_isot,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# corr_temp_prec_map <-data.frame(
#   value = CAVES$corr_data$temp_prec_alpha,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# corr_temp_isot_map <- data.frame(
#   value = CAVES$corr_data$temp_isot_alpha,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# corr_prec_isot_map <- data.frame(
#   value = CAVES$corr_data$prec_isot_alpha,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# reg_temp_prec_map <-data.frame(
#   value = CAVES$reg_data$temp_prec_alpha,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# reg_temp_isot_map <- data.frame(
#   value = CAVES$reg_data$temp_isot_alpha,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# reg_prec_isot_map <- data.frame(
#   value = CAVES$reg_data$prec_isot_alpha,
#   x_lon =    CAVES$site_info$site_lon,
#   y_lat =     CAVES$site_info$site_lat
# )
# 
# 
# 
# 
# 
# plot <- ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
#   coord_fixed(1.3) + 
#   geom_tile(data = reg_temp_prec_map, aes(x = x_lon, y = y_lat, fill = value, width = 8, height = 4.5)) +
#   scale_fill_distiller(palette = "RdBu") +
#   geom_polygon(data = land_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
#   geom_point(data = labs1, aes(x = long, y = lat), color = "white", size = 0.1) + 
#   geom_point(data = labs2, aes(x = long, y = lat), color = "white", size = 0.1) +
#   ggtitle("Regression alpha btw temp and prec at cave site")+
#   theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
#         axis.title.x = element_text(size=12),
#         axis.title.y = element_text(size=12),
#         legend.text = element_text(size=12)) 
# 
# plot %>% ggsave(filename = paste('map_reg_temp_prec', 'png', sep = '.'), plot = ., path = 'Plots', 
#          width = 15, height = 10, units = 'cm', dpi = 'print')
# 
# 
# ##OTHER PLOTS####################################
# 
# png(file = "Plots/plot_regtempprec_vs_isot.png", width = 600, height = 400)
# plot(CAVES$reg_data$temp_prec_alpha, CAVES$site_info$mean_isot, 
#      xlab = "Reg btw temp and prec", 
#      ylab = "isot", 
#      main = "Isot dependency on regression btw temp and prec")
# dev.off()
# 
# load_natural_earth_data <- function(file, dir = "Nat_Earth_Data") {
#   if (str_detect(file, '.shp')) {
#     data <- readOGR(dsn = file.path(dir, file), verbose = FALSE, ...)
#   } else if (str_detect(file, '.tif')) {
#     data <- raster::raster(x = file.path(NAT_EARTH_DATA_PATH, file))
#   }
#   #file.remove(tempdir())
#   return(data)
# }
