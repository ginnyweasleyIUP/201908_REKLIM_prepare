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

plot <- ggplot() + geom_polygon(data = land_map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
  coord_fixed(1.3) + 
  geom_point(data = NOT_corr_map, aes(x = x_lon, y = y_lat), color = "darkgrey", size = 0.2) + #(Non significant)
  geom_point(data = SIG_corr_map, aes(x = x_lon, y = y_lat, color = value)) + #(Significant with Color)
  scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Corr.") +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Correlation between local annual mean temperature \nand  d18O in precipitation, p<0.05")+
  xlab("")+ ylab("")+
  ylim(-60,80) + 
  xlim(-170, 180) +
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

# Reg not relevant as all regressions under r^2<0.5

# map <- map_data('world')
# 
# index_p <- CAVES$reg_data$temp_isot_rsquared>0.5
# index_c <- abs(CAVES$corr_data$prec_isot)>0.2
# noT <- sum(index_p*index_c, na.rm = TRUE)
# 
# index_plot  <- as.logical(index_p*index_c)
# index_nplot <- as.logical(abs(index_p*index_c-1))
# noT <- sum(index_plot, na.rm = TRUE)
# noF <- sum(index_nplot, na.rm =TRUE)
# 
# SIG_corr_map <-data.frame(
#   value = CAVES$corr_data$prec_isot[index_plot],
#   x_lon = CAVES$site_info$longitude[index_plot],
#   y_lat = CAVES$site_info$latitude[index_plot]
# )
# 
# NOT_corr_map <-data.frame(
#   value = CAVES$corr_data$prec_isot[index_nplot],
#   x_lon = CAVES$site_info$longitude[index_nplot],
#   y_lat = CAVES$site_info$latitude[index_nplot]
# )
# 
# plot <- ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group),fill ="white", color = NA) + 
#   coord_fixed(1.3) + 
#   geom_point(data = NOT_corr_map, aes(x = x_lon, y = y_lat), color = "darkgrey", size = 0.2) + #(Non significant)
#   geom_point(data = SIG_corr_map, aes(x = x_lon, y = y_lat, color = value)) + #(Significant with Color)
#   scale_color_gradient2(midpoint = 0, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Corr.") +
#   #scale_color_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red", space = "Lab" )
#   geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
#   ggtitle("Correlation between amount of local precipitation \nand local d18O in precipitation, p<0.05")+
#   ylim(-60,80) + 
#   theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
#         axis.title.x = element_text(size=11),
#         axis.title.y = element_text(size=11),
#         legend.text = element_text(size=11))
# 
# 
# plot %>% ggsave(filename = paste('map_reg_prec_isot_p005_c02', 'png', sep = '.'), plot = ., path = 'Plots', 
#                 width = 15, height = 10, units = 'cm', dpi = 'print')

