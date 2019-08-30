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
library(rgdal)



#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#https://egallic.fr/en/maps-with-r/

#################################################
## NAT EARTH DATA ###############################
#################################################

#Download from https://www.naturalearthdata.com/


layer <- ogrListLayers("Nat_Earth_Data/ne_10m_coastline.shp")
coastline_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_coastline.shp", verbose = FALSE, layer= layer)
land_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_land.shp", verbose = FALSE)
ocean_map <- readOGR(dsn ="Nat_Earth_Data/ne_10m_ocean.shp", verbose = FALSE)

##Verion #1 #####################################
# Use ggplot ####################################

MAP_VALUES <-data.frame(
  value = runif(10),
  x_lon = (runif(10)-0.5)*179,
  y_lat = (runif(10)-0.5)*90
)


plot <- ggplot() + geom_polygon(data = land_map, aes(x=long, y = lat, group = group),fill ="white", color = NA) +
  coord_fixed(1.3) + 
  geom_point(data = MAP_VALUES, aes(x = x_lon, y = y_lat, color = value), size = 3) +
  scale_color_gradient2(midpoint = 0.5, low = "#053061", mid = "white", high = "#67001F", space = "Lab", name = "Blablabla") +
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Choose a Title")+
  xlab("")+ ylab("")+
  ylim(-60,80) +
  xlim(-170, 180) +
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))


plot %>% ggsave(filename = paste('Map_blabla', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 10, units = 'cm', dpi = 'print')

##Version #2 ####################################

plot(c(-180, 180), c(-90, 90), type = "n")
map("world", add = TRUE, col = "grey", interior = FALSE)
symbols(x = MAP_VALUES$x_lon, y = MAP_VALUES$y_lat, circles = MAP_VALUES$value*5, inches = 1/20, bg = rgb(0.1, 0.9, 0.1, 0.5), fg = rgb(0.1, 0.9, 0.1), add = TRUE)
