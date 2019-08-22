#################################################
##PLOT SINGLE CAVES##############################
#################################################

#CAVES:
#site_id  entity_id name  country          
#136	    286             Mexiko	      	     
#141	    305	      	    Türkei

#################################################
### PLOT Map CAVES###############################
#################################################

# First 4_Read_in_SISAL !!

# site_ids_used

#Gut aus Kiras Analyse sind 286, 305, 390

Other_map <-data.frame(
  x_lon = CAVES$site_info$longitude[site_ids_used],
  y_lat = CAVES$site_info$latitude[site_ids_used]
)

Example_map <-data.frame(
  x_lon = c(CAVES$site_info$longitude[136],CAVES$site_info$longitude[141]),
  y_lat = c(CAVES$site_info$latitude[136],CAVES$site_info$latitude[141])
)

plot <- ggplot() + geom_polygon(data = land_map, aes(x=long, y = lat, group = group),fill ="lightgrey", color = NA) +
  coord_fixed(1.3) + 
  geom_point(data = Other_map, aes(x = x_lon, y = y_lat), color = "black", size = 1.5) + #(Non significant)
  geom_point(data = Example_map, aes(x = x_lon, y = y_lat), color = "red", size = 4) + #(Significant with Color)
  geom_polygon(data = land_map, aes(x = long, y = lat, group = group), size = 0.05, color = "black", fill = NA, alpha = 0.5) +
  ggtitle("Cave locations SISAL past millenium with res>50 samples")+
  ylim(-60,80) + 
  xlim (-170,180) +
  xlab("")+ ylab("")+
  theme(plot.title = element_text(size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))


plot %>% ggsave(filename = paste('map_sisal_examplecaves', 'png', sep = '.'), plot = ., path = 'Plots', 
                width = 15, height = 12, units = 'cm', dpi = 'print')

#################################################
### PLOT single CAVES############################
#################################################


s286 <- sample_tb %>% filter(entity_id == 286) #--> site_id 136 Mexiko: 17.4	-99.2
s305 <- sample_tb %>% filter(entity_id == 305) #--> site_id 141 Türkei: 41.42	31.93

time_caves=seq(from = -49, to = 1100, by = 1)

outdir = "/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_SA_Poster/Plots/Single_Cave/"

png(file = paste(outdir,"site_136_entity_286.png",sep=""), width = 10, height = 9, units = "cm", res = 300)
plot(time_caves, movavg(CAVES$yearly_data$isot[[136]],10),
     type = "l", 
     ylim = c(-9, -5), 
     lwd = 2, 
     xlab = "time BP in years",
     ylab = "d18O in (\u2030)",
     main = paste("d18O of site 136 in Mexico", sep = " "))
lines(s286$interp_age, movavg(s286$d18O_measurement,10), xlim=c(-49,1100), type = "l", col = "#B2182B", lwd = 2)
dev.off()

png(file = paste(outdir,"site_136_entity_286_tempprec.png",sep=""), width = 10, height = 9, units = "cm", res = 300)
par(mar = c(5, 5, 3, 5))
plot(time_caves, movavg(CAVES$yearly_data$temp[[136]],30),
     type = "l", 
     lwd = 2,
     xlab = "time in years BP",
     ylab = "Temp in CC",
     ylim = c(24.5,27.5),
     col = "#B2182B",
     main = "Temp. and Prec. at site 136")
par(new = TRUE)
plot(time_caves, movavg(CAVES$yearly_data$prec[[136]],30), 
     type = "l",
     lwd = 2,
     xaxt = "n", 
     yaxt = "n",
     ylab = "",
     ylim = c(2.6e-5, 3.7e-5),
     xlab = "", 
     col = "#2166AC")#, 
#lty = 2)
axis(side = 4)
mtext("Precipitation in kg/m^2/s", side = 4, line = 3)
dev.off()

# par(mar = c(5, 5, 3, 5))
# plot(beaver1[1:100, 3], type ="l", ylab = "beaver1 temperature",
#      main = "Beaver Temperature Plot", xlab = "Time",
#      col = "blue")
# par(new = TRUE)
# plot(beaver2[,3], type = "l", xaxt = "n", yaxt = "n",
#      ylab = "", xlab = "", col = "red", lty = 2)
# axis(side = 4)
# mtext("beaver2 temperature", side = 4, line = 3)
# legend("topleft", c("beaver1", "beaver2"),
#        col = c("blue", "red"), lty = c(1, 2))

png(file = paste(outdir,"site_141_entity_305_leg-off.png",sep=""), width = 10, height = 9, units = "cm", res = 300)
plot(time_caves, movavg(CAVES$yearly_data$isot[[141]],10), 
     type = "l",
     ylim = c(-9, -4), 
     lwd = 2, 
     xlab = "time BP in years",
     ylab = "d18O in (\u2030)",
     main = paste("d18O of", CAVES$site_info$site_name[141], "in Turkey", sep = " "))
lines(s305$interp_age, movavg(s305$d18O_measurement,5), 
      xlim=c(-49,1100), 
      type = "l", 
      col = "#B2182B", lwd =2)
#legend("topright", legend = c("simulated", "measured"), col = c("black", "#B2182B"), lty = c(1,1), lwd = c(2,2))
dev.off()

png(file = paste(outdir,"site_141_entity_305_tempprec_leg-on.png",sep=""), width = 10, height = 9, units = "cm", res = 300)
par(mar = c(5, 5, 3, 5))
plot(time_caves, movavg(CAVES$yearly_data$temp[[141]],30),
     type = "l", 
     lwd = 2,
     xlab = "time in years BP",
     ylab = "Temp in CC",
     ylim = c(9, 14.5),
     col = "#B2182B",
     main = "Temp. and Prec. at site 136")
par(new = TRUE)
plot(time_caves, movavg(CAVES$yearly_data$prec[[141]],30), 
     type = "l",
     lwd = 2,
     xaxt = "n", 
     yaxt = "n",
     ylab = "",
     ylim = c(2.4e-5, 3.3e-5),
     xlab = "", 
     col = "#2166AC")#, 
#lty = 2)
axis(side = 4)
mtext("Precipitation in kg/m^2/s", side = 4, line = 3)
legend("topright", legend = c("Temp", "Prec"), col = c("#B2182B", "#2166AC"), lty = c(1,1), lwd = c(2,2))
dev.off()

lines(s390$interp_age, s390$d18O_measurement, xlim=c(-40,1100), type = "l", col = "blue")

#################################################
##Combination plot###############################

#d18O

png(file = paste(outdir,"s136-e286-s141-e305_d18O.png",sep=""), width = 24, height = 10.5, units = "cm", res = 300)
par(mfrow=c(1,2), mai = c(.6, 0.9, 0.5, 0.5), mar=c(3, 3,3,0.1))
plot(time_caves, movavg(CAVES$yearly_data$isot[[136]],10),
     type = "l", 
     ylim = c(-9, -5), 
     lwd = 2, 
     xlab = "",
     ylab = "",
     main = "d18O of site 136 in Mexico")
mtext("Time (years BP)", side = 1, line = 2)
mtext("d18O in (\u2030)", side= 2, line = 2)
lines(s286$interp_age, movavg(s286$d18O_measurement,10), xlim=c(-49,1100), type = "l", col = "#B2182B", lwd = 2)
plot(time_caves, movavg(CAVES$yearly_data$isot[[141]],10), 
     type = "l",
     ylim = c(-9, -5), 
     lwd = 2, 
     xlab = "",
     ylab = "",
     main = "d18O of site 141 in Turkey")
mtext("Time (years BP)", side = 1, line = 2)
lines(s305$interp_age, movavg(s305$d18O_measurement,5), 
      xlim=c(-49,1100), 
      type = "l", 
      col = "#B2182B", lwd =2)
legend("topright", legend = c("prec-d18O (HadCM3 simulated)", "Calcite d18O measured"), col = c("black", "#B2182B"), lty = c(1,1), lwd = c(2,2))
dev.off()


# TEMP + PREC

png(file = paste(outdir,"s136-e286-s141-e305_temp-prec.png",sep=""), width = 24, height = 10.5, units = "cm", res = 300)
par(mfrow=c(1,2), mai = c(0.6,0.6,0.5,0.6))
plot(time_caves, movavg(CAVES$yearly_data$temp[[136]],30),
     type = "l", 
     lwd = 2.5,
     xlab = "",
     ylab = "T",
     ylim = c(11, 28),
     col = "#B2182B",
     main = "Local T and P at site 136 in Mexico")
par(new = TRUE)
plot(time_caves, movavg(CAVES$yearly_data$prec[[136]],30), 
     type = "l",
     lwd = 2.5,
     xaxt = "n", 
     yaxt = "n",
     ylab = "",
     ylim = c(2.3e-5, 3.7e-5),
     xlab = "", 
     col = "#2166AC")#, 
#axis(side = 4)
#mtext("P", side = 4, line = 2)
mtext("T", side = 2, line = 2)
mtext("Time (years BP)", side = 1, line = 2)
plot(time_caves, movavg(CAVES$yearly_data$temp[[141]],30),
     type = "l", 
     lwd = 2.5,
     xlab = "",
     ylab = "",
     ylim = c(11,28),
     col = "#B2182B",
     main = "Local T and P at site 141 in Turkey")
par(new = TRUE)
plot(time_caves, movavg(CAVES$yearly_data$prec[[141]],30), 
     type = "l",
     lwd = 2.5,
     xaxt = "n", 
     yaxt = "n",
     ylab = "",
     ylim = c(2.3e-5, 3.7e-5),
     xlab = "", 
     col = "#2166AC")#, 
#lty = 2)
axis(side = 4)
mtext("P", side = 4, line = 2)
#mtext("T", side = 2, line = 2)
mtext("Time (years BP)", side = 1, line = 2)
legend("topright", legend = c("Temp", "Prec"), col = c("#B2182B", "#2166AC"), lty = c(1,1), lwd = c(2,2))
dev.off()
