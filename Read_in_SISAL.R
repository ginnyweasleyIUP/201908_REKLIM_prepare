library(plyr)
library(tidyverse)
wd <- 'wo auch immer deine SISAL csv files liegen'
prefix <- '' # ab v1c heißen die csv files anderes

wd = "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_Export_1b/"



load_data <- function(prefix, wd) {
  setwd(wd)

  composite_link_entity <- read.csv(paste(prefix, 'composite_link_entity.csv',sep = ''), header = T,stringsAsFactors = F)
  d13C <- read.csv(paste(prefix, 'd13C.csv',sep='') ,header = T, stringsAsFactors = F)
  d13C <- rename(d13C, iso_std_d13C = iso_std )
  d18O <- read.csv(paste(prefix, 'd18O.csv', sep =''),header = T, stringsAsFactors = F)
  d18O <- rename(d18O, iso_std_d18O = iso_std)
  dating_lamina <- read.csv(paste(prefix, 'dating_lamina.csv', sep = ''), header = T, stringsAsFactors = F)
  dating <- read.csv(paste(prefix, 'dating.csv',sep = ''), header = T, stringsAsFactors = F)
  entity_link_reference <- read.csv(paste(prefix, 'entity_link_reference.csv', sep = ''), header =T, stringsAsFactors = F)
  entity <- read.csv(paste(prefix, 'entity.csv', sep = ''), header = T, stringsAsFactors = F)
  gap <- read.csv(paste(prefix, 'gap.csv', sep = ''), header = T, stringsAsFactors = F)
  hiatus <- read.csv(paste(prefix, 'hiatus.csv', sep =''), header = T, stringsAsFactors = F)
  notes <- read.csv(paste(prefix, 'notes.csv', sep = ''), header = T, stringsAsFactors = F)
  original_chronology <- read.csv(paste(prefix, 'original_chronology.csv', sep = ''), header = T, stringsAsFactors = F)
  reference <- read.csv(paste(prefix, 'reference.csv', sep = ''), header = T, stringsAsFactors = F)
  sample <- read.csv(paste(prefix, 'sample.csv', sep = ''), header = T, stringsAsFactors = F)
  sisal_chronology <- read.csv(paste(prefix, 'sisal_chronology.csv', sep = ''), header = T, stringsAsFactors = F)
  site <- read.csv(paste(prefix, 'site.csv', sep = ''), header = T, stringsAsFactors = F)

  site_tb <- left_join(site, entity, by = 'site_id') %>% left_join(., entity_link_reference, by = 'entity_id') %>%
    left_join(., reference, by = 'ref_id') %>% left_join(., notes, by = 'site_id') %>% mutate_at(vars(site_id, entity_id), as.numeric)
  dating_tb <- dating %>% group_by(entity_id) %>%mutate(laminar_dated = if_else((entity_id %in% dating_lamina$entity_id), 'yes', 'no')) %>%
    mutate_at(vars(dating_id, depth_dating, dating_thickness, X14C_correction, corr_age, corr_age_uncert_pos, corr_age_uncert_neg), as.numeric) %>%ungroup()
  dating_tb_2 <- dating %>% left_join(.,entity, by = "entity_id") %>% filter(entity_status == "current") %>%
    mutate_at(vars(dating_id, depth_dating, dating_thickness, X14C_correction, corr_age, corr_age_uncert_pos, corr_age_uncert_neg), as.numeric)
  sample_tb <- join_all(list(sample,hiatus, gap, original_chronology, sisal_chronology, d13C, d18O), by = 'sample_id', type = 'left', match = 'all') %>%
    mutate_at(vars(entity_id, sample_id, sample_thickness, depth_sample, interp_age, interp_age_uncert_pos, interp_age_uncert_neg, COPRA_age,
                   COPRA_age_uncert_pos, COPRA_age_uncert_neg, linear_age, linear_age_uncert_pos, linear_age_uncert_neg, d13C_measurement,
                   d13C_precision, d18O_measurement, d18O_precision), as.numeric)

  return(list(site_tb, dating_tb, dating_tb_2, sample_tb))
}

data <- load_data(prefix, wd)


site_tb <- as.data.frame(data[1])
dating_tb <- as.data.frame(data[2])
dating_tb_2 <- as.data.frame(data[3]) # dating_tb_2 ist die tb, die Carla nochmal extra aufbereitet hat (siehe oben in der Funktion). Hier ist entity_status included
sample_tb <- as.data.frame(data[4])

##Plot###########################################

#Gut aus Kiras Analyse sind 286, 305, 390

s286 <- sample_tb %>% filter(entity_id == 286) #--> site_id 136 Mexiko: 17.4	-99.2
s305 <- sample_tb %>% filter(entity_id == 305) #--> site_id 141 Türkei: 41.42	31.93
s390 <- sample_tb %>% filter(entity_id == 390) #--> side_id 179 Romania: 45.1	22.8

#Bereite Tabelle vor: 4 Spalten: site_id, entity_id, interp_age, d18O_measurement, für die gilt, mehr als 50 d18O Messungen in den letzten 1150 Jahren über einen Zeitraum von mehr als 500 Jahren
# 1) filtere alle fie nicht C14 datiert sind oder laminations haben --> dann bleiben nur Ur/Th Datierungen übrig
dating_tb_3 <- dating_tb_2 %>% filter(date_used == "yes" & date_type != c("C14", "Event; end of laminations", "Event; start of laminations"))

# 2) aus sample ID, verwende nur die, die bei datib_tb_§ rausgekommen sind
#     filtere dann die, die mindestens ein Alter haben das jünger ist als 1100y. 
#     Die werden sortiert nach entity_id und dann gezählt. Übrig sollen nur die bleiben, mit mehr als 50 Datierungen. 
sample_min50 <- sample_tb %>% filter( entity_id %in% dating_tb_3$entity_id) %>%
  filter(interp_age < 1100) %>% group_by(entity_id) %>% count() %>% filter(n>50)

# 3) erstelle Tabelle die alle Messungen zu den oben selektierten entities erstellt
sample_min50_data <- sample_tb %>% filter(entity_id %in% sample_min50$entity_id) %>% select(entity_id, interp_age, d18O_measurement) %>% filter(interp_age<1100)

# 4) Füge der Liste oben eine vordere Spalte mit der site_id hinzu
site_min50 <- site_tb %>% filter(entity_id %in% sample_min50$entity_id) %>% select(site_id, entity_id) %>% distinct(site_id, entity_id) %>% right_join(., sample_min50_data, by="entity_id")
#     Füge der Liste zwei Spalten hinzu wie jüngste und älteste Messung gemacht wird, berechne Länge, filtere alle, sodass nur noch perioden >500 Jahre übrig bleiben
site_period <- site_min50 %>% group_by(entity_id) %>% 
  filter(entity_id != 51 && entity_id != 85 && entity_id != 123 && entity_id != 435) %>%
  summarise(min_corr_age = round(min(interp_age, na.rm = T), digits = 2),
            max_corr_age = round(max(interp_age, na.rm = T), digits = 2)) %>% 
  mutate(period = max_corr_age -min_corr_age) %>% filter(period > 700)

sites_used <- site_period[,1]$entity_id

# 5) finale Tabelle (aus site_min_50 werden nochmal die entfernt, die weniger als 500 Jahre lange Messungen haben)
sample_final <- site_min50 %>% filter(entity_id %in% site_period$entity_id) 


remove(dating_tb_2, dating_tb_3, sample_min50, sample_min50_data, site_min50, site_period)

time_caves=seq(from = -49, to = 1100, by = 1)

ed_matrix_sample_min50 <- matrix(nrow = 1150, ncol = length(sites_used)+1)
colnames(ed_matrix_sample_min50) <- c("time", sites_used)
ed_matrix_sample_min50[,1] = time_caves


for (ii in 1:length(sites_used)){
  s <- sample_final%>%filter(entity_id == sites_used[ii])
  ed_matrix_sample_min50[,ii+1] <- PaleoSpec::MakeEquidistant(s$interp_age, s$d18O_measurement, dt = 1, time_caves)
  remove(s)
}


C<-matrix(NA,nrow=length(sites_used),ncol=length(sites_used))
colnames(C)<-rownames(C)<-names(sites_used)
P<-C

for (ii in 1:(length(sites_used)-1)){
  print(ii)
  for (jj in (i+1):length(sites_used)){
    print(jj)
    print(ed_matrix_sample_min50[,ii+1])
    temp<-nexcf_ci(ed_matrix_sample_min50[,ii+1],ed_matrix_sample_min50[,jj+1],conflevel=0.1)
    
    C[ii,jj]<-temp$rxy
    P[ii,jj]<-P[jj,ii]<-temp$pval
    rm(temp)
  }
}



ed_matrix_sample_min50 <- as.data.frame(ed_matrix_sample_min50)






outdir = "/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_SA_Poster/Plots/Single_Cave/"

png(file = paste(outdir,"site_136_entity_286.png",sep=""), width = 10, height = 9, units = "cm", res = 300)
plot(time_caves, movavg(CAVES$yearly_data$isot[[136]],10),
     type = "l", 
     ylim = c(-9, -5), 
     lwd = 2, 
     xlab = "time BP in years",
     ylab = "d18O in (\u2030)",
     main = paste("d18O of", CAVES$site_info$site_name[136], "in Mexico", sep = " "))
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

#Try with ggplot:

png(file = paste(outdir,"s136-e286-s141-e305_temp-prec.png",sep=""), width = 24, height = 10.5, units = "cm", res = 300)
par(mfrow=c(1,2))
plot(time_caves, movavg(CAVES$yearly_data$temp[[136]],30),
     type = "l", 
     lwd = 2.5,
     xlab = "",
     ylab = "",
     ylim = c(11, 28),
     col = "#B2182B",
     main = "At site 136")
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
axis(side = 4)
mtext("", side = 4, line = 3)
plot(time_caves, movavg(CAVES$yearly_data$temp[[141]],30),
     type = "l", 
     lwd = 2.5,
     xlab = "",
     ylab = "",
     ylim = c(11,28),
     col = "#B2182B",
     main = "At site 141")
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
mtext("Precipitation in kg/m^2/s", side = 4, line = 3)
legend("topright", legend = c("Temp", "Prec"), col = c("#B2182B", "#2166AC"), lty = c(1,1), lwd = c(2,2))
dev.off()


