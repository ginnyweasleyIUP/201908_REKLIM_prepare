library(plyr)
library(tidyverse)
wd <- "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_Export_1b/"
prefix <- '' # ab v1c heißen die csv files anderes



load_data <- function(prefix, wd) {
  composite_link_entity <- read.csv(paste(wd, prefix, 'composite_link_entity.csv',sep = ''), header = T,stringsAsFactors = F)
  d13C <- read.csv(paste(wd, prefix, 'd13C.csv',sep='') ,header = T, stringsAsFactors = F)
  d13C <- rename(d13C, iso_std_d13C = iso_std )
  d18O <- read.csv(paste(wd, prefix, 'd18O.csv', sep =''),header = T, stringsAsFactors = F)
  d18O <- rename(d18O, iso_std_d18O = iso_std)
  dating_lamina <- read.csv(paste(wd, prefix, 'dating_lamina.csv', sep = ''), header = T, stringsAsFactors = F)
  dating <- read.csv(paste(wd, prefix, 'dating.csv',sep = ''), header = T, stringsAsFactors = F)
  entity_link_reference <- read.csv(paste(wd, prefix, 'entity_link_reference.csv', sep = ''), header =T, stringsAsFactors = F)
  entity <- read.csv(paste(wd, prefix, 'entity.csv', sep = ''), header = T, stringsAsFactors = F)
  gap <- read.csv(paste(wd, prefix, 'gap.csv', sep = ''), header = T, stringsAsFactors = F)
  hiatus <- read.csv(paste(wd, prefix, 'hiatus.csv', sep =''), header = T, stringsAsFactors = F)
  notes <- read.csv(paste(wd, prefix, 'notes.csv', sep = ''), header = T, stringsAsFactors = F)
  original_chronology <- read.csv(paste(wd, prefix, 'original_chronology.csv', sep = ''), header = T, stringsAsFactors = F)
  reference <- read.csv(paste(wd, prefix, 'reference.csv', sep = ''), header = T, stringsAsFactors = F)
  sample <- read.csv(paste(wd, prefix, 'sample.csv', sep = ''), header = T, stringsAsFactors = F)
  sisal_chronology <- read.csv(paste(wd, prefix, 'sisal_chronology.csv', sep = ''), header = T, stringsAsFactors = F)
  site <- read.csv(paste(wd, prefix, 'site.csv', sep = ''), header = T, stringsAsFactors = F)

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

#Bereite Tabelle vor: 4 Spalten: site_id, entity_id, interp_age, d18O_measurement, für die gilt, mehr als 50 d18O Messungen in den letzten 1150 Jahren über einen Zeitraum von mehr als 500 Jahren
# 1) filtere alle fie nicht C14 datiert sind oder laminations haben --> dann bleiben nur Ur/Th Datierungen übrig
dating_tb_3 <- dating_tb_2 %>% filter(date_used == "yes" & date_type != c("C14", "Event; end of laminations", "Event; start of laminations"))

# 2) aus sample ID, verwende nur die, die bei dating_tb_3 rausgekommen sind
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
  filter(min_corr_age < 0 &  max_corr_age > 1000) %>%
  mutate(period = max_corr_age -min_corr_age) %>% filter(period > 700)

entities_used <- site_period[,1]$entity_id

# 5) finale Tabelle (aus site_min_50 werden nochmal die entfernt, die weniger als 500 Jahre lange Messungen haben)
sample_final <- site_min50 %>% filter(entity_id %in% site_period$entity_id)  %>% filter(interp_age < 1100)

sites_used <- sample_final %>% group_by(site_id)%>% count(site_id)%>% select(site_id)# %>% arrange(site_id)
sites_used <- sites_used[,1]$site_id

sites_entities_used <- sample_final %>% count(site_id, entity_id)%>% select(site_id, entity_id)

remove(dating_tb_2, dating_tb_3, sample_min50, sample_min50_data, site_period)
setwd("/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/")



