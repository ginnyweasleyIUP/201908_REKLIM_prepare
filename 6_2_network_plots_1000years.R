#################################################
##NETWORK PLOTS##################################
#################################################

sites_to_entities_used = c()

for (entity in entities_used){
  sites_to_entities_used = c(sites_to_entities_used, sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id))
}

## LAST 1000 YEARS

# Nr. 8 und 9 müssen vorher ausgeführt werden um die entsprechenden Datensätze zu haben!!!

# 3 Types of correlation matrices are needed
#     1) Simulation with yearly resolution corresponding to all entities used
#     2) Simulation with same resolution as all entities
#     3) Records with all entities 

#################################################
## prepare correlation matrixes #################

prepare_corr_matrix_SIM = c()       # already equidistant over time period of 1150 years so no further work needed
prepare_corr_matrix_SIM_ba_ed = c() # the simulation is block averaged to the same resolution as the record. Also in order to do correlations
                                    # the irregular time series needs to be transformed to an equidistant one!
prepare_corr_matrix_REC_ed = c()    # already in lower resolution but in order to correlate, equidistance is needed!

diff_dt = c()
for (ii in entities_used){
  name = paste0("ENTITY",ii)
  diff_dt = c(diff_dt,mean(diff(DATA_last1000_mean[[name]]$value_record_age), na.rm = T))
}
diff_dt = mean(diff_dt, na.rm = T)

for (entity in entities_used){
  site <- sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id)
  name = paste0("ENTITY", entity)
  print(name)
  prepare_corr_matrix_SIM       = c(prepare_corr_matrix_SIM,       CAVES$yearly_data$isot[[site]])
  prepare_corr_matrix_SIM_ba_ed = c(prepare_corr_matrix_SIM_ba_ed, PaleoSpec::MakeEquidistant(DATA_last1000_mean[[name]]$value_record_age,
                                                                                              DATA_last1000_mean[[name]]$value_sim_d18O,
                                                                                              time.target = seq(from = -49, to = 1100, by = diff_dt)))
  prepare_corr_matrix_REC_ed    = c(prepare_corr_matrix_REC_ed,    PaleoSpec::MakeEquidistant(DATA_last1000_mean[[name]]$value_record_age,
                                                                                              DATA_last1000_mean[[name]]$value_record_d18O,
                                                                                              time.target = seq(from = -49, to = 1100, by = diff_dt)))
}

prepare_corr_matrix_SIM       <- matrix(prepare_corr_matrix_SIM,       nrow = 1150)
prepare_corr_matrix_SIM_ba_ed <- matrix(prepare_corr_matrix_SIM_ba_ed, nrow = length(seq(from = -49, to = 1100, by = diff_dt)))
prepare_corr_matrix_REC_ed    <- matrix(prepare_corr_matrix_REC_ed,    nrow = length(seq(from = -49, to = 1100, by = diff_dt)))

corr_matrix_SIM       <- Hmisc::rcorr(prepare_corr_matrix_SIM)
corr_matrix_SIM_ba_ed <- Hmisc::rcorr(prepare_corr_matrix_SIM_ba_ed)
corr_matrix_REC_ed    <- Hmisc::rcorr(prepare_corr_matrix_REC_ed)

remove(prepare_corr_matrix_SIM, prepare_corr_matrix_SIM_ba_ed, prepare_corr_matrix_REC_ed)

#################################################
## Plotting #####################################

source("Functions/networkmap_simple3.R")

# 1) Simulation fully resolved ##################

index_p_SIM<-corr_matrix_SIM$P>0.1 # all that are not-significant, as they will later be that that are NA-ed
C_SIM<- corr_matrix_SIM$r
C_SIM_p <- C_SIM
C_SIM_p[index_p_SIM] <- NA

## Network-Plot
pdf(file = paste(outdir,"network_map_Sim-full_2.pdf",sep=""), width = 7, height = 5)
networkmap_simple3(CMAT = C_SIM_p, 
                   lat = CAVES$site_info$latitude[sites_to_entities_used], 
                   lon = CAVES$site_info$longitude[sites_to_entities_used],
                   title = "Corr-Map HadCM3 past millenium, sig level = 0.1", 
                   thresh = 0.1)
dev.off()

# 2) Simulation with Record resolution ##########

index_p_SIM_ba_ed <-corr_matrix_SIM_ba_ed$P>0.1 # all that are not-significant, as they will later be that that are NA-ed
C_SIM_down<- corr_matrix_SIM_ba_ed$r
C_SIM_down_p <- C_SIM_down
C_SIM_down_p[index_p_SIM_ba_ed] <- NA

## Network-Plot
pdf(file = paste(outdir,"network_map_Sim-downsampled_2.pdf",sep=""), width = 7, height = 5)
networkmap_simple3(CMAT = C_SIM_down_p, 
                   lat = CAVES$site_info$latitude[sites_to_entities_used], 
                   lon = CAVES$site_info$longitude[sites_to_entities_used], 
                   title = "Corr-Map HadCM3 past mill. in record res., sig level = 0.1", 
                   thresh = 0.1)
dev.off()

# 2) Simulation with Record resolution ##########

index_p_REC_ed <-corr_matrix_REC_ed$P>0.1 # all that are not-significant, as they will later be that that are NA-ed
C_REC<- corr_matrix_REC_ed$r
C_REC_p <- C_REC
C_REC_p[index_p_REC_ed] <- NA

## Network-Plot
pdf(file = paste(outdir,"network_map_Record_2.pdf",sep=""), width = 7, height = 5)
networkmap_simple3(CMAT = C_REC_p, 
                   lat = CAVES$site_info$latitude[sites_to_entities_used], 
                   lon = CAVES$site_info$longitude[sites_to_entities_used], 
                   title = "Corr-Map SISAL records past millenium, sig level = 0.1", 
                   thresh = 0.10)
dev.off()

remove(C_SIM, C_SIM_p)

#################################################
## Distance Plots ###############################
#################################################

dist_matrix <- matrix(nrow = length(entities_used), ncol = length(entities_used))

for (ii in 1:dim(dist_matrix)[1]){
  for (jj in 1:dim(dist_matrix)[2]){
    dist_matrix[ii,jj] <- fossil::deg.dist(CAVES$site_info$longitude[sites_to_entities_used[ii]], 
                                           CAVES$site_info$latitude[sites_to_entities_used[ii]], 
                                           CAVES$site_info$longitude[sites_to_entities_used[jj]], 
                                           CAVES$site_info$latitude[sites_to_entities_used[jj]])
  }
}

dist_vec_sim <- as.vector(dist_matrix[upper.tri(dist_matrix)])
o <- order(dist_vec_sim)
dist_vec_sorted <- dist_vec_sim[o]
corr_vec_sim <- as.vector(corr_matrix_SIM$r[upper.tri(corr_matrix_SIM$r)])
corr_vec_sim_sorted <- corr_vec_sim[o]

# every_nth <- c(rep(c(TRUE, rep(FALSE, 9)),2236), TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)

plot(dist_vec_sorted, corr_vec_sim_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sim_sorted, corr_vec_sim_sorted, f = 1/5), lwd = 4, col = "#B2182B")

corr_vec_sim_ba_ed <- as.vector(corr_matrix_SIM_ba_ed$r[upper.tri(corr_matrix_SIM_ba_ed$r)])
corr_vec_sim_ba_ed_sorted <- corr_vec_sim_ba_ed[o]
plot(dist_vec_sorted, corr_vec_sim_ba_ed_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sim_sorted, corr_vec_sim_ba_ed_sorted, f = 1/5), lwd = 4, col = "#B2182B")

corr_vec_rec <- as.vector(corr_matrix_REC_ed$r[upper.tri(corr_matrix_REC_ed$r)])
corr_vec_rec_sorted <- corr_vec_rec[o]
plot(dist_vec_sorted, corr_vec_rec_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sorted, corr_vec_rec_sorted, f = 1/5), lwd = 4, col = "#B2182B")

#################################################
##MULTIPLOT######################################
#################################################

# Only possible after all above calculations have been done
scaling = 1.5
spacing = 0.7
pdf(file = paste(outdir,"network_sisal_vs_hadcm3_p01_c01_downsampled.pdf",sep=""), height= scaling*5, width = scaling*6)
par(mfrow=c(2,2), mai = c(rep(spacing, 4)), mar = c(3,3,2,0.5))
#SIM MAP
networkmap_simple3(CMAT = C_SIM_down_p, 
                   lat = CAVES$site_info$latitude[sites_to_entities_used], 
                   lon = CAVES$site_info$longitude[sites_to_entities_used], 
                   title = "Corr down-sampled HadCM3 past1000y, p<0.1", 
                   thresh = 0.1)
#down-sampled SIM Cor-Dist
plot(dist_vec_sorted, corr_vec_sim_ba_ed_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sim_sorted, corr_vec_sim_ba_ed_sorted, f = 1/5), lwd = 4, col = "#B2182B")
mtext("Distance between pairs (km)", side= 1, line = 2)

#SISAL MAP
networkmap_simple3(CMAT = C_REC, 
                   lat = CAVES$site_info$latitude[sites_to_entities_used], 
                   lon = CAVES$site_info$longitude[sites_to_entities_used], 
                   title = "Corr SISAL past millenium, p<0.1",
                   thresh = 0.2)
#SISAL Cor-Dist
plot(dist_vec_sorted, corr_vec_rec_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sorted, corr_vec_rec_sorted, f = 1/5), lwd = 4, col = "#B2182B")
mtext("Distance between pairs (km)", side= 1, line = 2)
dev.off()

#################################################
## Check for same sign ##########################

# 1) Record with full resolution sim
# 2) Record with same resolution sim

SAME_SIGN_full = array(dim = c(29,29))
SAME_SIGN_down = array(dim = c(29,29))

C1 <- corr_matrix_SIM$r
C2 <- corr_matrix_SIM_ba_ed$r
C3 <- corr_matrix_REC_ed$r

for (ii in 1:length(sites_to_entities_used)){
  for (jj in 1:length(sites_to_entities_used)){
    if(!is.na(C1[ii,jj]) & !is.na(C3[ii,jj])){
      if(sign(C1[ii,jj]) == sign(C3[ii,jj])){
        SAME_SIGN_full[ii,jj] = 1
      } else {
        SAME_SIGN_full[ii,jj] = -1
      }
    } else {
      SAME_SIGN_full[ii,jj] = NA
    }
    
    if(!is.na(C2[ii,jj]) & !is.na(C3[ii,jj])){
      if(sign(C2[ii,jj]) == sign(C3[ii,jj])){
        SAME_SIGN_down[ii,jj] = 1
      } else {
        SAME_SIGN_down[ii,jj] = -1
      }
    } else {
      SAME_SIGN_down[ii,jj] = NA
    }
    
  }
}

outdir = "/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_REKLIM_prepare/Plots/Network/"

col2 <- colorRampPalette(rev(c("#B2182B", "#D6604D", "#F4A582",
                               "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                               "#4393C3", "#2166AC")))

P1<-corr_matrix_SIM$P
P2<-corr_matrix_SIM_ba_ed$P
P3<-corr_matrix_REC_ed$P

SAME_SIGN_full[index_p_SIM] <- NA
SAME_SIGN_full[index_p_REC_ed] <- NA

SAME_SIGN_down[index_p_SIM_ba_ed] <- NA
SAME_SIGN_down[index_p_REC_ed] <- NA

colnames(C1)<- rownames(C1)<-colnames(C2)<- rownames(C2)<- colnames(C3)<- rownames(C3) <-as.character(entities_used)
colnames(SAME_SIGN_full)<- rownames(SAME_SIGN_full)<- colnames(SAME_SIGN_down)<- rownames(SAME_SIGN_down) <-as.character(entities_used)



pdf(file = paste(outdir,"combined_matrix_comp_full_2.pdf",sep=""))
corrplot(SAME_SIGN_full, 
         type ="full", 
         p.mat = P1+P3, 
         sig.level = 0.1, 
         diag = FALSE, 
         is.corr = TRUE, 
         col = col2(200),
         na.label = "square",
         na.label.col = "white")
dev.off()

pdf(file = paste(outdir,"combined_matrix_comp_down-sampled_2.pdf",sep=""))
corrplot(SAME_SIGN_down, 
         type ="full", 
         p.mat = P2+P3, 
         sig.level = 0.1, 
         diag = FALSE, 
         is.corr = TRUE, 
         col = col2(200),
         na.label = "square",
         na.label.col = "white")
dev.off()



remove(index_p_REC_ed, index_p_SIM, index_p_SIM_ba_ed)

#################################################
## Cross-Correlation Table from Kira ############
#replot for Cross-Corr from Simulation ##########
#################################################

#DETRENDING STILL MISSING!!!

#replot Kiras corr Matrix with the isot-data from the simulation
#entity site
#188    94
#209    107
#226    104
#286    136
#305    141
#326    151
#390    179
#97     27
#443    183
#33     4

kira_prep_matrix_2 <- matrix(c(PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY188$value_record_age,
                                                        DATA_last1000_mean$ENTITY188$value_record_d18O,
                                                        time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY209$value_record_age,
                                                          DATA_last1000_mean$ENTITY209$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY226$value_record_age,
                                                          DATA_last1000_mean$ENTITY226$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY286$value_record_age,
                                                          DATA_last1000_mean$ENTITY286$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY305$value_record_age,
                                                          DATA_last1000_mean$ENTITY305$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY329$value_record_age,
                                                          DATA_last1000_mean$ENTITY329$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY390$value_record_age,
                                                          DATA_last1000_mean$ENTITY390$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY97$value_record_age,
                                                          DATA_last1000_mean$ENTITY97$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY443$value_record_age,
                                                          DATA_last1000_mean$ENTITY443$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt)),
                               PaleoSpec::MakeEquidistant(DATA_last1000_mean$ENTITY33$value_record_age,
                                                          DATA_last1000_mean$ENTITY33$value_record_d18O,
                                                          time.target = seq(from = -49, to = 1100, by = diff_dt))), nrow = length(seq(from = -49, to = 1100, by = diff_dt)))

col2 <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                               "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                               "#4393C3", "#2166AC", "#053061")))

kira_corr_matrix_2 <- Hmisc::rcorr(kira_prep_matrix_2)
C<-kira_corr_matrix_2$r
P<-kira_corr_matrix_2$P
colnames(C)<- rownames(C)<-c("188", "209", "226", "286", "305", "326", "390", "97", "443", "33")

pdf(file = paste(outdir,"combined_matrix_colr_sim_2.pdf",sep=""))
corrplot(C, type = "full", p.mat = P, sig.level = 0.1, diag = FALSE, is.corr = TRUE, col = col2(200))
dev.off()

pdf(file = paste(outdir,"combined_matrix_number_sim_2.pdf",sep=""))
corrplot(C, type = "full", p.mat = P, sig.level = 0.1, diag = FALSE, is.corr = TRUE, col = col2(200), method="number")
dev.off()
