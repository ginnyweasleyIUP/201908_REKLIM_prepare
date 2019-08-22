#################################################
##NETWORK PLOTS##################################
#################################################

# Big question, which data should be used? --> what sites from SISAL database are past millenium and >50 samples in that period
# --> plot only those!!!

#Prepare Cross-Correlation Matix

prepare_corr_matrix <- matrix(c(CAVES$yearly_data$isot[[1]],   CAVES$yearly_data$isot[[2]],   CAVES$yearly_data$isot[[3]],   CAVES$yearly_data$isot[[4]],   CAVES$yearly_data$isot[[5]],   CAVES$yearly_data$isot[[6]],   CAVES$yearly_data$isot[[7]],   CAVES$yearly_data$isot[[8]],   CAVES$yearly_data$isot[[9]],   CAVES$yearly_data$isot[[10]],
                                CAVES$yearly_data$isot[[11]],  CAVES$yearly_data$isot[[12]],  CAVES$yearly_data$isot[[13]],  CAVES$yearly_data$isot[[14]],  CAVES$yearly_data$isot[[15]],  CAVES$yearly_data$isot[[16]],  CAVES$yearly_data$isot[[17]],  CAVES$yearly_data$isot[[18]],  CAVES$yearly_data$isot[[19]],  CAVES$yearly_data$isot[[20]], 
                                CAVES$yearly_data$isot[[21]],  CAVES$yearly_data$isot[[22]],  CAVES$yearly_data$isot[[23]],  CAVES$yearly_data$isot[[24]],  CAVES$yearly_data$isot[[25]],  CAVES$yearly_data$isot[[26]],  CAVES$yearly_data$isot[[27]],  CAVES$yearly_data$isot[[28]],  CAVES$yearly_data$isot[[29]],  CAVES$yearly_data$isot[[30]], 
                                CAVES$yearly_data$isot[[31]],  CAVES$yearly_data$isot[[32]],  CAVES$yearly_data$isot[[33]],  CAVES$yearly_data$isot[[34]],  CAVES$yearly_data$isot[[35]],  CAVES$yearly_data$isot[[36]],  CAVES$yearly_data$isot[[37]],  CAVES$yearly_data$isot[[38]],  CAVES$yearly_data$isot[[39]],  CAVES$yearly_data$isot[[40]],
                                CAVES$yearly_data$isot[[41]],  CAVES$yearly_data$isot[[42]],  CAVES$yearly_data$isot[[43]],  CAVES$yearly_data$isot[[44]],  CAVES$yearly_data$isot[[45]],  CAVES$yearly_data$isot[[46]],  CAVES$yearly_data$isot[[47]],  CAVES$yearly_data$isot[[48]],  CAVES$yearly_data$isot[[49]],  CAVES$yearly_data$isot[[50]],
                                CAVES$yearly_data$isot[[51]],  CAVES$yearly_data$isot[[52]],  CAVES$yearly_data$isot[[53]],  CAVES$yearly_data$isot[[54]],  CAVES$yearly_data$isot[[55]],  CAVES$yearly_data$isot[[56]],  CAVES$yearly_data$isot[[57]],  CAVES$yearly_data$isot[[58]],  CAVES$yearly_data$isot[[59]],  CAVES$yearly_data$isot[[60]],
                                CAVES$yearly_data$isot[[61]],  CAVES$yearly_data$isot[[62]],  CAVES$yearly_data$isot[[63]],  CAVES$yearly_data$isot[[64]],  CAVES$yearly_data$isot[[65]],  CAVES$yearly_data$isot[[66]],  CAVES$yearly_data$isot[[67]],  CAVES$yearly_data$isot[[68]],  CAVES$yearly_data$isot[[69]],  CAVES$yearly_data$isot[[70]],
                                CAVES$yearly_data$isot[[71]],  CAVES$yearly_data$isot[[72]],  CAVES$yearly_data$isot[[73]],  CAVES$yearly_data$isot[[74]],  CAVES$yearly_data$isot[[75]],  CAVES$yearly_data$isot[[76]],  CAVES$yearly_data$isot[[77]],  CAVES$yearly_data$isot[[78]],  CAVES$yearly_data$isot[[79]],  CAVES$yearly_data$isot[[80]],
                                CAVES$yearly_data$isot[[81]],  CAVES$yearly_data$isot[[82]],  CAVES$yearly_data$isot[[83]],  CAVES$yearly_data$isot[[84]],  CAVES$yearly_data$isot[[85]],  CAVES$yearly_data$isot[[86]],  CAVES$yearly_data$isot[[87]],  CAVES$yearly_data$isot[[88]],  CAVES$yearly_data$isot[[89]],  CAVES$yearly_data$isot[[90]],
                                CAVES$yearly_data$isot[[91]],  CAVES$yearly_data$isot[[92]],  CAVES$yearly_data$isot[[93]],  CAVES$yearly_data$isot[[94]],  CAVES$yearly_data$isot[[95]],  CAVES$yearly_data$isot[[96]],  CAVES$yearly_data$isot[[97]],  CAVES$yearly_data$isot[[98]],  CAVES$yearly_data$isot[[99]],  CAVES$yearly_data$isot[[100]],
                                CAVES$yearly_data$isot[[101]], CAVES$yearly_data$isot[[102]], CAVES$yearly_data$isot[[103]], CAVES$yearly_data$isot[[104]], CAVES$yearly_data$isot[[105]], CAVES$yearly_data$isot[[106]], CAVES$yearly_data$isot[[107]], CAVES$yearly_data$isot[[108]], CAVES$yearly_data$isot[[109]], CAVES$yearly_data$isot[[110]],
                                CAVES$yearly_data$isot[[111]], CAVES$yearly_data$isot[[112]], CAVES$yearly_data$isot[[113]], CAVES$yearly_data$isot[[114]], CAVES$yearly_data$isot[[115]], CAVES$yearly_data$isot[[116]], CAVES$yearly_data$isot[[117]], CAVES$yearly_data$isot[[118]], CAVES$yearly_data$isot[[119]], CAVES$yearly_data$isot[[120]],
                                CAVES$yearly_data$isot[[121]], CAVES$yearly_data$isot[[122]], CAVES$yearly_data$isot[[123]], CAVES$yearly_data$isot[[124]], CAVES$yearly_data$isot[[125]], CAVES$yearly_data$isot[[126]], CAVES$yearly_data$isot[[127]], CAVES$yearly_data$isot[[128]], CAVES$yearly_data$isot[[129]], CAVES$yearly_data$isot[[130]],
                                CAVES$yearly_data$isot[[131]], CAVES$yearly_data$isot[[132]], CAVES$yearly_data$isot[[133]], CAVES$yearly_data$isot[[134]], CAVES$yearly_data$isot[[135]], CAVES$yearly_data$isot[[136]], CAVES$yearly_data$isot[[137]], CAVES$yearly_data$isot[[138]], CAVES$yearly_data$isot[[139]], CAVES$yearly_data$isot[[140]],
                                CAVES$yearly_data$isot[[141]], CAVES$yearly_data$isot[[142]], CAVES$yearly_data$isot[[143]], CAVES$yearly_data$isot[[144]], CAVES$yearly_data$isot[[145]], CAVES$yearly_data$isot[[146]], CAVES$yearly_data$isot[[147]], CAVES$yearly_data$isot[[148]], CAVES$yearly_data$isot[[149]], CAVES$yearly_data$isot[[150]],
                                CAVES$yearly_data$isot[[151]], CAVES$yearly_data$isot[[152]], CAVES$yearly_data$isot[[153]], CAVES$yearly_data$isot[[154]], CAVES$yearly_data$isot[[155]], CAVES$yearly_data$isot[[156]], CAVES$yearly_data$isot[[157]], CAVES$yearly_data$isot[[158]], CAVES$yearly_data$isot[[159]], CAVES$yearly_data$isot[[160]],
                                CAVES$yearly_data$isot[[161]], CAVES$yearly_data$isot[[162]], CAVES$yearly_data$isot[[163]], CAVES$yearly_data$isot[[164]], CAVES$yearly_data$isot[[165]], CAVES$yearly_data$isot[[166]], CAVES$yearly_data$isot[[167]], CAVES$yearly_data$isot[[168]], CAVES$yearly_data$isot[[169]], CAVES$yearly_data$isot[[170]],
                                CAVES$yearly_data$isot[[171]], CAVES$yearly_data$isot[[172]], CAVES$yearly_data$isot[[173]], CAVES$yearly_data$isot[[174]], CAVES$yearly_data$isot[[175]], CAVES$yearly_data$isot[[176]], CAVES$yearly_data$isot[[177]], CAVES$yearly_data$isot[[178]], CAVES$yearly_data$isot[[179]], CAVES$yearly_data$isot[[180]],
                                CAVES$yearly_data$isot[[181]], CAVES$yearly_data$isot[[182]], CAVES$yearly_data$isot[[183]], CAVES$yearly_data$isot[[184]], CAVES$yearly_data$isot[[185]], CAVES$yearly_data$isot[[186]], CAVES$yearly_data$isot[[187]], CAVES$yearly_data$isot[[188]], CAVES$yearly_data$isot[[189]], CAVES$yearly_data$isot[[190]],
                                CAVES$yearly_data$isot[[191]], CAVES$yearly_data$isot[[192]], CAVES$yearly_data$isot[[193]], CAVES$yearly_data$isot[[194]], CAVES$yearly_data$isot[[195]], CAVES$yearly_data$isot[[196]], CAVES$yearly_data$isot[[197]], CAVES$yearly_data$isot[[198]], CAVES$yearly_data$isot[[199]], CAVES$yearly_data$isot[[200]],
                                CAVES$yearly_data$isot[[201]], CAVES$yearly_data$isot[[202]], CAVES$yearly_data$isot[[203]], CAVES$yearly_data$isot[[204]], CAVES$yearly_data$isot[[205]], CAVES$yearly_data$isot[[206]], CAVES$yearly_data$isot[[207]], CAVES$yearly_data$isot[[208]], CAVES$yearly_data$isot[[209]], CAVES$yearly_data$isot[[210]],
                                CAVES$yearly_data$isot[[211]], CAVES$yearly_data$isot[[212]]), nrow = 1150)

#Cross-Correlation Matrix
corr_matrix <- Hmisc::rcorr(prepare_corr_matrix)
remove(prepare_corr_matrix)

#corr_matrix2$r --> correlation
#corr_matrix2$p --> p-value

source("Functions/networkmap_simple2.R")

index_p_sim<-corr_matrix$P>0.1 # all that are not-significant, as they will later be that that are NA-ed
C_SIM<- corr_matrix$r
C_SIM_p <- C_SIM
C_SIM_p[index_p_sim] <- NA

## Network-Plot
networkmap_simple2(CMAT = C_SIM_p, 
                   lat = CAVES$site_info$latitude, 
                   lon = CAVES$site_info$longitude, 
                   title = "Correlation HadCM3 past millenium, sig level = 0.1", 
                   thresh = 0.15)

#LEGEND STILL MISSING

## Corr-Dist-Plot

dist_matrix <- matrix(nrow = dim(corr_matrix$r)[1], ncol = dim(corr_matrix$r)[2])

for (ii in 1:dim(dist_matrix)[1]){
  for (jj in 1:dim(dist_matrix)[2]){
    dist_matrix[ii,jj] <- fossil::deg.dist(CAVES$site_info$longitude[ii], CAVES$site_info$latitude[ii], CAVES$site_info$longitude[jj], CAVES$site_info$latitude[jj])
  }
}

dist_vec_sim <- as.vector(dist_matrix[upper.tri(dist_matrix)])
o <- order(dist_vec_sim)
dist_vec_sim_sorted <- dist_vec_sim[o]
corr_vec_sim <- as.vector(corr_matrix$r[upper.tri(corr_matrix$r)])
corr_vec_sim_sorted <- corr_vec_sim[o]

every_nth <- c(rep(c(TRUE, rep(FALSE, 9)),2236), TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)

plot(dist_vec_sim_sorted[every_nth], corr_vec_sim_sorted[every_nth], 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sim_sorted, corr_vec_sim_sorted, f = 1/5), lwd = 4, col = "#B2182B")


#################################################
##CROSS CORELATION FROM SISAL DATABASE###########
#################################################
#read in data from SISAL first!

time_caves_ed=seq(from = 0, to = 1000, by = 1)

ed_matrix_sample_min50 <- matrix(nrow = 1001, ncol = length(sites_used)+1)
colnames(ed_matrix_sample_min50) <- c("time", sites_used)
ed_matrix_sample_min50[,1] = time_caves_ed


for (ii in 1:length(sites_used)){
  s <- sample_final%>%filter(entity_id == sites_used[ii])
  ed_matrix_sample_min50[,ii+1] <- PaleoSpec::MakeEquidistant(s$interp_age, s$d18O_measurement, dt = 1, time_caves_ed)
  remove(s)
}


C_SISAL<-matrix(NA,nrow=length(sites_used),ncol=length(sites_used))
colnames(C_SISAL)<-rownames(C_SISAL)<-names(sites_used)
P_SISAL<-C_SISAL

for (ii in 1:(length(sites_used)-1)){
  print(ii)
  for (jj in (ii+1):length(sites_used)){
    #print(ed_matrix_sample_min50[,ii+1])
    x<- zoo(ed_matrix_sample_min50[,ii+1], order.by = time_caves_ed)
    y<- zoo(ed_matrix_sample_min50[,jj+1], order.by = time_caves_ed)
    temp<-nexcf_ci(x,y,conflevel=0.1)
    
    C_SISAL[ii,jj]<-temp$rxy
    P_SISAL[ii,jj]<-P_SISAL[jj,ii]<-temp$pval
    C_SISAL[jj,ii] = C_SISAL[ii,jj]
    rm(temp)
  }
}


site_ids_used = c()
index_p_sisal <- P_SISAL>0.1
for(ii in 1:length(sites_used)){
  s <- site_tb %>% filter(entity_id == sites_used[ii]) %>% distinct(site_id, entity_id)
  #if (index_p[ii] == TRUE){
  site_ids_used = c(site_ids_used, s$site_id)
  #}
  remove(s)
}
C_SIG <- C_SISAL
C_SIG[index_p_sisal] <- NA

networkmap_simple2(CMAT = C_SIG, 
                   lat = CAVES$site_info$latitude[site_ids_used], 
                   lon = CAVES$site_info$longitude[site_ids_used], 
                   title = "Correlation SISAL past millenium, sig level = 0.1",
                   thresh = 0.2)

## Corr-Dist-Plot

dist_matrix_sisal <- matrix(nrow = dim(C_SISAL)[1], ncol = dim(C_SISAL)[2])

for (ii in 1:dim(dist_matrix_sisal)[1]){
  for (jj in 1:dim(dist_matrix_sisal)[2]){
    dist_matrix_sisal[ii,jj] <- fossil::deg.dist(CAVES$site_info$longitude[site_ids_used[ii]], 
                                                 CAVES$site_info$latitude[site_ids_used[ii]], 
                                                 CAVES$site_info$longitude[site_ids_used[jj]], 
                                                 CAVES$site_info$latitude[site_ids_used[jj]])
  }
}

dist_vec_sisal <- as.vector(dist_matrix_sisal[upper.tri(dist_matrix_sisal)])
o <- order(dist_vec_sisal)
dist_vec_sisal_sorted <- dist_vec_sisal[o]
corr_vec_sisal <- as.vector(C_SISAL[upper.tri(C_SISAL)])
corr_vec_sisal_sorted <- corr_vec_sisal[o]

every_nth <- c(rep(c(TRUE, rep(FALSE, 9)),2236), TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)

plot(dist_vec_sisal_sorted, corr_vec_sisal_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "Distance between pairs", 
     cex = 1, 
     lwd = 1)
lines(lowess(dist_vec_sisal_sorted, corr_vec_sisal_sorted, f=1/5), lwd = 4, col = "#B2182B")

#################################################
##MULTIPLOT######################################
#################################################

# Only possible after all above calculations have been done
outdir = "/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_SA_Poster/Plots/Network/"
scaling = 1.5
spacing = 0.7
pdf(file = paste(outdir,"network_sisal_vs_hadcm3_p01_c015.pdf",sep=""), height= scaling*5, width = scaling*6)
par(mfrow=c(2,2), mai = c(rep(spacing, 4)), mar = c(3,3,2,0.5))
#SIM MAP
networkmap_simple3(CMAT = C_SIM_p, 
                   lat = CAVES$site_info$latitude, 
                   lon = CAVES$site_info$longitude, 
                   title = "Corr HadCM3 past millenium, p<0.1", 
                   thresh = 0.15)
#SIM Cor-Dist
plot(dist_vec_sim_sorted[every_nth], corr_vec_sim_sorted[every_nth], 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "", 
     cex = 1, 
     lwd = 0.5)
lines(lowess(dist_vec_sim_sorted, corr_vec_sim_sorted, f = 1/5), lwd = 4, col = "#B2182B")
mtext("Distance between pairs (km)", side= 1, line = 2)

#SISAL MAP
networkmap_simple3(CMAT = C_SIG, 
                   lat = CAVES$site_info$latitude[site_ids_used], 
                   lon = CAVES$site_info$longitude[site_ids_used], 
                   title = "Corr SISAL past millenium, p<0.1",
                   thresh = 0.2)
#SISAL Cor-Dist
plot(dist_vec_sisal_sorted, corr_vec_sisal_sorted, 
     ylim = c(-1,1),
     xlim = c(0,20000),
     ylab = "",
     xlab = "", 
     cex = 1, 
     lwd = 1)
lines(lowess(dist_vec_sisal_sorted, corr_vec_sisal_sorted, f=1/5), lwd = 4, col = "#B2182B")
mtext("Distance between pairs (km)", side= 1, line = 2)
dev.off()




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
outdir = "/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_SA_Poster/Plots/Cor_Matrix/"

kira_prep_matrix <- matrix(c(CAVES$yearly_data$isot[[94]],
                             CAVES$yearly_data$isot[[107]],
                             CAVES$yearly_data$isot[[104]],
                             CAVES$yearly_data$isot[[136]],   
                             CAVES$yearly_data$isot[[141]],   
                             CAVES$yearly_data$isot[[151]],   
                             CAVES$yearly_data$isot[[179]],   
                             CAVES$yearly_data$isot[[27]],   
                             CAVES$yearly_data$isot[[183]],
                             CAVES$yearly_data$isot[[4]]), nrow = 1150)

col2 <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                               "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                               "#4393C3", "#2166AC", "#053061")))

kira_corr_matrix <- Hmisc::rcorr(kira_prep_matrix)
C<-kira_corr_matrix$r
P<-kira_corr_matrix$P
colnames(C)<- rownames(C)<-c("188", "209", "226", "286", "305", "326", "390", "97", "443", "33")

pdf(file = paste(outdir,"combined_matrix_colr_sim.pdf",sep=""))
corrplot(C, type ="full", p.mat = kira_corr_matrix$P, sig.level = 0.1, diag = FALSE, is.corr = TRUE, col = col2(200))
dev.off()

pdf(file = paste(outdir,"combined_matrix_number_sim.pdf",sep=""))
corrplot(C,type="full",p.mat=P,sig.level = 0.1,diag=FALSE,is.corr=TRUE,col=col2(200),method="number")
dev.off()