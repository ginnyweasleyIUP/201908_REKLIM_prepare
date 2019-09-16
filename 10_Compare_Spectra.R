#################################################
## COMPARE SPECTRA ##############################
#################################################

sites_to_entities_used = c()

for (entity in entities_used){
  sites_to_entities_used = c(sites_to_entities_used, sample_final %>% filter(entity_id == entity) %>% count(site_id) %>% pull(site_id))
}

##CREATE SPECTRA#################################

## And Plot immediately #########################

#TODO: Make ts object from it and remove Na such that all works!

for (ii in 1:length(entities_used)){
  name = paste0("ENTITY", entities_used[ii])
  spec_sim_full <- PaleoSpec::SpecMTM(CAVES$yearly_data$isot[[sites_to_entities_used[ii]]])
  spec_sim_down <- PaleoSpec::SpecMTM(PaleoSpec::MakeEquidistant(DATA_last1000_mean[[name]]$value_record_age,
                                                                 DATA_last1000_mean[[name]]$value_sim_d18O,
                                                                 time.target = seq(from = -49, to = 1100)))
  spec_rec      <- PaleoSpec::SpecMTM(PaleoSpec::MakeEquidistant(DATA_last1000_mean[[name]]$value_record_age,
                                                                 DATA_last1000_mean[[name]]$value_record_d18O,
                                                                 time.target = seq(from = -49, to = 1100)))
  yrang<-range(spec_sim_full$spec,spec_rec$spec)
  xrang<-range(spec_sim_full$freq,spec_rec$freq)
  
  STACYRED  = "#B5123E"
  LIGHTBLUE = '#3E92CC'
  DARKBLUE = '#0A2463'
  GREY = '#808080'
  
  
  png(file = paste0("Spectra/PSD_ENTITY_", entities_used[ii], "_compare.png"), width = 600, height = 400)
  LPlot(LogSmooth(spec_sim_full), 
        lwd = 2, 
        col = DARKBLUE, 
        xlim = xrang, 
        ylim = c(1e-2,1e3), 
        main = "Compare Spectra of Sim and Record in diff. resolutions", 
        xlab = "Frequency in years", 
        ylab = "PSD", 
        xaxt = 'n')
  axis(side = 1, at = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.2, 0.5), 
       labels = c(1/0.001, 1/0.005, 1/0.01, 1/0.02, 1/0.05, 1/0.2, 1/0.5))
  LLines(LogSmooth(spec_sim_down), lwd = 2, col = LIGHTBLUE)
  LLines(LogSmooth(spec_rec), lwd = 2, col = STACYRED)
  LLines(spec_red,col= STACYRED)

  legend("topright",
         c(TeX("full res $\\delta^{18}O$ signal HadCM3"), TeX("down sampled HadCM3 signal"), "signal from SISAL database"),
         lty = c(1,1,1),
         col = c("DARKBLUE", "LIGHTBLUE", "STACYRED"))
  
  dev.off()
  
}




##PLOTS###################################################################

yrang<-range(spec_input$spec,spec_veg35$spec)
xrang<-range(spec_input$freq,spec_veg35$freq)

png(file = "PSD_tau5_vegcompare.png", width = 600, height = 400)


##Other possible colors: #################################################
#
# deep carmine:   #B5123E
# black:
# trolley grey:   #808080
# celestail blue: #3E92CC
# royal blue:     #0A2463
# cadet blue:     #56A3A6
# gainsboro:      #E2DADB
# kombu green:    #3E442B
# pineapple:      #5C415D
# sugar plum:     #8E5572
