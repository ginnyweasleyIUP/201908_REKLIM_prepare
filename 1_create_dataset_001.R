########Main#####################################
##
## This Script creates the dataset for the 40 extracted
## caves from Bakers paper.
## This script is brute force only for 40 random caves
## It has to be modified more neatly to be able to handle
## any amount of caves as an input
##
#################################################



#0) load librarys and source functions
source('0_init_library.R', echo=FALSE)
source('0_init_source.R', echo=FALSE)

#1) the cave data needs to be read in############
cave_data = read.csv("site_bearbeitet.csv")

#3) read in data from nc-files###################
# Neu exportiert werden müssen: 14,23,24,29,30,31,32,35,36,40
for (ii in 1:5){
  data_name_temp = paste("sisal_ncdata/xnapa_cave", ii, "_temp.nc", sep = "")
  data_name_prec = paste("sisal_ncdata/xnapa_cave", ii, "_prec.nc", sep = "")
  data_name_isot = paste("sisal_ncdata/xnapa_cave", ii, "_isot.nc", sep = "")
  
  #time is extracted from temperature
  data_xnap_temp <- ncdf4::nc_open(data_name_temp)
  data_temp <- ncdf4::ncvar_get(data_xnap_temp)
  nam <- paste("cave", ii, "_temp", sep = "")
  nam_t <- paste("cave", ii, "_time", sep = "")
  assign(nam, data_temp[1:13811])
  assign(nam_t, data_xnap_temp$dim$t$vals[1:13811])
  ncdf4::nc_close(data_xnap_temp)
  remove(data_xnap_temp)
  
  data_xnap_prec <- ncdf4::nc_open(data_name_prec)
  data_prec <- ncdf4::ncvar_get(data_xnap_prec)
  nam <- paste("cave", ii, "_prec", sep = "")
  assign(nam, data_prec[1:13811])
  ncdf4::nc_close(data_xnap_prec)
  remove(data_xnap_prec)
  
  data_xnap_isot <- ncdf4::nc_open(data_name_isot)
  data_isot <- ncdf4::ncvar_get(data_xnap_isot, data_xnap_isot$var[[3]])
  nam <- paste("cave", ii, "_isot", sep = "")
  assign(nam, data_isot[1:13811])
  ncdf4::nc_close(data_xnap_isot)
  remove(data_xnap_isot)
  
  remove(data_name_isot, data_name_prec, data_name_temp)
  remove(nam)
  remove(nam_t)
  
} 
remove(ii)

remove(data_isot)
remove(data_prec)
remove(data_temp)

##CLEAR DATA#####################################
#Clear data for any NAs or any too high or too low values

for (ii in 1:5){
  nam <- paste("cave", ii, "_time", sep = "")
  assign(nam, clear_data(get(paste('cave',ii,'_time', sep='')), 1))
  nam <- paste("cave", ii, "_temp", sep = "")
  assign(nam, clear_data(get(paste('cave',ii,'_temp', sep='')), 2))
  nam <- paste("cave", ii, "_prec", sep = "")
  assign(nam, clear_data(get(paste('cave',ii,'_prec', sep='')), 3))
  nam <- paste("cave", ii, "_isot", sep = "")
  assign(nam, clear_data(get(paste('cave',ii,'_isot', sep='')), 4))
}
remove(ii, nam)


##Data-Structure#################################

CAVES <- CAVES_dataset$new()

CAVES$raw_data$time= tibble(time1 = cave1_time, time2 = cave2_time, time3 = cave3_time, time4 = cave4_time, time5 = cave5_time)
CAVES$raw_data$temp= tibble(temp1 = cave1_temp, temp2 = cave2_temp, temp3 = cave3_temp, temp4 = cave4_temp, temp5 = cave5_temp)
CAVES$raw_data$prec= tibble(prec1 = cave1_prec, prec2 = cave2_prec, prec3 = cave3_prec, prec4 = cave4_prec, prec5 = cave5_prec)
CAVES$raw_data$isot= tibble(isot1 = cave1_isot, isot2 = cave2_isot, isot3 = cave3_isot, isot4 = cave4_isot, isot5 = cave5_isot)

CAVES$raw_data = tibble(cave1 =tibble(time = cave1_time,  temp = cave1_temp,  prec = cave1_prec,  isot = cave1_isot ),
                        cave2 =tibble(time = cave2_time,  temp = cave2_temp,  prec = cave2_prec,  isot = cave2_isot ),
                        cave3 =tibble(time = cave3_time,  temp = cave3_temp,  prec = cave3_prec,  isot = cave3_isot ),
                        cave4 =tibble(time = cave4_time,  temp = cave4_temp,  prec = cave4_prec,  isot = cave4_isot ),
                        cave5 =tibble(time = cave5_time,  temp = cave5_temp,  prec = cave5_prec,  isot = cave5_isot ),
                        cave6 =tibble(time = cave6_time,  temp = cave6_temp,  prec = cave6_prec,  isot = cave6_isot ),
                        cave7 =tibble(time = cave7_time,  temp = cave7_temp,  prec = cave7_prec,  isot = cave7_isot ),
                        cave8 =tibble(time = cave8_time,  temp = cave8_temp,  prec = cave8_prec,  isot = cave8_isot ),
                        cave9 =tibble(time = cave9_time,  temp = cave9_temp,  prec = cave9_prec,  isot = cave9_isot ),
                        cave10=tibble(time = cave10_time, temp = cave10_temp, prec = cave10_prec, isot = cave10_isot),
                        cave11=tibble(time = cave11_time, temp = cave11_temp, prec = cave11_prec, isot = cave11_isot),
                        cave12=tibble(time = cave12_time, temp = cave12_temp, prec = cave12_prec, isot = cave12_isot),
                        cave13=tibble(time = cave13_time, temp = cave13_temp, prec = cave13_prec, isot = cave13_isot),
                        cave14=tibble(time = cave14_time, temp = cave14_temp, prec = cave14_prec, isot = cave14_isot),
                        cave15=tibble(time = cave15_time, temp = cave15_temp, prec = cave15_prec, isot = cave15_isot),
                        cave16=tibble(time = cave16_time, temp = cave16_temp, prec = cave16_prec, isot = cave16_isot),
                        cave17=tibble(time = cave17_time, temp = cave17_temp, prec = cave17_prec, isot = cave17_isot),
                        cave18=tibble(time = cave18_time, temp = cave18_temp, prec = cave18_prec, isot = cave18_isot),
                        cave19=tibble(time = cave19_time, temp = cave19_temp, prec = cave19_prec, isot = cave19_isot),
                        cave20=tibble(time = cave20_time, temp = cave20_temp, prec = cave20_prec, isot = cave20_isot),
                        cave21=tibble(time = cave21_time, temp = cave21_temp, prec = cave21_prec, isot = cave21_isot),
                        cave22=tibble(time = cave22_time, temp = cave22_temp, prec = cave22_prec, isot = cave22_isot),
                        cave23=tibble(time = cave23_time, temp = cave23_temp, prec = cave23_prec, isot = cave23_isot),
                        cave24=tibble(time = cave24_time, temp = cave24_temp, prec = cave24_prec, isot = cave24_isot),
                        cave25=tibble(time = cave25_time, temp = cave25_temp, prec = cave25_prec, isot = cave25_isot),
                        cave26=tibble(time = cave26_time, temp = cave26_temp, prec = cave26_prec, isot = cave26_isot),
                        cave27=tibble(time = cave27_time, temp = cave27_temp, prec = cave27_prec, isot = cave27_isot),
                        cave28=tibble(time = cave28_time, temp = cave28_temp, prec = cave28_prec, isot = cave28_isot),
                        cave29=tibble(time = cave29_time, temp = cave29_temp, prec = cave29_prec, isot = cave29_isot),
                        cave30=tibble(time = cave30_time, temp = cave30_temp, prec = cave30_prec, isot = cave30_isot),
                        cave31=tibble(time = cave31_time, temp = cave31_temp, prec = cave31_prec, isot = cave31_isot),
                        cave32=tibble(time = cave32_time, temp = cave32_temp, prec = cave32_prec, isot = cave32_isot),
                        cave33=tibble(time = cave33_time, temp = cave33_temp, prec = cave33_prec, isot = cave33_isot),
                        cave34=tibble(time = cave34_time, temp = cave34_temp, prec = cave34_prec, isot = cave34_isot),
                        cave35=tibble(time = cave35_time, temp = cave35_temp, prec = cave35_prec, isot = cave35_isot),
                        cave36=tibble(time = cave36_time, temp = cave36_temp, prec = cave36_prec, isot = cave36_isot),
                        cave37=tibble(time = cave37_time, temp = cave37_temp, prec = cave37_prec, isot = cave37_isot),
                        cave38=tibble(time = cave38_time, temp = cave38_temp, prec = cave38_prec, isot = cave38_isot),
                        cave39=tibble(time = cave39_time, temp = cave39_temp, prec = cave39_prec, isot = cave39_isot),
                        cave40=tibble(time = cave40_time, temp = cave40_temp, prec = cave40_prec, isot = cave40_isot),
                        cave51=tibble(time = cave51_time, temp = cave51_temp, prec = cave51_prec, isot = cave51_isot),
                        cave52=tibble(time = cave52_time, temp = cave52_temp, prec = cave52_prec, isot = cave52_isot),
                        cave53=tibble(time = cave53_time, temp = cave53_temp, prec = cave53_prec, isot = cave53_isot),
                        cave54=tibble(time = cave54_time, temp = cave54_temp, prec = cave54_prec, isot = cave54_isot),
                        cave55=tibble(time = cave55_time, temp = cave55_temp, prec = cave55_prec, isot = cave55_isot),
                        cave56=tibble(time = cave56_time, temp = cave56_temp, prec = cave56_prec, isot = cave56_isot),
                        cave57=tibble(time = cave57_time, temp = cave57_temp, prec = cave57_prec, isot = cave57_isot),
                        cave58=tibble(time = cave58_time, temp = cave58_temp, prec = cave58_prec, isot = cave58_isot),
                        cave59=tibble(time = cave59_time, temp = cave59_temp, prec = cave59_prec, isot = cave59_isot),
                        cave60=tibble(time = cave60_time, temp = cave60_temp, prec = cave60_prec, isot = cave60_isot),
                        cave71=tibble(time = cave71_time, temp = cave71_temp, prec = cave71_prec, isot = cave71_isot),
                        cave72=tibble(time = cave72_time, temp = cave72_temp, prec = cave72_prec, isot = cave72_isot),
                        cave73=tibble(time = cave73_time, temp = cave73_temp, prec = cave73_prec, isot = cave73_isot),
                        cave74=tibble(time = cave74_time, temp = cave74_temp, prec = cave74_prec, isot = cave74_isot),
                        cave75=tibble(time = cave75_time, temp = cave75_temp, prec = cave75_prec, isot = cave75_isot),
                        cave76=tibble(time = cave76_time, temp = cave76_temp, prec = cave76_prec, isot = cave76_isot),
                        cave77=tibble(time = cave77_time, temp = cave77_temp, prec = cave77_prec, isot = cave77_isot),
                        cave78=tibble(time = cave78_time, temp = cave78_temp, prec = cave78_prec, isot = cave78_isot),
                        cave79=tibble(time = cave79_time, temp = cave79_temp, prec = cave79_prec, isot = cave79_isot),
                        cave80=tibble(time = cave80_time, temp = cave80_temp, prec = cave80_prec, isot = cave80_isot),
                        cave81=tibble(time = cave81_time, temp = cave81_temp, prec = cave81_prec, isot = cave81_isot),
                        cave82=tibble(time = cave82_time, temp = cave82_temp, prec = cave82_prec, isot = cave82_isot),
                        cave83=tibble(time = cave83_time, temp = cave83_temp, prec = cave83_prec, isot = cave83_isot),
                        cave84=tibble(time = cave84_time, temp = cave84_temp, prec = cave84_prec, isot = cave84_isot),
                        cave85=tibble(time = cave85_time, temp = cave85_temp, prec = cave85_prec, isot = cave85_isot),
                        cave86=tibble(time = cave86_time, temp = cave86_temp, prec = cave86_prec, isot = cave86_isot),
                        cave87=tibble(time = cave87_time, temp = cave87_temp, prec = cave87_prec, isot = cave87_isot),
                        cave88=tibble(time = cave88_time, temp = cave88_temp, prec = cave88_prec, isot = cave88_isot),
                        cave89=tibble(time = cave89_time, temp = cave89_temp, prec = cave89_prec, isot = cave89_isot),
                        cave90=tibble(time = cave90_time, temp = cave90_temp, prec = cave90_prec, isot = cave90_isot),
                        cave91=tibble(time = cave91_time, temp = cave91_temp, prec = cave91_prec, isot = cave91_isot),
                        cave92=tibble(time = cave92_time, temp = cave92_temp, prec = cave92_prec, isot = cave92_isot),
                        cave93=tibble(time = cave93_time, temp = cave93_temp, prec = cave93_prec, isot = cave93_isot),
                        cave94=tibble(time = cave94_time, temp = cave94_temp, prec = cave94_prec, isot = cave94_isot),
                        cave95=tibble(time = cave95_time, temp = cave95_temp, prec = cave95_prec, isot = cave95_isot),
                        cave96=tibble(time = cave96_time, temp = cave96_temp, prec = cave96_prec, isot = cave96_isot),
                        cave97=tibble(time = cave97_time, temp = cave97_temp, prec = cave97_prec, isot = cave97_isot),
                        cave98=tibble(time = cave98_time, temp = cave98_temp, prec = cave98_prec, isot = cave98_isot),
                        cave99=tibble(time = cave99_time, temp = cave99_temp, prec = cave99_prec, isot = cave99_isot),
                        cave100=tibble(time = cave100_time, temp = cave100_temp, prec = cave100_prec, isot = cave100_isot),
                        cave101=tibble(time = cave101_time, temp = cave101_temp, prec = cave101_prec, isot = cave101_isot),
                        cave102=tibble(time = cave102_time, temp = cave102_temp, prec = cave102_prec, isot = cave102_isot),
                        cave103=tibble(time = cave103_time, temp = cave103_temp, prec = cave103_prec, isot = cave103_isot),
                        cave104=tibble(time = cave104_time, temp = cave104_temp, prec = cave104_prec, isot = cave104_isot),
                        cave105=tibble(time = cave105_time, temp = cave105_temp, prec = cave105_prec, isot = cave105_isot),
                        cave106=tibble(time = cave106_time, temp = cave106_temp, prec = cave106_prec, isot = cave106_isot),
                        cave107=tibble(time = cave107_time, temp = cave107_temp, prec = cave107_prec, isot = cave107_isot),
                        cave108=tibble(time = cave108_time, temp = cave108_temp, prec = cave108_prec, isot = cave108_isot),
                        cave109=tibble(time = cave109_time, temp = cave109_temp, prec = cave109_prec, isot = cave109_isot),
                        cave110=tibble(time = cave110_time, temp = cave110_temp, prec = cave110_prec, isot = cave110_isot),
                        cave111=tibble(time = cave111_time, temp = cave111_temp, prec = cave111_prec, isot = cave111_isot),
                        cave112=tibble(time = cave112_time, temp = cave112_temp, prec = cave112_prec, isot = cave112_isot),
                        cave113=tibble(time = cave113_time, temp = cave113_temp, prec = cave113_prec, isot = cave113_isot),
                        cave114=tibble(time = cave114_time, temp = cave114_temp, prec = cave114_prec, isot = cave114_isot),
                        cave115=tibble(time = cave115_time, temp = cave115_temp, prec = cave115_prec, isot = cave115_isot),
                        cave116=tibble(time = cave116_time, temp = cave116_temp, prec = cave116_prec, isot = cave116_isot),
                        cave117=tibble(time = cave117_time, temp = cave117_temp, prec = cave117_prec, isot = cave117_isot),
                        cave118=tibble(time = cave118_time, temp = cave118_temp, prec = cave118_prec, isot = cave118_isot),
                        cave119=tibble(time = cave119_time, temp = cave119_temp, prec = cave119_prec, isot = cave119_isot),
                        cave120=tibble(time = cave120_time, temp = cave120_temp, prec = cave120_prec, isot = cave120_isot),
                        cave121=tibble(time = cave121_time, temp = cave121_temp, prec = cave121_prec, isot = cave121_isot),
                        cave122=tibble(time = cave122_time, temp = cave122_temp, prec = cave122_prec, isot = cave122_isot),
                        cave123=tibble(time = cave123_time, temp = cave123_temp, prec = cave123_prec, isot = cave123_isot),
                        cave124=tibble(time = cave124_time, temp = cave124_temp, prec = cave124_prec, isot = cave124_isot),
                        cave125=tibble(time = cave125_time, temp = cave125_temp, prec = cave125_prec, isot = cave125_isot),
                        cave126=tibble(time = cave126_time, temp = cave126_temp, prec = cave126_prec, isot = cave126_isot),
                        cave127=tibble(time = cave127_time, temp = cave127_temp, prec = cave127_prec, isot = cave127_isot),
                        cave128=tibble(time = cave128_time, temp = cave128_temp, prec = cave128_prec, isot = cave128_isot),
                        cave129=tibble(time = cave129_time, temp = cave129_temp, prec = cave129_prec, isot = cave129_isot),
                        cave130=tibble(time = cave130_time, temp = cave130_temp, prec = cave130_prec, isot = cave130_isot),
                        cave131=tibble(time = cave131_time, temp = cave131_temp, prec = cave131_prec, isot = cave131_isot),
                        cave132=tibble(time = cave132_time, temp = cave132_temp, prec = cave132_prec, isot = cave132_isot),
                        cave133=tibble(time = cave133_time, temp = cave133_temp, prec = cave133_prec, isot = cave133_isot),
                        cave134=tibble(time = cave134_time, temp = cave134_temp, prec = cave134_prec, isot = cave134_isot),
                        cave135=tibble(time = cave135_time, temp = cave135_temp, prec = cave135_prec, isot = cave135_isot),
                        cave136=tibble(time = cave136_time, temp = cave136_temp, prec = cave136_prec, isot = cave136_isot),
                        cave137=tibble(time = cave137_time, temp = cave137_temp, prec = cave137_prec, isot = cave137_isot),
                        cave138=tibble(time = cave138_time, temp = cave138_temp, prec = cave138_prec, isot = cave138_isot),
                        cave139=tibble(time = cave139_time, temp = cave139_temp, prec = cave139_prec, isot = cave139_isot),
                        cave140=tibble(time = cave140_time, temp = cave140_temp, prec = cave140_prec, isot = cave140_isot),
                        cave151=tibble(time = cave151_time, temp = cave151_temp, prec = cave151_prec, isot = cave151_isot),
                        cave152=tibble(time = cave152_time, temp = cave152_temp, prec = cave152_prec, isot = cave152_isot),
                        cave153=tibble(time = cave153_time, temp = cave153_temp, prec = cave153_prec, isot = cave153_isot),
                        cave154=tibble(time = cave154_time, temp = cave154_temp, prec = cave154_prec, isot = cave154_isot),
                        cave155=tibble(time = cave155_time, temp = cave155_temp, prec = cave155_prec, isot = cave155_isot),
                        cave156=tibble(time = cave156_time, temp = cave156_temp, prec = cave156_prec, isot = cave156_isot),
                        cave157=tibble(time = cave157_time, temp = cave157_temp, prec = cave157_prec, isot = cave157_isot),
                        cave158=tibble(time = cave158_time, temp = cave158_temp, prec = cave158_prec, isot = cave158_isot),
                        cave159=tibble(time = cave159_time, temp = cave159_temp, prec = cave159_prec, isot = cave159_isot),
                        cave160=tibble(time = cave160_time, temp = cave160_temp, prec = cave160_prec, isot = cave160_isot),
                        cave171=tibble(time = cave171_time, temp = cave171_temp, prec = cave171_prec, isot = cave171_isot),
                        cave172=tibble(time = cave172_time, temp = cave172_temp, prec = cave172_prec, isot = cave172_isot),
                        cave173=tibble(time = cave173_time, temp = cave173_temp, prec = cave173_prec, isot = cave173_isot),
                        cave174=tibble(time = cave174_time, temp = cave174_temp, prec = cave174_prec, isot = cave174_isot)
                        )

CAVES$reg_data = tibble(site_id = 1:174)
CAVES$corr_data = tibble(site_id = 1:174)

##Delete Cave Values#############################
for (ii in 1:174){
  removing_data = (paste('cave',ii,'_temp', sep=''))
  remove(list = removing_data)
  removing_data = (paste('cave',ii,'_prec', sep=''))
  remove(list = removing_data)
  removing_data = (paste('cave',ii,'_isot', sep=''))
  remove(list = removing_data)
}
remove(removing_data)

##Mean,Min,Max Data##############################
#Add Mean Data and Data Range to site_info for better plotting later
data_mean_temp = c()
data_max_temp = c()
data_min_temp = c()
data_mean_prec = c()
data_max_prec = c()
data_min_prec = c()
data_mean_isot = c()
data_max_isot = c()
data_min_isot = c()

for (ii in 1:174){
  data_mean_temp = c(data_mean_temp, mean(CAVES$raw_data[[ii]]$temp))
  data_min_temp  = c(data_min_temp, min(CAVES$raw_data[[ii]]$temp))
  data_max_temp  = c(data_max_temp, min(CAVES$raw_data[[ii]]$temp))
  
  data_mean_prec = c(data_mean_prec, mean(CAVES$raw_data[[ii]]$prec))
  data_min_prec  = c(data_min_prec, min(CAVES$raw_data[[ii]]$prec))
  data_max_prec  = c(data_max_prec, min(CAVES$raw_data[[ii]]$prec))
  
  data_mean_isot = c(data_mean_isot, mean(CAVES$raw_data[[ii]]$isot))
  data_min_isot  = c(data_min_isot, min(CAVES$raw_data[[ii]]$isot))
  data_max_isot  = c(data_max_isot, min(CAVES$raw_data[[ii]]$isot))
}

CAVES$site_info$mean_temp = data_mean_temp
CAVES$site_info$max_temp = data_max_temp
CAVES$site_info$min_temp = data_min_temp
CAVES$site_info$mean_prec = data_mean_prec
CAVES$site_info$max_prec = data_max_prec
CAVES$site_info$min_prec = data_min_prec
CAVES$site_info$mean_isot = data_mean_isot
CAVES$site_info$max_isot = data_max_isot
CAVES$site_info$min_isot = data_min_isot

remove(data_mean_temp, data_max_temp, data_min_temp,
       data_mean_prec, data_max_prec, data_min_prec,
       data_mean_isot, data_max_isot, data_min_isot)


remove(ii)

sisal_data = read.csv("site_bearbeitet.csv")

text = sisal_data[1,13]

for (ii in 2:174){
  text = paste(text,", ", sisal_data[ii,13], sep = "")
}

