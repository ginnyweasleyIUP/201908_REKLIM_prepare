########Main#####################################
##
## This Script creates the dataset for the 40 extracted
## caves from Bakers paper.
## This script is brute force only for 40 random caves
## It has to be modified more neatly to be able to handle
## any amount of caves as an input
##
#################################################



##0) load librarys and source functions##########
source('0_init_library.R', echo=FALSE)
source('0_init_source.R', echo=FALSE)

##1) the cave data needs to be read in###########
sisal_data = read.csv("site_bearbeitet.csv")
CAVES <- CAVES_dataset$new()
CAVES$raw_data$time = fill_raw_NA_time(13811)
CAVES$raw_data$temp = fill_raw_NA_temp(13811)
CAVES$raw_data$prec = fill_raw_NA_prec(13811)
CAVES$raw_data$isot = fill_raw_NA_isot(13811)

CAVES$yearly_data$time = fill_raw_NA_time(1150)
CAVES$yearly_data$temp = fill_raw_NA_temp(1150)
CAVES$yearly_data$prec = fill_raw_NA_prec(1150)
CAVES$yearly_data$isot = fill_raw_NA_isot(1150)

##2) read in data from nc-files##################
#Clear data for any NAs or any too high or too low values
for (ii in 1:212){
  data_name_temp = paste("sisal_ncdata/xnapa_cave", ii, "_temp.nc", sep = "")
  data_name_prec = paste("sisal_ncdata/xnapa_cave", ii, "_prec.nc", sep = "")
  data_name_isot = paste("sisal_ncdata/xnapa_cave", ii, "_isot.nc", sep = "")
  
  #time is extracted from temperature
  data_xnap_temp <- ncdf4::nc_open(data_name_temp)
  data_temp <- ncdf4::ncvar_get(data_xnap_temp)
  CAVES$raw_data$time[ii] <- clear_data(data_xnap_temp$dim$t$vals[1:13811], 1)
  CAVES$raw_data$temp[ii] <- clear_data(data_temp[1:13811], 2)
  ncdf4::nc_close(data_xnap_temp)
  remove(data_xnap_temp)
  
  data_xnap_prec <- ncdf4::nc_open(data_name_prec)
  data_prec <- ncdf4::ncvar_get(data_xnap_prec)
  CAVES$raw_data$prec[ii] <- clear_data(data_prec[1:13811], 3)
  ncdf4::nc_close(data_xnap_prec)
  remove(data_xnap_prec)
  
  data_xnap_isot <- ncdf4::nc_open(data_name_isot)
  data_isot <- ncdf4::ncvar_get(data_xnap_isot, data_xnap_isot$var[[3]])
  CAVES$raw_data$isot[ii] <- clear_data(data_isot[1:13811], 4)
  ncdf4::nc_close(data_xnap_isot)
  remove(data_xnap_isot)
  
  remove(data_name_isot, data_name_prec, data_name_temp)
  
} 
remove(ii)

remove(data_isot)
remove(data_prec)
remove(data_temp)

##3) Data-Structure##############################

CAVES$climate_info = tibble(site_id = 1:212)
CAVES$reg_data = tibble(site_id = 1:212)
CAVES$corr_data = tibble(site_id = 1:212)


##4) Mean,Min,Max Data###########################
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

for (ii in 1:212){
  data_mean_temp = c(data_mean_temp, mean(CAVES$raw_data$temp[[ii]]))
  data_min_temp  = c(data_min_temp,  min(CAVES$raw_data$temp[[ii]]))
  data_max_temp  = c(data_max_temp,  min(CAVES$raw_data$temp[[ii]]))
  
  data_mean_prec = c(data_mean_prec, mean(CAVES$raw_data$prec[[ii]]))
  data_min_prec  = c(data_min_prec,  min(CAVES$raw_data$prec[[ii]]))
  data_max_prec  = c(data_max_prec,  min(CAVES$raw_data$prec[[ii]]))
  
  data_mean_isot = c(data_mean_isot, mean(CAVES$raw_data$isot[[ii]]))
  data_min_isot  = c(data_min_isot,  min(CAVES$raw_data$isot[[ii]]))
  data_max_isot  = c(data_max_isot,  min(CAVES$raw_data$isot[[ii]]))
}

# Careful with 152: Hoti Cave --> calculation for d18O rubish as there is no precipitation in these months

CAVES$climate_info$mean_temp = data_mean_temp
CAVES$climate_info$max_temp = data_max_temp
CAVES$climate_info$min_temp = data_min_temp
CAVES$climate_info$mean_prec = data_mean_prec
CAVES$climate_info$max_prec = data_max_prec
CAVES$climate_info$min_prec = data_min_prec
CAVES$climate_info$mean_isot = data_mean_isot
CAVES$climate_info$max_isot = data_max_isot
CAVES$climate_info$min_isot = data_min_isot

remove(data_mean_temp, data_max_temp, data_min_temp,
       data_mean_prec, data_max_prec, data_min_prec,
       data_mean_isot, data_max_isot, data_min_isot)

##5) Create yearly date##########################
#################################################
# yearly data is important because for a correlation analysis
# we need deseasonalized data  -> Chatfield

for (ii in (1:212)){
  yearly_time_start = numeric(1150)
  yearly_temp_mean  = numeric(1150)
  yearly_prec_mean  = numeric(1150)
  yearly_isot_mean  = numeric(1150)
  
  for(jj in 1:1150){
    pos_start = 12*(jj-1)+1
    pos_stop  = 12*(jj-1)+12
    yearly_time_start[jj] = CAVES$raw_data$time[[ii]][pos_start]
    yearly_temp_mean[jj]  = mean(CAVES$raw_data$temp[[ii]][pos_start:pos_stop])
    yearly_prec_mean[jj]  = mean(CAVES$raw_data$prec[[ii]][pos_start:pos_stop])
    yearly_isot_mean[jj]  = mean(CAVES$raw_data$isot[[ii]][pos_start:pos_stop])
    
  }
  
  CAVES$yearly_data$time[ii] <- yearly_time_start
  CAVES$yearly_data$temp[ii] <- yearly_temp_mean
  CAVES$yearly_data$prec[ii] <- yearly_prec_mean
  CAVES$yearly_data$isot[ii] <- yearly_isot_mean
}

remove(yearly_time_start, yearly_temp_mean, yearly_prec_mean, yearly_isot_mean)

remove(ii)

