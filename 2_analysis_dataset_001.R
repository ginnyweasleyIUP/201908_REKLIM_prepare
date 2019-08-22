#################################################
##REGRESSION AND CORRELATION ANALYSIS############
#################################################

##1) Create Dataset via 1_create_dataset.R#######
#source("0_init_library.R")
#source("0_init_source.R")
source("1_create_dataset_001.R")

##2) REGRESSION ANALYSIS#########################
###a)Regression btw temp and prec################
###b)Regression btw temp and isot################
###c)Regression btw prec and isot################

## All holds for Regression within the cave
## Linear regression after y = alpha*x + beta with r^2 --> only r^2>0.5 is ok to explain phaenomena

data_reg_temp_prec_alpha = numeric(212)
data_reg_temp_prec_beta  = numeric(212)
data_reg_temp_prec_rsqared = numeric(212)
data_reg_temp_isot_alpha = numeric(212)
data_reg_temp_isot_beta  = numeric(212)
data_reg_temp_isot_rsqared = numeric(212)
data_reg_prec_isot_alpha = numeric(212)
data_reg_prec_isot_beta  = numeric(212)
data_reg_prec_isot_rsqared = numeric(212)

for (ii in 1:212){
  Reg1 = lm(CAVES$yearly_data$prec[[ii]] ~ CAVES$yearly_data$temp[[ii]])
  data_reg_temp_prec_alpha[ii]  = Reg1$coefficients[[2]]
  data_reg_temp_prec_beta[ii]   = Reg1$coefficients[[1]]
  data_reg_temp_prec_rsqared[ii] = summary(Reg1)$r.squared
  
  Reg2 = lm(CAVES$yearly_data$isot[[ii]] ~ CAVES$yearly_data$temp[[ii]])
  data_reg_temp_isot_alpha[ii]   = Reg2$coefficients[[2]]
  data_reg_temp_isot_beta[ii]    = Reg2$coefficients[[1]]
  data_reg_temp_isot_rsqared[ii] = summary(Reg2)$r.squared
  
  Reg3 = lm(CAVES$yearly_data$isot[[ii]] ~ CAVES$yearly_data$prec[[ii]])
  data_reg_prec_isot_alpha[ii]   = Reg3$coefficients[[2]]
  data_reg_prec_isot_beta[ii]    = Reg3$coefficients[[1]]
  data_reg_prec_isot_rsqared[ii] = summary(Reg3)$r.squared
  
  remove(Reg1, Reg2, Reg3)
}



CAVES$reg_data$temp_prec_alpha = data_reg_temp_prec_alpha
CAVES$reg_data$temp_prec_beta  = data_reg_temp_prec_beta
CAVES$reg_data$temp_prec_rsqared = data_reg_temp_prec_rsqared
CAVES$reg_data$temp_isot_alpha = data_reg_temp_isot_alpha
CAVES$reg_data$temp_isot_beta  = data_reg_temp_isot_beta
CAVES$reg_data$temp_isot_rsquared = data_reg_temp_isot_rsqared
CAVES$reg_data$prec_isot_alpha = data_reg_prec_isot_alpha
CAVES$reg_data$prec_isot_beta  = data_reg_prec_isot_beta
CAVES$reg_data$prec_isot_rsquared = data_reg_prec_isot_rsqared

remove(data_reg_temp_prec_alpha,
       data_reg_temp_prec_beta,
       data_reg_temp_prec_rsqared,
       data_reg_temp_isot_alpha,
       data_reg_temp_isot_beta,
       data_reg_temp_isot_rsqared,
       data_reg_prec_isot_alpha,
       data_reg_prec_isot_beta, 
       data_reg_prec_isot_rsqared)

##3) Correlation ANALYSIS########################
###a)Correlation btw temp and prec###############
###b)Correlation btw temp and isot###############
###c)Correlation btw prec and isot###############

## All holds for Correlation within the cave
## Linear regression after y = alpha*x + beta

data_corr_temp_prec = numeric(212)
data_corr_temp_prec_p = numeric(212)
data_corr_temp_isot = numeric(212)
data_corr_temp_isot_p = numeric(212)
data_corr_prec_isot = numeric(212)
data_corr_prec_isot_p =numeric(212)

for (ii in 1:212){
  Cor1 = cor.test(CAVES$yearly_data$temp[[ii]], CAVES$yearly_data$prec[[ii]])
  data_corr_temp_prec[ii] = Cor1$estimate[[1]]
  data_corr_temp_prec_p[ii] = Cor1$p.value
  
  Cor2 = cor.test(CAVES$yearly_data$temp[[ii]], CAVES$yearly_data$isot[[ii]])
  data_corr_temp_isot[ii] = Cor2$estimate[[1]]
  data_corr_temp_isot_p[ii] = Cor2$p.value
  
  Cor3 = cor.test(CAVES$yearly_data$prec[[ii]], CAVES$yearly_data$isot[[ii]])
  data_corr_prec_isot[ii] = Cor3$estimate[[1]]
  data_corr_prec_isot_p[ii] = Cor3$p.value
  
  remove(Cor1, Cor2, Cor3)
}



CAVES$corr_data$temp_prec = data_corr_temp_prec
CAVES$corr_data$temp_prec_p = data_corr_temp_prec_p
CAVES$corr_data$temp_isot = data_corr_temp_isot
CAVES$corr_data$temp_isot_p = data_corr_temp_isot_p
CAVES$corr_data$prec_isot = data_corr_prec_isot
CAVES$corr_data$prec_isot_p = data_corr_prec_isot_p

remove(data_corr_temp_prec,
       data_corr_temp_prec_p,
       data_corr_temp_isot,
       data_corr_temp_isot_p,
       data_corr_prec_isot,
       data_corr_prec_isot_p)
remove(ii)
remove(pos_start, pos_stop)

#################################################
##4) Cross-Correlation ##########################
#(Correlation btw isotopy of different caves)####
#################################################

#--> migrated to 5_network_plot

       
## At the end --> create plotting file