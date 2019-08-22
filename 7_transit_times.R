######################MAIN SPELEO FILTER################################
## Version 002
## Include Dees Borneo Cave Data to be able to compare later
##
##
##INPUT FUNCTIONS#########
source('Functions/Filter/bwf_filter.R')
source('Functions/Filter/EASY_Sensor_WM3.R')
source('Functions/Filter/filter_function3.R')

##INPUT VARIABLES##########

# Signal without fractionation at the drip site!!!

site_nb= 141

d18O_soil <- CAVES$yearly_data$isot[[site_nb]][1:1000]
temp_transtime <- CAVES$yearly_data$temp[[site_nb]][1:1000]

#TSLength <- length(d18O_soil)

#t_start = 1000

#t <-seq(1:TSLength)+t_start
dt <- 1.

tau1 <- 1.
tau2 <- 5.
tau3 <- 10.
tau4 <- 20.
#Pe <- 1.0

##MAIN######################

Results1 <- easy_sensor_wm3(dt, d18O_soil, tau1, temp_transtime)
Results2 <- easy_sensor_wm3(dt, d18O_soil, tau2, temp_transtime)
Results3 <- easy_sensor_wm3(dt, d18O_soil, tau3, temp_transtime)
Results4 <- easy_sensor_wm3(dt, d18O_soil, tau4, temp_transtime)

d18Oc_results1 <- Results1$d18OK
d18Oc_results2 <- Results2$d18OK
d18Oc_results3 <- Results3$d18OK
d18Oc_results4 <- Results4$d18OK

##CREATE SPECTRA###########
spec0 <- SpecMTM(d18O_soil)
spec1 <- SpecMTM(d18Oc_results1)
spec2 <- SpecMTM(d18Oc_results2)
spec3 <- SpecMTM(d18Oc_results3)
spec4 <- SpecMTM(d18Oc_results4)

#spec<-spec.pgram(d18Oc_results1)
##PLOTS####################

yrang<-range(spec4$spec,spec0$spec)
xrang<-range(spec2$freq,spec4$freq)


#plot(spec2$freq,spec2$spec,log='xy', ylim=c(0.001, 1), xaxt = 'n')
#points(1/spec4$freq,spec4$spec,col='red')
outdir = "/home/ginnyweasley/Dokumente/01_Promotion/07_R_Code/201908_SA_Poster/Plots/Single_Cave/"
png(file = paste(outdir,"s141-TransitTimes.png",sep=""), width = 16, height = 14, units = "cm", res = 300)
par(mai=c(0.7, 0.8, 0.4, 0.1))
LPlot(LogSmooth(spec1), col = '#808080', lwd = 2, 
      xaxt = 'n',
      xlim = c(0.001, 0.5), 
      ylim = c(5e-5, 3.6e2),
      xlab = "",
      main = TeX("Sensor signal for different transit times $\\tau$ at site 141"))
#lines(spec2,log='xy')
mtext("Periode (years)", side = 1, line= 2)
axis(side = 1, at = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.2, 0.5), 
     labels = c(1/0.001, 1/0.005, 1/0.01, 1/0.02, 1/0.05, 1/0.2, 1/0.5))
#LLines(spec2, col = '#808080')
LLines(LogSmooth(spec2), lwd = 2, col = '#51A3A3')
#LLines(spec2, col = '#0A2463')
LLines(LogSmooth(spec3), lwd = 2, col = '#3E92CC')
#LLines(spec3, col = '#3E92CC')
LLines(LogSmooth(spec4), lwd = 2, col = '#0A2463')
#LLines(spec4, col = '#0A2463')
LLines(LogSmooth(spec0), lwd = 2, col = '#B5123E')
LLines(spec0, col = '#B5123E')


legend("bottomleft",
       legend = c("Input Signal", TeX("$\\tau_0 = 1$ years"),TeX("$\\tau_1 = 5$ year"),TeX("$\\tau_2 = 10$ years"),TeX("$\\tau_3 = 20$ years")),
       col = c('#B5123E', '#808080', '#51A3A3', '#3E92CC', '#0A2463'), lty = 1, lwd = 2)

dev.off()




