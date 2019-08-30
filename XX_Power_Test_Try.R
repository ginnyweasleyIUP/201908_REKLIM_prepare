CAVE_1 <- CAVES$yearly_data$temp[[141]][1:600]
CAVE_2 <- CAVES$yearly_data$temp[[136]][1:600]
CAVE_3 <- CAVES$yearly_data$temp[[1]][1:600]
CAVE_4 <- CAVES$yearly_data$temp[[2]][1:600]

plot(1, type="n", xlab="", ylab="", xlim=c(0, length(CAVE_1)), ylim=c(-3, 3))
list_n = c()
mu_1 = mean(CAVE_1)

for (jj in 1:100){
  for (ii in 1:(length(CAVE_3)-5)){
    #y = t.test(CAVE_3, sample(CAVE_3, length(CAVE_3)-ii))
    samples = length(CAVE_1)-ii
    y = t.test(sample(CAVE_1, samples),
               alternative= "two.sided",
               mu = mu_1)
    if (TRUE){
      
      if (y$p.value >0.95){#(y$statistic[[1]]>y$conf.int[2] || y$statistic[[1]]<y$conf.int[1]){
        points(ii, y$statistic[[1]], col = "red")
        list_n = c(list_n, ii)
      }else{
        points(ii, y$statistic[[1]])
      }
    }
  
  }
  print(jj)
}
hist(list_n, breaks = 500)


