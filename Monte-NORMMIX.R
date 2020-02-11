#monte - MixNormal
library(logspline)
library(histogram)
library(EnvStats)
n_1 <- 250;ISE_1 <- numeric()
n_2 <- 500;ISE_2 <- numeric()
n_3 <- 1000;ISE_3 <- numeric()
R <- 1000

start.time <- Sys.time()
for(r in 1:R){
  x_norm_m1 <- rnormMix(n_3, mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)
  #kernel
  bw_norm_m1 = bw.ucv(x_norm_m1)
  est_kernel_norm_m1 <- density(x_norm_m1,kernel = "gaussian",bw = bw_norm_m1)
  
  #ISE-KDE
  y <- dnormMix(est_kernel_norm_m1$x,mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)
  ISE_1[r] <- sfsmisc::integrate.xy(x = est_kernel_norm_m1$x, (est_kernel_norm_m1$y - y)^2)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#n3 ISE = 0.00116207; TIME = 5.076435 secs
#n2 ISE = 0.001997027;  TIME = 4.233651 secs
#n1 ISE = 0.003516243;  TIME = 3.536425 secs

start.time <- Sys.time()  
for(r in 1:R){
  x_norm_m1 <- rnormMix(n_3, mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)
  #hist
  est_hist_norm_m1 <- histogram(x_norm_m1,type="regular",penalty="cv",freq = FALSE,control=list(cvformula=1),plot = FALSE)
  
  #ISE-hist
  x_temp <- sort(x_norm_m1)
  f <- dnormMix(x_temp,mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)
  f_hat <- numeric()
  for(i in 1:length(est_hist_norm_m1$counts)){
    f_hat <- c(f_hat,rep(est_hist_norm_m1$density[i],each = est_hist_norm_m1$counts[i]))
  }
  ISE_2[r] <- sfsmisc::integrate.xy(x_temp, (f_hat - f)^2)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#n3 ISE = 0.004074347; TIME = 53.15455 secs
#n2 ISE = 0.006466501;  TIME = 26.98668 secs
#n1 ISE = 0.01072714;  TIME = 17.37344 secs


start.time <- Sys.time()    
for(r in 1:R){ 
  x_norm_m1 <- rnormMix(n_3, mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)
  
  #logspline 
  est_lspline_norm_m1 <- logspline(x_norm_m1)
  
  #ISE-log
  f_hat <- dlogspline(x_norm_m1, est_lspline_norm_m1)
  f <- dnormMix(x_norm_m1,
                mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)
  ISE_3[r] <- sfsmisc::integrate.xy(x_norm_m1, (f_hat - f)^2)  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#n3 ISE = 0.001738776; TIME = 22.93557 secs
#n2 ISE = 0.002914249;  TIME = 14.30196 secs
#n1 ISE = 0.00542875;  TIME = 11.03742 secs