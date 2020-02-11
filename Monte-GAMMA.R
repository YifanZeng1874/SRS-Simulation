#monte - GAMMA
library(logspline)
library(histogram)
library(EnvStats)
n_1 <- 250;ISE_1 <- numeric()
n_2 <- 500;ISE_2 <- numeric()
n_3 <- 1000;ISE_3 <- numeric()
R <- 1000

start.time <- Sys.time()
for(r in 1:R){
  x_norm_m1 <- rgamma(n_1,shape = 5)
  #kernel
  bw_norm_m1 = bw.ucv(x_norm_m1)
  est_kernel_norm_m1 <- density(x_norm_m1,kernel = "gaussian",bw = bw_norm_m1)
  
  #ISE-KDE
  ISE_1[r] <- sfsmisc::integrate.xy(x = est_kernel_norm_m1$x, (est_kernel_norm_m1$y - dgamma(est_kernel_norm_m1$x,shape = 5))^2)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#n3 ISE = 0.0007408765; TIME = 2.416531 secs
#n2 ISE = 0.001283826;  TIME = 2.133291 secs
#n1 ISE = 0.002166417;  TIME = 1.975712 secs
  
start.time <- Sys.time()  
for(r in 1:R){
  x_norm_m1 <- rgamma(n_2,shape = 5)
  #hist
  est_hist_norm_m1 <- histogram(x_norm_m1,type="regular",penalty="cv",freq = FALSE,control=list(cvformula=1),plot = FALSE)
  
  #ISE-hist
  x_temp <- sort(x_norm_m1)
  f <- dgamma(x_temp,shape = 5)
  f_hat <- numeric()
  for(i in 1:length(est_hist_norm_m1$counts)){
    f_hat <- c(f_hat,rep(est_hist_norm_m1$density[i],each = est_hist_norm_m1$counts[i]))
  }
  ISE_2[r] <- sfsmisc::integrate.xy(x_temp, (f_hat - f)^2)
    }
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#n3 ISE = 0.00574994; TIME = 48.60957 secs
#n2 ISE = 0.004594242;  TIME = 30.83226 secs
#n1 ISE = 0.007729572;  TIME = 18.41772 secs
  
  
start.time <- Sys.time()    
for(r in 1:R){ 
  x_norm_m1 <- rgamma(n_2,shape = 5)
    
  #logspline 
  est_lspline_norm_m1 <- logspline(x_norm_m1)
  
  #ISE-log
  y <- dlogspline(x_norm_m1, est_lspline_norm_m1)
  ISE_3[r] <- sfsmisc::integrate.xy(x_norm_m1, (y - dgamma(x_norm_m1,shape = 5))^2)  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#n3 ISE = 0.0007512295; TIME = 19.09952 secs
#n2 ISE = 0.001638446;  TIME = 12.82272 secs
#n1 ISE = 0.003367416;  TIME = 19.33025 secs