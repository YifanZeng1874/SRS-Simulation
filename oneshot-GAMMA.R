library(logspline)
library(histogram)
#Unimodal:gamma
x_gamma <- rgamma(500,shape = 5)

###kernel density estimation - with cross-validation
bw_gamma = bw.ucv(x_gamma)
est_kernel_gamma <- density(x_gamma,kernel = "gaussian",bw = bw_gamma)

###hist - the same using a regular histogram with leave-1-out CV
est_hist_gamma <- histogram(x_gamma,type="regular",penalty="cv",
                            freq = FALSE,control=list(cvformula=1))

###logspline 
est_lspline_gamma <- logspline(x_gamma)

#computing time
start.time <- Sys.time()
#code here
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####plot
plot(est_kernel_gamma)
sd_gamma <- seq(0,12,length=1000)
sd_dgamma <- dgamma(sd_gamma,shape = 5)

par(mfrow=c(1,1))
plot(sd_gamma, sd_dgamma, xlim = c(0,12),ylim = c(0,0.25),type="l",lty = 2,
     col = "grey",lwd = 4,xlab = "",ylab = "")
par(new = TRUE)
plot(est_kernel_gamma,xlim = c(0,12),ylim = c(0,0.25),type = "l",lty = 2,col = "red",
     xlab = "x",ylab = "Density",
     main = "Density Estimation for Gamma dist - KDE and Logspline")
par(new = TRUE)
plot(est_lspline_gamma,xlim = c(0,12),ylim = c(0,0.25),type = "l",col = "blue",
     xlab = "",ylab = "")
legend(10,0.25, legend=c("KDE", "Logspline","True Curve"),
       col=c("red", "blue","grey"), lty=c(2,1,2), cex=0.5)

####plot
plot(est_hist_gamma,freq = FALSE,xlab = "x",main = "Density Estimation for Gamma dist - Histogram")
lines(sd_gamma, sd_dgamma, xlim = c(0,12),ylim = c(0,0.28),type="l",lty = 2,
      col = "grey",lwd = 3,xlab = "",ylab = "")
legend(9,0.2, legend=c("True Curve"),col=c("grey"), lty=c(2), cex=0.6)
