
library(logspline)
library(histogram)
library(EnvStats)
#bimodal:lognormal
set.seed(7)
x_normmix <- rnormMix(500, mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)

plot(density(x_normmix))

###kernel density estimation - with cross-validation
bw_normmix = bw.ucv(x_normmix)
est_kernel_normmix <- density(x_normmix,kernel = "gaussian",bw = bw_normmix)

###hist - the same using a regular histogram with leave-1-out CV
est_hist_normmix <- histogram(x_normmix,type="regular",penalty="cv",
                           freq = FALSE,control=list(cvformula=1))
###logspline 
est_lspline_normmix <- logspline(x_normmix)

#computing time
start.time <- Sys.time()
#code here
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####plot
sd_normmix <- seq(-6,7,length=1000)
sd_dnormmix <- dnormMix(sd_normmix,
                        mean1 = -1, sd1 = 1, mean2 = 3, sd2 = 1, p.mix = 0.4)

par(mfrow=c(1,1))
plot(sd_normmix, sd_dnormmix, xlim = c(-6,7),ylim = c(0,0.25),type="l",lty = 2,
     col = "grey",lwd = 4,xlab = "",ylab = "")
par(new = TRUE)
plot(est_kernel_normmix,xlim = c(-6,7),ylim = c(0,0.25),type = "l",lty = 2,col = "red",
     xlab = "x",ylab = "Density",main = "Density Estimation for NormalMix - KDE and Logspline")
par(new = TRUE)
plot(est_lspline_normmix,xlim = c(-6,7),ylim = c(0,0.25),type = "l",col = "blue",
     xlab = "",ylab = "")
legend(5, 0.25, legend=c("KDE", "Logspline","True Curve"),
       col=c("red", "blue","grey"), lty=c(2,1,2), cex=0.5)

####plot
plot(est_hist_normmix,freq = FALSE,xlim = c(-6,7),ylim = c(0,0.3),xlab = "x",main = "Density Estimation for NormalMix - Histogram")
lines(sd_normmix, sd_dnormmix, xlim = c(-6,7),ylim = c(0,0.25),type="l",lty = 2,
      col = "grey",lwd = 3,xlab = "",ylab = "")
legend(4, 0.25, legend=c("Tru Curve"),col=c("grey"), lty=c(2), cex=0.5)
