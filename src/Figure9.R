rm(list=ls())
library(zoo)
# Set working folder path, put all nc files and this R code in fdir folder
#setwd("D:/HydroUncertainty/cache")

wtxt <- "cache/post_BMA_mean.txt"         # mu and var (each row)of gauge 11119745, 11119750 11119940 and 11120000 for RCP 4.5 and 8.5
wt<-read.table(wtxt, header = FALSE)
# for guage 11119750

mu_45_mean=wt[3,1] #RCP4.5
var_45_mean=wt[3,2] #RCP4.5
mu_85_mean=wt[4,1] #RCP8.5
var_85_mean=wt[4,2] #RCP8.5

wtxt <- "cache/post_BMA_max.txt"         # mu and var (each row)of gauge 11119745, 11119750 11119940 and 11120000 for RCP 4.5 and 8.5
wt<-read.table(wtxt, header = FALSE)

mu_45_max=wt[3,1] #RCP4.5
var_45_max=wt[3,2] #RCP4.5
mu_85_max=wt[4,1] #RCP8.5
var_85_max=wt[4,2] #RCP8.5

wtxt <- "cache/post_BMA_q100.txt"         # mu and var (each row)of gauge 11119745, 11119750 11119940 and 11120000 for RCP 4.5 and 8.5
wt<-read.table(wtxt, header = FALSE)

mu_45_q100=wt[3,1] #RCP4.5
var_45_q100=wt[3,2] #RCP4.5
mu_85_q100=wt[4,1] #RCP8.5
var_85_q100=wt[4,2] #RCP8.5

lb=0; ub=250
#i <- x >= lb & x <= ub
#tiff("C:/Research/Uncertainty/paper4/graphs/figure6.tiff", 
   # height = 7.62, width = 22.86, units = 'cm', res = 1200)
windows(9,3)
par(mfrow=c(1,3))
par(mar=c(3.5,3.5,0.8,0.1),mgp=c(2.2,0.8,0),family='serif')
par(fig=c(0,3.6,0,10)/10,new=TRUE)
x <- seq(-5,6,length=500)*(var_45_mean)**0.5 + mu_45_mean
hx <- dnorm(x,mu_45_mean,(var_45_mean)**0.5)
plot(x,hx,type='l',col='blue',lty=1,ylim=c(0,0.015),ylab='Density',xlim=c(-100,200),xlab='',cex.axis=1.5,
     tck=-0.01,cex.lab=1.5)   #RCP 4.5 mean
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(0, 0, 1,1))
area <- pnorm(ub, mu_45_mean, (var_45_mean)**0.5) - pnorm(lb, mu_45_mean, (var_45_mean)**0.5)
result1 <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
legend('topleft',expression(bold("Q"["m"])), bty="n", text.font=2,cex=1.5,horiz = TRUE)
#RCP 8.5 mean
x <- seq(-5,6,length=500)*(var_85_mean)**0.5 + mu_85_mean
hx <- dnorm(x,mu_85_mean,(var_85_mean)**0.5)
lines(x,hx,col='red',lty=1)  
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(1, 0, 0,0.9))
area <- pnorm(ub, mu_85_mean, (var_85_mean)**0.5) - pnorm(lb, mu_85_mean, (var_85_mean)**0.5)
result2 <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))


#RCP 4.5 max
par(mar=c(3.5,1,0.8,0.1),mgp=c(2.2,0.8,0),family='serif')
par(fig=c(3.6,6.8,0,10)/10,new=TRUE)
x <- seq(-5,6,length=500)*(var_45_max)**0.5 + mu_45_max
hx <- dnorm(x,mu_45_max,(var_45_max)**0.5)
plot(x,hx,type='l',col='blue',lty=1,ylim=c(0,0.015),yaxt='n',xlim=c(-100,200),xlab='Relative change (%)',
     cex.lab=1.5,tck=-0.01,cex.axis=1.5) 
legend('topleft',expression(bold("Q"["p"])), bty="n", text.font=2,cex=1.5,horiz = TRUE)
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(0, 0, 1,1))
area <- pnorm(ub, mu_45_max, (var_45_max)**0.5) - pnorm(lb, mu_45_max, (var_45_max)**0.5)
result3 <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))

#RCP 8.5 max
x <- seq(-5,6,length=500)*(var_85_max)**0.5 + mu_85_max
hx <- dnorm(x,mu_85_max,(var_85_max)**0.5)
lines(x,hx,col='red',lty=1) 
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(1, 0, 0,0.9))
area <- pnorm(ub, mu_85_max, (var_85_max)**0.5) - pnorm(lb, mu_85_max, (var_85_max)**0.5)
result4 <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))

#RCP 4.5 q100
par(mar=c(3.5,1,0.8,0.1),mgp=c(2.2,0.8,0),family='serif')
par(fig=c(6.8,10,0,10)/10,new=TRUE)
x <- seq(-5,6,length=500)*(var_45_q100)**0.5 + mu_45_q100
hx <- dnorm(x,mu_45_q100,(var_45_q100)**0.5)

plot(x,hx,type='l',col='blue',lty=1,ylim=c(0,0.015),yaxt='n',xlim=c(-100,200),xlab='',tck=-0.01,cex.axis=1.5) 
legend('topleft',expression(bold("Q"["100"])), bty="n", text.font=2,cex=1.5,horiz = TRUE)
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(0, 0, 1,1))
area <- pnorm(ub, mu_45_q100, (var_45_q100)**0.5) - pnorm(lb, mu_45_q100, (var_45_q100)**0.5)
result5 <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
#RCP 8.5 q100
x <- seq(-5,6,length=500)*(var_85_q100)**0.5 + mu_85_q100
hx <- dnorm(x,mu_85_q100,(var_85_q100)**0.5)
lines(x,hx,col='red',lty=1)  
i <- x >= lb & x <= ub
polygon(c(lb,x[i],ub), c(0,hx[i],0), col=rgb(1, 0, 0,0.9))
area <- pnorm(ub, mu_85_q100, (var_85_q100)**0.5) - pnorm(lb, mu_85_q100, (var_85_q100)**0.5)
result6 <- signif(area, digits=3)

legend('topright',c('RCP 4.5', 'RCP 8.5'),
       lty=c(1,1),col=c('blue','red'),ncol=1,bty='n',cex=1.5)

#dev.off()
