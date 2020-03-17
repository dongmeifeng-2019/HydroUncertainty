rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("C:/Research/SantaBarbara/RandomCali/summary")

Q_txt <- "discharge_cfs_HRR.txt"
data1 = read.table(Q_txt, header=TRUE)
Q_txt <- "discharge_cfs_VIC.txt"
data2 = read.table(Q_txt, header=TRUE)
Q_txt <- "discharge_cfs_CLM.txt"
data3 = read.table(Q_txt, header=TRUE)
Q_txt <- "usgsdata_cal.txt"
data4 = read.table(Q_txt, header=TRUE)
Q_txt <- "precipitation_Livneh_cfs.txt"
#data5 = read.table(Q_txt, header=TRUE)

nr = nrow(data1)
x = c(1:nr)
date = as.Date(x,origin= "1983-12-31")
date_wy = as.Date(x,origin= "1984-5-1")
z1 <- aggregate(zoo(data1), cut(date_wy,'y'), max)*0.3048^3
Peak1 <-coredata(z1)
z1 <- aggregate(zoo(data2), cut(date_wy,'y'), max)*0.3048^3
Peak2 <-coredata(z1)
z1 <- aggregate(zoo(data3), cut(date_wy,'y'), max)*0.3048^3
Peak3 <-coredata(z1)
z1 <- aggregate(zoo(data4[x,]), cut(date_wy,'y'), max)*0.3048^3
Peak4 <-coredata(z1)
#z1 <- aggregate(zoo(data5[x,]), cut(date_wy,'y'), max)*0.3048^3
#Peak5 <-coredata(z1)

windows(9,16)
par(mfrow=c(4,2))
par(mgp=c(2.5,1.0,0),mar=c(4.1,4.5,0.5,1))
x1 = c(1985:2004)
y1 = c(2:21)

for(gid in 2:5){
  
  if(gid==1){
    ylmax = c(0,120)
    ylmean = c(0,2)
  }
  
  if(gid==4|gid==2){
    ylmax = c(0,20)
    ylmean = c(0,2)
  }
  
  if(gid==3){
    ylmax = c(0,40)
    ylmean = c(0,2)
  }
  
  if(gid==5){
    ylmax = c(0,70)
    ylmean = c(0,2)
  }
  par(font=2)
  plot(x1,Peak1[y1,gid],ylim=ylmax,xlab='Water Year', ylab=expression(paste('Annual Peak Discharge (m'^3,'/s)',sep='')),col='green',
       cex.lab=1.5, cex.axis=1.5,font.lab=1,lwd=2,type='l')
  
  #lines(x1,Peak1[,2],col='green',lwd=2)
  lines(x1,Peak2[y1,gid],col='blue',lwd=2)
  lines(x1,Peak3[y1,gid],col='red',lwd=2)
  points(x1,Peak4[y1,gid])
  legend('topleft',c('Gauge','RCM-HRR','VIC-HRR','STP-HRR'),col=c('black','green','blue','red'),lty=c(NA,1,1,1),pch=c(1,NA,NA,NA),cex=1.5,
         bty='n',horiz=FALSE,lwd=c(NA,2,2,2))
  
  
  z1 <- aggregate(zoo(data1), cut(date_wy,'y'), mean)*0.3048^3
  Peak5 <-coredata(z1)
  z1 <- aggregate(zoo(data2), cut(date_wy,'y'), mean)*0.3048^3
  Peak6 <-coredata(z1)
  z1 <- aggregate(zoo(data3), cut(date_wy,'y'), mean)*0.3048^3
  Peak7 <-coredata(z1)
  z1 <- aggregate(zoo(data4)[x,], cut(date_wy,'y'), mean)*0.3048^3
  Peak8 <-coredata(z1)
  
  #par(mai=c(0.5,0.5,0,0.1))
  plot(x1,Peak5[y1,gid],ylim=ylmean,xlab='Water Year', ylab=expression(paste('Annual Mean Discharge (m'^3,'/s)',sep='')),col='white',
       cex.lab=1.5, cex.axis=1.5,font.lab=1)
  
  points(x1,Peak8[y1,gid])
  lines(x1,Peak5[y1,gid],col='green',lwd=2)
  lines(x1,Peak6[y1,gid],col='blue',lwd=2)
  lines(x1,Peak7[y1,gid],col='red',lwd=2)
  
  legend('topright',substring(names(data4)[gid],2),bty='n',cex=1.5)
  
} 


  