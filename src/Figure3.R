# library(ProjectTemplate)
# library(zoo)
# setwd("D:/HydroUncertainty")
# load.project() 
load("cache/error_RCM.RData")  #old RCM borg
load("cache/error_STP.RData")   #old STP borg
load("cache/error_VIC.RData")  #new VIC borg
error_RCM$mean = error_RCM[,18]
error_STP$mean = error_STP[,18]
error_VIC$mean = error_VIC[,18]

currentmax = function(x){
  y = array()
  for(i in 1:length(x)){
    y[i] = max(x[1:i])
  }
  #y=format(round(y, 2), nsmall = 2)
  y=formatC(y, digits = 2, format = "f")
  return(y)
}

error_RCM$best = currentmax(error_RCM$mean)
error_STP$best = currentmax(error_STP$mean)
error_VIC$best = currentmax(error_VIC$mean)

Q_STP = read.table('data/discharge_cfs_STP_val.txt',header=TRUE)*0.3048^3
Q_VIC = read.table('data/discharge_cfs_VIC_val_new.txt',header=TRUE)*0.3048^3
Q_RCM = read.table('data/discharge_cfs_RCM_val.txt',header=TRUE)*0.3048^3
Q_gauge = read.table('data/usgsdata_cal.txt',header=FALSE)*0.3048^3
Q_gauge[Q_gauge<0]=NA
dates = as.Date(c(1:10957),origin="1983-12-31")
date_wy = as.Date(c(1:10957),origin= "1984-5-1")
z1 <- aggregate(zoo(Q_STP), cut(date_wy,'y'), max)
Peak1 <-coredata(z1)
z1 <- aggregate(zoo(Q_VIC), cut(date_wy,'y'), max)
Peak2 <-coredata(z1)
z1 <- aggregate(zoo(Q_RCM), cut(date_wy,'y'), max)
Peak3 <-coredata(z1)
z1 <- aggregate(zoo(Q_gauge[1:10957,]), cut(date_wy,'y'), max)
Peak4 <-coredata(z1)
z1 <- aggregate(zoo(Q_STP), cut(date_wy,'y'), mean)
mean1 <-coredata(z1)
z1 <- aggregate(zoo(Q_VIC), cut(date_wy,'y'), mean)
mean2 <-coredata(z1)
z1 <- aggregate(zoo(Q_RCM), cut(date_wy,'y'), mean)
mean3 <-coredata(z1)
z1 <- aggregate(zoo(Q_gauge[1:10957,]), cut(date_wy,'y'), mean)
mean4 <-coredata(z1)
x1 = c(1984:2013)
y1 = c(2:30)
#tiff("graphs/figure3.tiff", 
     #height = 17.78, width = 17.78, units = 'cm', res = 1200)
windows(7,7)
par(mfrow=c(2,2))
par(mar=c(1,3.6,1.1,1.1),mgp=c(2.2,0.8,0),family = "serif")
layout(matrix(c(1,2,3,3,4,4), 3, 2, byrow = TRUE))
plot(c(1:nrow(error_STP))/nrow(error_STP),error_STP$best,ylim=c(0.3,0.7),type='l',col='darkgreen',
     frame.plot = FALSE,cex.lab=1.2,axes = FALSE,family='serif' ,xlab= '',lwd=1.5,lty=4,
     ylab = 'Best performance (NSE)',xaxt="n")
lines(c(1:nrow(error_RCM))/nrow(error_RCM),error_RCM$best,col='red',lty=2,lwd=1.5)
lines(c(1:nrow(error_VIC))/nrow(error_VIC),error_VIC$best[1:nrow(error_VIC)],col='blue',lty=3,lwd=1.5)
legend('topleft',c('STP-HRR','RCM-HRR','VIC-HRR'),col=c('darkgreen','red','blue'),lty=c(4,2,3),bty='n',
       cex=1.2,lwd=1.5)
#axis(side=1, pos = 0.0,cex.axis=0.8,tck=-0.01,family = 'serif')
par(mgp=c(1.5,0.6,0))
axis(side=1, pos = 0.3,cex.axis=1.0,tck=-0.01,at=c(0,0.2,0.4,0.6,0.8,1.0), 
     labels=c("0.0","0.2","0.4",'0.6','0.8','1.0'),family = 'serif')
axis(side=2, cex.axis=1.0,tck=-0.01,family = 'serif')
abline(h=0.3)
text(x=1,y=0.7,labels='(a)',font=2,cex=1.2)
#mtext("Normalized calibration progress", side=1, line=1, at=0.5,family = 'serif',cex=0.8)
b = c(6220:6290)
par(mar=c(1,3.6,1.1,1.1),mgp=c(2,0.8,0),family = "serif")
plot(dates[b],Q_STP$X11119750[b],type='l',col='darkgreen',ylim=c(0,20),
     frame.plot = FALSE,cex.lab=1.2,axes = FALSE,family='serif' ,xlab= '',lty=4,lwd=1.5,
     ylab=expression(paste('Discharge (m'^3,'/s)',sep='')),xaxt="n")
lines(dates[b],Q_RCM$X11119750[b],col='red',lty=2,lwd=1.5)
lines(dates[b],Q_VIC$X11119750[b],col='blue',lty=3,lwd=1.5)
points(dates[b],Q_gauge[b,3],col='black',lty=1)
par(mgp=c(1.5,0.6,0))
axis.Date(1, pos=0.0,at = seq(dates[b[1]], dates[b[length(b)]], length.out = 5),
          format= "%m-%d", las = 1,family = 'serif',cex.axis=1.0,tck=-0.01)  #"%Y-%m-%d"

axis(side=2, cex.axis=1.0,tck=-0.01,family = 'serif')
abline(h=0.0)
text(dates[b[length(b)-3]],y=20,labels='(b)',font=2,cex=1.2)
#mtext("Normalized calibration progress", side=1, line=1, at=4000,family = 'serif',cex=0.8)
legend('topleft',c('STP-HRR','RCM-HRR','VIC-HRR','USGS Gauge'),col=c('darkgreen','red','blue','black'),
       lty=c(4,2,3,NA),bty='n',lwd=1.5,pch=c(NA,NA,NA,1),
       cex=1.2)
par(mar=c(0.0,3.6,0.0,1.1),mgp=c(2,0.8,0),family = "serif")
plot(x1[y1],Peak1[y1,3], ylab=expression(paste('Discharge (m'^3,'/s)',sep='')),
     col='darkgreen',lty=4,cex.lab=1.2, cex.axis=1.0,font.lab=1,lwd=1.5,type='l',ylim=c(0,40),
     frame.plot = FALSE,xaxt="n",xlim=c(1985,2013),axes = FALSE)
lines(x1[y1],Peak3[y1,3],col='red',lty=2,lwd=1.5)
lines(x1[y1],Peak2[y1,3],col='blue',lty=3,lwd=1.5)
points(x1[y1],Peak4[y1,3],col='black',lty=1)
par(mgp=c(1.5,0.6,0))
#axis(side=1, pos = 0.0,cex.axis=1.0,tck=-0.01,at=c(1985,1989,1993,1997,2001,2005,2009,2013), 
     #labels=c("1985","1989","1993",'1997','2001','2005','2009','2013'),family = 'serif')
axis(side=1, pos = 0.0,cex.axis=1.0,tck=-0.01,at=c(1985:2013), labels = F,family = 'serif')
axis(side=2, cex.axis=1.0,tck=-0.01,family = 'serif')
abline(h=0.0)
segments(2005.5, 0.01, x1 = 2005.5, y1 = 39,col = 'grey30', lty = 2)
arrows(1996,35,1995.2,38.5,col='red',length = 0.05, angle = 20,lwd =2)
arrows(1999,25,1998.1,21.0,col='blue',length = 0.05, angle = 20,lwd =2)
arrows(2003.5,33.5,2004.7,30.7,col='red',length = 0.05, angle = 20,lwd =2)
text(1997.3,36.2,'1/10/1995',col='red')
text(1999,27.2,'2/23/1998',col='blue')
text(2003,35.2,'1/9/2005',col='red')
text(x=1986,y=38,labels='Annual Peak',font=2,cex=1.2)
text(x=1988,y=28,labels='Calibration (WY 1985-2005)', cex=1.2)
text(x=1987.1,y=22,labels=paste('RCM-HRR: NSE=',max(error_RCM$best),sep=''), cex=1.0)
text(x=1987.,y=25,labels=paste('STP-HRR: NSE=',max(error_STP$best),sep=''),     cex=1.0)
text(x=1986.9,y=19,labels=paste('VIC-HRR: NSE=',max(error_VIC$best),sep=''),    cex=1.0)

text(x=2009.5,y=28,labels='Validation (WY 2006-2011)',cex=1.2)
text(x=2008.6,y=22,labels='RCM-HRR: NSE=0.54', cex=1.0)
text(x=2008.56,y=25,labels='STP-HRR: NSE=0.62',     cex=1.0)
text(x=2008.45,y=19,labels='VIC-HRR: NSE=0.53',    cex=1.0)
text(x=2013,y=39,labels='(c)',font=2,cex=1.2)
par(mar=c(2.0,3.6,0.0,1.1),mgp=c(2,0.8,0),family = "serif")
plot(x1[y1],mean1[y1,3], ylab=expression(paste('Discharge (m'^3,'/s)',sep='')),
     col='darkgreen',lty=4,cex.lab=1.2, cex.axis=1.0,font.lab=1,lwd=1.5,type='l',ylim=c(0,1),
     frame.plot = FALSE,xaxt="n",xlim=c(1985,2013),axes = FALSE)
lines(x1[y1],mean3[y1,3],col='red',lty=2,lwd=1.5)
lines(x1[y1],mean2[y1,3],col='blue',lty=3,lwd=1.5)
points(x1[y1],mean4[y1,3],col='black',lty=1)
par(mgp=c(1.5,0.6,0))
axis(side=1, pos = 0.0,cex.axis=1.0,tck=-0.01,at=c(1985:2013), #at=c(1985,1989,1993,1997,2001,2005,2009,2013), 
     labels=c("1985","1986","1987",'1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999',
              '2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013'),
     family = 'serif')
axis(side=2, cex.axis=1.0,tck=-0.01,family = 'serif')
abline(h=0.0)
text(x=1986,y=0.95,labels='Annual Mean',font=2,cex=1.2)
text(x=2013,y=0.95,labels='(d)',font=2,cex=1.2)
#abline(v=2005.5,h=c(0.1,0.9),col='grey30',lty=2)
segments(2005.5, 0.01, x1 = 2005.5, y1 = 1.0,col = 'grey30', lty = 2)
#dev.off()