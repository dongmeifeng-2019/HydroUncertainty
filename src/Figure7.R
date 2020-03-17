library(ProjectTemplate)
library(zoo)
#setwd("D:/UncertaintyPaper/PaperOutput_revision/future simulation")
showWID = c(22,29,35,50,53,59,65,69,71,76,78,81,83,90,92,93,95,98,100,105,111,117,119,120,126,132,134,135)
data_txt<-'cache/q100_uncertain.txt'
data = read.table(data_txt, header = FALSE)
uncertainty_txt = 'cache/q100_uncertainty.txt'
uncertaity = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent
for(i in 1:nrow(uncertaity)){uncertaity[i,11]=100-sum(uncertaity[i,1:10])}
tiff("graphs/figure7.tiff", 
     height = 17.7, width = 22.9, units = 'cm', res = 1200)
#windows(9,7)
par(mfrow=c(2,1))
par(mar=c(1,2,0.5,1.0),mgp=c(2.2,0.8,0),family='serif')

boxplot(data[,showWID],outline=FALSE,xaxt='n',yaxt='n', ann=FALSE,tck=-0.01)
abline(h=0,lty=2,col='grey40')
axis(2,at=c(-100,-50,0,50,100,150,200,250),pos=-0.625,labels=c(-100,-50,0,50,100,150,200,250),tck=-0.01)
legend('topright','(a)', bty="n", text.font=2,cex=1.2,horiz = TRUE)
par(mar=c(3,2,0,1.0),mgp=c(2.2,0.8,0),family='serif')
plot(c(1:28),uncertaity[showWID,1],xaxt='n',ann=FALSE,yaxt='n',type='l',col='black',log='y',ylim=c(0.1,1000),lwd=2) #hydro
lines(c(1:28),uncertaity[showWID,2],col='blue',lwd=2)      #para
lines(c(1:28),uncertaity[showWID,3],col='red',lwd=2)          #GCM
lines(c(1:28),uncertaity[showWID,4],col='green',lwd=2)         #RCP
lines(c(1:28),uncertaity[showWID,5],col='orange',lwd=2)         #Hydro-Para
lines(c(1:28),uncertaity[showWID,6],col='purple',lwd=2)           #Hydro-GCM
lines(c(1:28),uncertaity[showWID,7],col='brown',lwd=2)           #Hydro-RCP
lines(c(1:28),uncertaity[showWID,8],col='gray',lwd=2)             ##Para-GCM
lines(c(1:28),uncertaity[showWID,9],col='darkseagreen2',lwd=2)    #Para-rcp
lines(c(1:28),uncertaity[showWID,10],col='pink',lwd=2)           #GCM-rcp
lines(c(1:28),uncertaity[showWID,11],col='darkcyan',lwd=2)      #other
lablist<-as.vector(c(1:28))
axis(1, at=seq(1, 28, by=1), pos=0.07,labels = FALSE,tck=0.01)
text(seq(1, 28, by=1), 0.07, labels = lablist, srt = 0, pos = 1, xpd = TRUE)
lablist<-as.vector(c(0.1,1,5,10,20,40,60))
axis(2,at=c(0.1,1,5,10,20,40,60),pos=-0.08,labels=lablist,tck=-0.01)
legtxt = c('Hydro','Hydro-Para','Para-RCP','Para','Hydro-GCM','Para-GCM','RCP','Hydro-RCP','other','GCM','GCM-RCP')
legend('top',legtxt,col=c('black','orange','darkseagreen2','blue','purple','gray','green','brown','darkcyan','red','pink'),ncol=5,
       bty='n',lty=1,lwd=2)
legend('topright','(b)', bty="n", text.font=2,cex=1.2,horiz = TRUE)

dev.off()