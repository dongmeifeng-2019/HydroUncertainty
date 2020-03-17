library(ProjectTemplate)
library(zoo)
#setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")
#showWID = c(22,29,35,50,53,59,65,69,71,76,78,81,83,90,92,93,95,98,100,105,111,117,119,120,126,132,134,135)
data_txt<-'cache/ws_start_change.txt'
data_st = read.table(data_txt, header = FALSE)
uncertainty_txt = 'cache/ws_start_uncertainty.txt'
uncertaity_st = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_txt<-'cache/ws_length_change.txt'
data_len = read.table(data_txt, header = FALSE)
uncertainty_txt = 'cache/ws_length_uncertainty.txt'
uncertaity_len = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

change_st_all = rowMeans(data_st)
uncertainty_st_all = colMeans(uncertaity_st)
uncertainty_st_all[11] = 100-sum(uncertainty_st_all[1:10])
change_len_all = rowMeans(data_len)
uncertainty_len_all = colMeans(uncertaity_len)
uncertainty_len_all[11] = 100-sum(uncertainty_len_all[1:10])

#tiff("D:/UncertaintyPaper/paper4_revision_new/graphs/figure8.tiff", 
#     height = 2.0, width = 4.5, units = 'in', res = 1200)
windows(5,3)
#par(mfrow=c(1,2))
layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
par(mar=c(2,2,1,1),mgp=c(2.2,0.8,0),family='serif')

boxplot(change_st_all,change_len_all,outline=FALSE,xaxt='n',yaxt='n', ann=FALSE,tck=-0.01,ylim=c(-50,50))
abline(h=0,lty=2,col='grey40')
axis(2,at=c(-40,-20,0,20,40),pos=0.418,labels=c(-40,-20,0,20,40),tck=-0.01)
legend('topright','(a)', bty="n", text.font=2,cex=1.2,horiz = TRUE)
lablist<-c('onset','duration')
axis(1, at=c(1,2), pos=-54,labels = FALSE,tck=0.01)
text(c(1,2), -54, labels = lablist, srt = 0, pos = 1, xpd = TRUE)

par(mar=c(2,4,1,10),mgp=c(2.2,0.8,0),family='serif',xpd=TRUE)
cols=c('red','grey','grey50','grey90','yellow','orange','blue','green','purple','skyblue','brown')
barplot(cbind(uncertainty_st_all,uncertainty_len_all),
        xaxt='n',yaxt='n', ann=FALSE,tck=-0.01,col=cols,
        beside=F)
lablist<-c('onset','duration')
axis(1, at=c(0,1,2), pos=0,labels = FALSE,tck=0.00)
text(c(0.7,1.9), 0, labels = lablist, srt = 0, pos = 1, xpd = TRUE)
axis(2,at=c(0,20,40,60,80,100),pos=0.1,labels=c(0,20,40,60,80,100),tck=-0.01)
legend(4,100,inset=c(-0.3,0),legend='(b)', bty="n", text.font=2,cex=1.2,horiz = TRUE)

#plot()
legend(3,100,legend =c('Hydro','Para','GCM','RCP','Hydro-Para','Hydro-GCM','Hydro-RCP','Para-GCM','Para-RCP',
                        'GCM-RCP','other'), 
       fill = cols, bty = "n")


#dev.off()