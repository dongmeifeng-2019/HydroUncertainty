library(ProjectTemplate)
library(zoo)
library(reshape)
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")

uncertainty_txt = 'tr_monthly_uncertainty_19.txt'  #uncertainties
uncertaity_tr = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent
for(i in 1:12){uncertaity_tr[i,11]=100-sum(uncertaity_tr[i,1:10])}

uncertaity=uncertaity_tr

tiff("D:/UncertaintyPaper/paper4_revision_new/graphs/figure6_tr_uncertain.tiff", 
     height = 3.5, width = 7, units = 'in', res = 1200)
nmonth=12
par(mar=c(3,3.0,0,1.0),mgp=c(2.2,0.5,0))#,family='serif')
plot(c(1:nmonth),uncertaity[,1],xaxt='n',ann=FALSE,yaxt='n',type='l',col='black',log='y',ylim=c(0.5,400),lwd=2,xlim=c(1,12),
     ylab='Uncertainty Contribution (%)') #hydro
lines(c(1:nmonth),uncertaity[,2],col='blue',lwd=2)      #para
lines(c(1:nmonth),uncertaity[,3],col='red',lwd=2)          #GCM
lines(c(1:nmonth),uncertaity[,4],col='green',lwd=2)         #RCP
lines(c(1:nmonth),uncertaity[,5],col='orange',lwd=2)         #Hydro-Para
lines(c(1:nmonth),uncertaity[,6],col='purple',lwd=2)           #Hydro-GCM
lines(c(1:nmonth),uncertaity[,7],col='brown',lwd=2)           #Hydro-RCP
lines(c(1:nmonth),uncertaity[,8],col='gray',lwd=2)             ##Para-GCM
lines(c(1:nmonth),uncertaity[,9],col='darkseagreen2',lwd=2)    #Para-rcp
lines(c(1:nmonth),uncertaity[,10],col='pink',lwd=2)           #GCM-rcp
lines(c(1:nmonth),uncertaity[,11],col='darkcyan',lwd=2)      #other
mtext(side=2,line=1.8, 'Uncertainty Contribution (%)', col="black", font=2,cex=1.2)
lablist<-c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
axis(1, at=seq(0, nmonth+1, by=1), pos=0.385,labels = FALSE,tck=0.01)
text(seq(1, 12, by=1),
     0.3, labels = lablist, srt = 0,  xpd = TRUE)
lablist<-as.vector(c(0.1,0.5,1,5,10,20,40,1000))
axis(2,at=c(0.1,0.5,1,5,10,20,40,1000),pos=0.555,labels=lablist,tck=-0.01)
legtxt = c('Hydro','Hydro-Para','Para-RCP','Para','Hydro-GCM','Para-GCM','RCP','Hydro-RCP','other','GCM','GCM-RCP')
legend('top',legtxt,col=c('black','orange','darkseagreen2','blue','purple','gray','green','brown','darkcyan','red','pink'),ncol=4,
       bty='n',lty=1,lwd=2)
#legend('topright','(b)', bty="n", text.font=2,cex=1.2,horiz = TRUE)

dev.off()











uncertaity = data.frame(t(uncertaity_tr))

uncertaity[,13] = c('Hydro','Para','GCM','RCP','Hydro-Para','Hydro-GCM','Hydro-RCP','Para-GCM','Para-RCP','GCM-RCP','other')
colnames(uncertaity)=c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Sources")
dat2 <- melt(uncertaity, id.vars = "Sources")
cols = rep(c('grey90','grey80','grey70','grey60','grey50','grey40','grey30','grey10','red','red2','red4'),12)
cols = rep(c('skyblue4','cornflowerblue','dodgerblue','lightskyblue','lightskyblue2','lightsteelblue',
             'grey90','grey70','grey50','grey10','grey40'),12)

p <- ggplot(data = dat2, aes(x=variable, y=value,fill=Sources)) + geom_bar(stat="identity")+
  xlab(" ") +
  ylab("Uncertainty Contribution (%)")+
  theme_minimal()

p = p+scale_fill_manual(values=cols)
p = p +theme(legend.position="top",legend.text=element_text(size=10), legend.title=element_text(size=12))
p <- p +theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"))

p

ggsave("D:/UncertaintyPaper/paper4_revision_new/graphs/Figure6_tr_uncertain.tiff", units="in", width=7, height=4, dpi=1200, compression = 'lzw')
