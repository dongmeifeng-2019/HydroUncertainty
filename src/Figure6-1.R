library(ProjectTemplate)
library(zoo)
library(reshape)
library(ggplot2)
library(Ipaper)
library(tidyverse)
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")
data_txt<-'sr_monthly_uncertain_19.txt'  #relative change (%)
data_sr = read.table(data_txt, header = FALSE)
uncertainty_txt = 'sr_monthly_uncertainty.txt'  #uncertainties
uncertaity_sr = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_txt<-'ss_monthly_uncertain_19.txt'  #relative change (%)
data_ss = read.table(data_txt, header = FALSE)
uncertainty_txt = 'ss_monthly_uncertainty_19.txt'  #uncertainties
uncertaity_ss = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_txt<-'tr_monthly_uncertain_19.txt'  #relative change (%)
data_tr = read.table(data_txt, header = FALSE)
uncertainty_txt = 'tr_monthly_uncertainty_19.txt'  #uncertainties
uncertaity_tr = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_change = rbind(data_sr,data_ss,data_tr)  #change %
uncertainty = rbind(uncertaity_sr,uncertaity_ss) #uncertainty %
colnames(data_change)=c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

dd=gather(data_change)
dd$category=rep(c(rep("surface runoff",600),rep("subsurface runoff",600),rep("total runoff",600)),12)

cols <- rainbow(3, s = 0.5)
boxplot(value ~ key + category, data = dd, col=cols,at=c(1:3,5:7,9:11,13:15,17:19,21:23,25:27,29:31,
                                                         33:35,37:39,41:43,45:47),range=1,
        axes = FALSE,outline=F,ylim=c(-150,500))

mtext(side=1,line=2, "Month", col="black", font=2,cex=1.2)
mtext(side=2,line=2, 'Change (%)', col="black", font=2,cex=1.2)
#title(xlab="Groups", line=2.5, cex.lab=1.2,font=2)
#title(ylab="Changes (days)",line=2.5, cex.lab=1.2,font=2)
abline(h=0.0)
#axis(1, at=c(5,15,25,37,48,58,68,78), labels=c(1:8),cex=0.8)
Map(axis,side=1,at=c(5,15,25,37,48,58,68,78),labels=c(1:8),
    col.axis=c("orange","2","3","4","5","6","7","8"),cex.axis=0.8)
mtext( c(2,6,10,14,18,22,26,30,34,38,42,46),
      c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
legend("topleft", fill = cols, legend = c(1,2,3), horiz = T,bty='n')

tiff("D:/UncertaintyPaper/paper4_revision_new/graphs/figure8.tiff", ##runoff uncertainty sources
     height = 17.7, width = 22.9, units = 'cm', res = 1200)
#windows(9,7)
par(mfrow=c(1,1))
par(mar=c(1,2,0.5,1.0),mgp=c(2.2,0.8,0),family='serif')

