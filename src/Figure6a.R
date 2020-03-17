library(ProjectTemplate)
library(zoo)
library(reshape)
#setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")
showWID = c(22,29,35,50,53,59,65,69,71,76,78,81,83,90,92,93,95,98,100,105,111,117,119,120,126,132,134,135)
data_txt<-'cache/sr_monthly_uncertain_19.txt'  #relative change (%)
data_sr = read.table(data_txt, header = FALSE)
uncertainty_txt = 'cache/sr_monthly_uncertainty_19.txt'  #uncertainties
uncertaity_sr = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_txt<-'cache/ss_monthly_uncertain_19.txt'  #relative change (%)
data_ss = read.table(data_txt, header = FALSE)
uncertainty_txt = 'cache/ss_monthly_uncertainty_19.txt'  #uncertainties
uncertaity_ss = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_txt<-'cache/tr_monthly_uncertain_19.txt'  #relative change (%)
data_tr = read.table(data_txt, header = FALSE)
uncertainty_txt = 'cache/tr_monthly_uncertainty_19.txt'  #uncertainties
uncertaity_tr = read.table(uncertainty_txt, header = FALSE)*100.0   # to percent

data_change = rbind(data_sr,data_ss,data_tr)

data_change[,13]=c(rep("surface runoff",600),rep("subsurface runoff",600),rep("total runoff",600))
colnames(data_change)=c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Label")
df.m <- melt(data_change, id.var = "Label")
df.m$Label<-factor(df.m$Label, levels=c("surface runoff",  "subsurface runoff","total runoff"))

p <- ggplot(data = df.m, aes(x=variable, y=value,fill=Label)) + geom_boxplot(outlier.shape = NA)+ylim(-120,300)
p <- p + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p <- p +xlab(" ")+ ylab("Change (%)")

p = p + theme(legend.position=c(0.17,0.92),legend.direction="vertical",
              legend.background = element_rect(fill=FALSE, 
               size=2.0, linetype="solid"),legend.text=element_text(size=12),legend.title=element_blank())
p <- p +theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"))
p = p+geom_hline(yintercept=0, #linetype="dashed", 
color = "grey", size=0.5)

fig6a=p+scale_fill_manual(values=c( "red", "grey40","black"))

#ggsave("D:/UncertaintyPaper/paper4_revision_new/graphs/Figure6-runoffchange.tiff", units="in", width=7, height=4, dpi=1200, compression = 'lzw')
