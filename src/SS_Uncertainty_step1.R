rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")

a = 1:228  # months for 20 years
modelnm = c('RCM', 'VIC','STP')
for (modelID in 1:3){ #hydro models
  for(i in 1:10){   #para sets
    for (j in 1:10){  # GCMs
      Qh <- paste(modelnm[modelID],'_P',i,'_M',j,'_00_ss','.txt',sep='')
      data1<-read.table(Qh, header = F)
      Qf1 <- paste(modelnm[modelID],'_P',i,'_M',j,'_45_ss','.txt',sep='')
      data2<-read.table(Qf1, header = F)
      Qf2 <- paste(modelnm[modelID],'_P',i,'_M',j,'_85_ss','.txt',sep='')
      data3<-read.table(Qf2, header = F)
      
      deltaq1 <- data.frame()
      #row.names(deltaq1) = c('change in Qm RCP45','change in Qm RCP85')
      #deltaq1[1,]=0
      
      d1<-data.frame()
      d2<-data.frame()
      d3<-data.frame()
      
      d1[1:length(a),1] <- rowMeans(data1)[13:240]
      d2[1:length(a),1] <- rowMeans(data2)[13:240]
      d3[1:length(a),1] <- rowMeans(data3)[13:240]
      
      
      z1 <- aggregate(zoo(d1[,1]), rep(c(1:12),19), mean) 
      z2 <- aggregate(zoo(d2[,1]), rep(c(1:12),19), mean) 
      z3 <- aggregate(zoo(d3[,1]), rep(c(1:12),19), mean) 
      d1.max <- coredata(z1) 
      d2.max <- coredata(z2) 
      d3.max <- coredata(z3)
      for (k in 1:12){ 
        qq2 <- rbind((d2.max[k]-d1.max[k])/d1.max[k]*100,
                     (d3.max[k]-d1.max[k])/d1.max[k]*100)
        deltaq1[1:2,k] <- qq2
      }
      
      #relative change (%) of surface runoff for 12 months; each out 2 rows 12 columns
      write.table(deltaq1, 'ss_monthly_uncertain_19.txt', sep = "\t",
                  quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE) 
    }
  }
}