rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")

#x=1:20453   #1950-2005
#x2=1:34697   #2006-2100
#a=c(13150:20453) #1986-2005
#date_1=as.Date(x,  origin = "1949-12-31")
#date_2=as.Date(x2,  origin = "2005-12-31")
#date1=date_1[a]
#date2=date_2[14240:(14239+length(a))]
#b=c(27394:(27393+length(a)))  #future 2081-2100

x=1:7303  #1:175272  # 1986-2005
x2 = x # 2081-2100
a = x
date_1=as.Date(x,  origin = "1985-12-31")
date_2=as.Date(x2,  origin = "2080-12-31")

date1 = date_1
date2 = date_2
b = a 

modelnm = c('RCM', 'VIC','STP')
for (modelID in 1:3){ #hydro models
  for(i in 1:10){   #para sets
    for (j in 1:10){  # GCMs
      Qh <- paste(modelnm[modelID],'_P',i,'_M',j,'_00','.txt',sep='')
      data1<-read.table(Qh, header = TRUE)
      Qf1 <- paste(modelnm[modelID],'_P',i,'_M',j,'_45','.txt',sep='')
      data2<-read.table(Qf1, header = TRUE)
      Qf2 <- paste(modelnm[modelID],'_P',i,'_M',j,'_85','.txt',sep='')
      data3<-read.table(Qf2, header = TRUE)
      
      deltaq1 <- data.frame()
      #row.names(deltaq1) = c('change in Qm RCP45','change in Qm RCP85')
      #deltaq1[1,]=0
      for (k in 1:145){
        d1<-data.frame()
        d2<-data.frame()
        d3<-data.frame()
        
        d1[1:length(a),1] <- data1[a,k]   
        d2[1:length(a),1] <- data2[b,k]
        d3[1:length(a),1] <- data3[b,k]
        
        z1 <- aggregate(zoo(d1[,1]), cut(date1, "y"), max) 
        z2 <- aggregate(zoo(d2[,1]), cut(date2, "y"), max) 
        z3 <- aggregate(zoo(d3[,1]), cut(date2, "y"), max) 
        d1.max <- coredata(z1) 
        d2.max <- coredata(z2) 
        d3.max <- coredata(z3)
        
        #Gunbel distribution
        mu = mean(d1.max)
        sdv = sd(d1.max)
        alpha = 6^0.5*sdv/pi
        zeta = mu - 0.5772*alpha
        kt = -log(log(100/(100-1)))
        q100_1 = zeta+kt*alpha
        
        mu = mean(d2.max)
        sdv = sd(d2.max)
        alpha = 6^0.5*sdv/pi
        zeta = mu - 0.5772*alpha
        q100_2 = zeta+kt*alpha
        
        mu = mean(d3.max)
        sdv = sd(d3.max)
        alpha = 6^0.5*sdv/pi
        zeta = mu - 0.5772*alpha
        q100_3 = zeta+kt*alpha
        
        
        
        qq2 <- rbind((q100_2-q100_1)/q100_1*100,(q100_3-q100_1)/q100_1*100)
        deltaq1[1:2,k] <- qq2
      }
      
      write.table(deltaq1, 'q100_uncertain.txt', sep = "\t",
                  quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE) 
    }
  }
}
