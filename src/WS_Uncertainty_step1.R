#--------------------------------------------------------------------------------------------------------
####### compare the difference in wet seasons between 1986-2005 and 2081-2100 for the whole coast ####### 
####### By Dongmei Feng   on /09/03/2019  
#--------------------------------------------------------------------------------------------------------

rm(list=ls())
library(zoo)
# Set working folder path, put all nc files and this R code in fdir folder
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")
source("D:/UncertaintyPaper/paper4_revision_new/src/WetSeasonLength.r")

modelnm = c('RCM', 'VIC','STP')

x=1:7304  #1986/1/1-2005/12/31

date1=as.Date(x,  origin = "1985-12-31")
date2=as.Date(x,  origin = "2080-12-31") 

for (modelID in 1:3){ #hydro models
  for(i in 1:10){   #para sets
    for (j in 1:10){  # GCMs
      Qh <- paste(modelnm[modelID],'_P',i,'_M',j,'_00','.txt',sep='')
      print(Qh)
      data1<-read.table(Qh, header = TRUE)
      Qf1 <- paste(modelnm[modelID],'_P',i,'_M',j,'_45','.txt',sep='')
      data2<-read.table(Qf1, header = TRUE)
      Qf2 <- paste(modelnm[modelID],'_P',i,'_M',j,'_85','.txt',sep='')
      data3<-read.table(Qf2, header = TRUE)
      
      wst1 = data.frame()
      wse1 = data.frame()
      wsl1 = data.frame()
      wst2 = data.frame()
      wse2 = data.frame()
      wsl2 = data.frame()
      for (k in 1:145){
        ws1 = wetseason(data1[,k],date1)
        ws2 = wetseason(data2[,k],date2)
        ws3 = wetseason(data3[,k],date2)
        wst1[1,k] = ws2[1]-ws1[1] #change in onset for RCP45
        wse1[1,k] = ws2[2]-ws1[2] #change in end for RCP45
        wsl1[1,k] = (ws2[2]-ws2[1])-(ws1[2]-ws1[1]) #change in length for RCP45
        wst2[1,k] = ws3[1]-ws1[1] #change in onset for RCP85
        wse2[1,k] = ws3[2]-ws1[2] #change in end for RCP85
        wsl2[1,k] = (ws3[2]-ws3[1])-(ws1[2]-ws1[1]) #change in length for RCP85
      }
      write.table(wst1,"ws_start_change.txt",sep = "\t",quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
      write.table(wst2,"ws_start_change.txt",sep = "\t",quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
      write.table(wse1,"ws_end_change.txt",sep = "\t",quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
      write.table(wse2,"ws_end_change.txt",sep = "\t",quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
      write.table(wsl1,"ws_length_change.txt",sep = "\t",quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
      write.table(wsl2,"ws_length_change.txt",sep = "\t",quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
    }
  }
}

