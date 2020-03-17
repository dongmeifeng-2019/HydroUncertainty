rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("D:/UncertaintyPaper/paper4_revision_new")
error_RCM = read.table('data/cal_metrics_RCM_Borg.txt',header=F)

data = error_RCM
nr = nrow(data)
d1 = data
d1$e=0
nbest = 4

for(j in 1:nr){
  #d1$e[j]=sqrt(mean(d1[j,c(7:30)]^2))
  d1$e[j]=mean(c(d1[j,14],d1[j,18],d1[j,22],d1[j,26]))
}

d1 = d1[order(-d1$e, na.last=TRUE),]

n_range = nrow(d1[d1$e>(d1$e[1]*0.8),])

para_final = data.frame()

for(i in 1:nbest){
  para_final = rbind(para_final,d1[i,])
}
d2 = d1[(nbest+1):n_range,]
d2 = d2[order(-d2[,3], na.last=TRUE),]
para = data.frame()
p1 = floor(runif((10-nbest),1,(n_range-nbest+1)))
para[1:(10-nbest),1:27] = d2[p1,]

para_final_RCM = rbind(para_final,para)

write.table(para_final_RCM,'cache/para_final_RCM.txt',col.names = F, row.names = F, append = F)
