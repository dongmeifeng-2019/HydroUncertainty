rm(list=ls())
library(zoo)
# Set working folder path, put all nc files and this R code in fdir folder
setwd("C:/Research/Uncertainty/PaperOutput/future simulation")

wtxt <- "weights_BMA_mean.txt"
wt<-read.table(wtxt, header = FALSE)
chtxt <- "mean_uncertain.txt"
data1<-read.table(chtxt, header = FALSE)
gage = c(136,137,138,142,143)  #coresponding to 11119500,11120000,11119750,11119745,11119940
qs = data.frame()
for (i in 1:5){
  qs[1:180,i] = data1[,gage[i]]
}
#qs.45 = qs[seq(2, nrow(qs), 2), ]

para = data.frame()
#11119745
mu1 = 0
mu2 = 0
for(j in 1:90){
  mu1 = mu1+wt[1,j]*qs[(1+2*(j-1)),4]
  mu2 = mu2+wt[1,j]*qs[(2+2*(j-1)),4]
}
va1 = 0
va2 = 0
for(j in 1:90){
  va1 = va1+wt[1,j]*(qs[(1+2*(j-1)),4]-mu1)^2
  va2 = va2+wt[1,j]*(qs[(2+2*(j-1)),4]-mu2)^2
}
para=rbind(para,c(mu1,va1))
para=rbind(para,c(mu2,va2))
#11119750
mu1 = 0
mu2 = 0
for(j in 1:90){
  mu1 = mu1+wt[3,j]*qs[(1+2*(j-1)),3]
  mu2 = mu2+wt[3,j]*qs[(2+2*(j-1)),3]
}
va1 = 0
va2 = 0
for(j in 1:90){
  va1 = va1+wt[3,j]*(qs[(1+2*(j-1)),3]-mu1)^2
  va2 = va2+wt[3,j]*(qs[(2+2*(j-1)),3]-mu2)^2
}
para=rbind(para,c(mu1,va1))
para=rbind(para,c(mu2,va2))

#11119940
mu1 = 0
mu2 = 0
for(j in 1:90){
  mu1 = mu1+wt[5,j]*qs[(1+2*(j-1)),5]
  mu2 = mu2+wt[5,j]*qs[(2+2*(j-1)),5]
}
va1 = 0
va2 = 0
for(j in 1:90){
  va1 = va1+wt[5,j]*(qs[(1+2*(j-1)),5]-mu1)^2
  va2 = va2+wt[5,j]*(qs[(2+2*(j-1)),5]-mu2)^2
}
para=rbind(para,c(mu1,va1))
para=rbind(para,c(mu2,va2))

#11120000
mu1 = 0
mu2 = 0
for(j in 1:90){
  mu1 = mu1+wt[7,j]*qs[(1+2*(j-1)),2]
  mu2 = mu2+wt[7,j]*qs[(2+2*(j-1)),2]
}
va1 = 0
va2 = 0
for(j in 1:90){
  va1 = va1+wt[7,j]*(qs[(1+2*(j-1)),2]-mu1)^2
  va2 = va2+wt[7,j]*(qs[(2+2*(j-1)),2]-mu2)^2
}
para=rbind(para,c(mu1,va1))
para=rbind(para,c(mu2,va2))
write.table(para, 'post_BMA_mean.txt', sep = "\t",
            quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE) 
