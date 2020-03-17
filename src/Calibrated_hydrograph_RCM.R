rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("C:/Research/Uncertainty/OnlyGauges/S1SS1/RCM")
input_txt <- "input_num.txt"
input <- readLines(input_txt)

Q_txt <- "calibration_metric_best10.txt"
data = read.table(Q_txt, header=FALSE)
d1 = data
d1$mean = rowMeans(d1[,c(14,18,22,26)], na.rm = TRUE)

d1 = d1[order(-d1$mean, na.last=TRUE),]
        
input[10]=paste(d1[1,3],str_sub(input[10],start=-73),sep=' ')  #khgw_all
input[11]=paste(d1[1,1],str_sub(input[11],start=-73),sep=' ')    #kh_all
input[13]=paste(d1[1,2],str_sub(input[13],start=-37),sep=' ')     #ksat_all
input[40]=paste(d1[1,4],str_sub(input[40],start=-11),sep=' ')  #RC1m
input[41]=paste(d1[1,5],str_sub(input[41],start=-11),sep=' ')  #RC2m
input[42]=paste(d1[1,6],str_sub(input[42],start=-11),sep=' ')   #s_tip
           
        
writeLines(input, con=input_txt)
        
system(paste('C:\\Windows\\SysWOW64\\cmd.exe /K "RCM_S1SS1.exe"'), intern = FALSE,ignore.stdout = FALSE, 
       ignore.stderr = FALSE,wait = TRUE, input = NULL, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)

file.rename("discharge_cfs.txt",'calibrated_discharge_cfs_RCM.txt')

  