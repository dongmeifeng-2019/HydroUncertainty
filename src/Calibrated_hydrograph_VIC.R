rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/S1SS1/VIC")
input_txt <- "input_num.txt"
input <- readLines(input_txt)

Q_txt <- "cal_metrics.txt"
data = read.table(Q_txt, header=FALSE)
d1 = data
d1$mean = rowMeans(d1[,c(14,18,22,26)], na.rm = TRUE)

d1 = d1[order(-d1$mean, na.last=TRUE),]

input[10]=paste(d1[1,2],str_sub(input[10],start=-73),sep=' ')  #khgw_all
input[11]=paste(d1[1,1],str_sub(input[11],start=-73),sep=' ')    #kh_all
input[62]=paste(d1[1,3],str_sub(input[62],start=-4),sep=' ')     #bin
input[65]=paste(d1[1,4],str_sub(input[65],start=-3),sep=' ')   #Ds
input[66]=paste(d1[1,5],str_sub(input[66],start=-8),sep=' ')
input[67]=paste(d1[1,6],str_sub(input[67],start=-7),sep=' ')

writeLines(input, con=input_txt)

system(paste('C:\\Windows\\SysWOW64\\cmd.exe /K "run.exe"'), intern = FALSE,ignore.stdout = FALSE, 
       ignore.stderr = FALSE,wait = TRUE, input = NULL, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)

file.rename("discharge_cfs.txt",'calibrated_discharge_cfs_VIC.txt')
