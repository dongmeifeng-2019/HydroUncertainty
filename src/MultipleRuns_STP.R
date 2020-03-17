rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("C:/Research/Uncertainty/PaperOutput/future simulation")
load("C:/Research/Uncertainty/paper4/cache/error_STP.RData")
input_txt <- "input_num.txt"
input <- readLines(input_txt)

data = error_STP
nr = nrow(data)
d1 = data
d1$e=0
nbest = 3
modelnm = c('RCM', 'VIC','STP')
modelID = 3  # change here for hydrologic models

for(j in 1:nr){
  #d1$e[j]=sqrt(mean(d1[j,c(7:30)]^2))
  d1$e[j]=mean(c(d1[j,14],d1[j,18],d1[j,22],d1[j,26]))
}

d1 = d1[order(-d1$e, na.last=TRUE),]

for(i in 1:nbest){
  for (j in 1:10){  # GCMs
      for(k in 0:2) {# sceinarios
        
        input[10]=paste(d1[i,2],str_sub(input[10],start=-73),sep=' ')  #khgw_all
        input[11]=paste(d1[i,1],str_sub(input[11],start=-73),sep=' ')    #kh_all
        input[67]=paste(d1[i,3],str_sub(input[67],start=-32),sep=' ')     #fover
        input[68]=paste(d1[i,4],str_sub(input[68],start=-36),sep=' ')   #fdrain
        input[70]=paste(d1[i,5],str_sub(input[70],start=-20),sep=' ')   #Rsbmax
        input[71]=paste(d1[i,6],str_sub(input[71],start=-37),sep=' ')      #hs
        
        
        input[45]=paste(1,str_sub(input[45],start=-7),sep=" ")  # calnum
        input[46]=paste(100,str_sub(input[46],start=-7),sep=" ")  # run ID
        input[47]=paste(0,str_sub(input[47],start=-43),sep=" ")  #calvalpro
        input[48]=paste(j,str_sub(input[48],start=-23),sep=" ")   # model ID
        if(k==0){
          #input[3]=paste(490872,str_sub(input[3],start=-14),sep=' ')  # ndt 
          #input[5]=paste(1950,str_sub(input[5],start=-6),sep=' ')  # iyear
          input[3]=paste(175272,str_sub(input[3],start=-14),sep=' ')  # ndt 
          input[5]=paste(1986,str_sub(input[5],start=-6),sep=' ')  # iyear
          input[49]=paste(0,str_sub(input[49],start=-52),sep=' ')  #GCM RCP setting 45 or 85; use 0 for historical runs 
          pd = '00'
        }
        if(k==1){
          #input[3]=paste(832728,str_sub(input[3],start=-14),sep=' ')  # 
          #input[5]=paste(2006,str_sub(input[5],start=-6),sep=' ')  # iyear
          input[3]=paste(175272,str_sub(input[3],start=-14),sep=' ')  # ndt 
          input[5]=paste(2081,str_sub(input[5],start=-6),sep=' ')  # iyear
          input[49]=paste(45,str_sub(input[49],start=-52),sep=' ')  #GCM RCP setting 45 or 85; use 0 for historical runs 
          pd = '45'
        }
        if(k==2){
          #input[3]=paste(832728,str_sub(input[3],start=-14),sep=' ')  # 
          #input[5]=paste(2006,str_sub(input[5],start=-6),sep=' ')  # iyear
          input[3]=paste(175272,str_sub(input[3],start=-14),sep=' ')  # ndt 
          input[5]=paste(2081,str_sub(input[5],start=-6),sep=' ')  # iyear
          input[49]=paste(85,str_sub(input[49],start=-52),sep=' ')  #GCM RCP setting 45 or 85; use 0 for historical runs 
          pd = '85'
        }
        
        print(paste(modelnm[modelID],'_P',i,'_M',j,'_',pd,sep=''))
        
        writeLines(input, con=input_txt)
        
        system(paste('C:\\Windows\\SysWOW64\\cmd.exe /K "STP_S1SS1.exe"'), intern = FALSE,ignore.stdout = FALSE, 
               ignore.stderr = FALSE,wait = TRUE, input = NULL, show.output.on.console = TRUE,
               minimized = FALSE, invisible = TRUE)
        
        file.rename("discharge_cfs.txt",paste(modelnm[modelID],'_P',i,'_M',j,'_',pd,'.txt',sep=''))
        
    }
    
  } 
}
  