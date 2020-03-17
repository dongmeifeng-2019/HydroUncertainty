rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("D:/UncertaintyPaper/PaperOutput_revision/future simulation")
#load("C:/Research/Uncertainty/paper4_revision/cache/error_RCM.RData")
error_RCM = read.table('D:/UncertaintyPaper/paper4_revision/cache/para_final_RCM.txt',header=F)
input_txt <- "input_num.txt"
input <- readLines(input_txt)

data = error_RCM
nr = nrow(data)
d1 = data
nbest = 10
modelnm = c('RCM', 'VIC','STP')
modelID = 1  # change here for hydrologic models

for(i in 1:nbest){
  for (j in 1:4){  # GCMs
    for(k in 0:2) {# sceinarios
      input[10]=paste(d1[i,3],str_sub(input[10],start=-73),sep=' ')  #khgw_all
      input[11]=paste(d1[i,1],str_sub(input[11],start=-73),sep=' ')    #kh_all
      input[13]=paste(d1[i,2],str_sub(input[13],start=-37),sep=' ')     #ksat_all
      input[40]=paste(d1[i,4],str_sub(input[40],start=-11),sep=' ')  #RC1m
      input[41]=paste(d1[i,5],str_sub(input[41],start=-11),sep=' ')  #RC2m
      input[42]=paste(d1[i,6],str_sub(input[42],start=-11),sep=' ')   #s_tip
      
      
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
      
      system(paste('C:\\Windows\\SysWOW64\\cmd.exe /K "RCM_S1SS1_Runoff.exe"'), intern = FALSE,ignore.stdout = FALSE, 
             ignore.stderr = FALSE,wait = TRUE, input = NULL, show.output.on.console = TRUE,
             minimized = FALSE, invisible = TRUE)
      
      file.rename("surface_runoff_mmpm.txt",paste(modelnm[modelID],'_P',i,'_M',j,'_',pd,'_sr.txt',sep=''))
      file.rename("subsurface_runoff_mmpm.txt",paste(modelnm[modelID],'_P',i,'_M',j,'_',pd,'_ss.txt',sep=''))
      
    }
    
  } 
}
        



  