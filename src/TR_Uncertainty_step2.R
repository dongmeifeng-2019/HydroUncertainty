rm(list=ls())
library(zoo)
library(stringr)
library(e1071)
setwd("D:/UncertaintyPaper/PaperOutput_revision_new/future simulation")
input_txt <- 'tr_monthly_uncertain_19.txt'        #"q100_uncertain.txt"  'max_uncertain.txt'


input <- read.table(input_txt, header = FALSE)
input[input==Inf]=100
input[is.na(input)]=0

aa=combn(x = 3, m = 2)
bb=combn(x = 10, m = 2)
cc=combn(x = 10, m = 2)
dd=combn(x = 2, m = 2)
nn = ncol(aa)*ncol(bb)*ncol(cc)*ncol(dd)
n1 = 200  #number runs of each hydro model
n2 = 20  # number runs of each para set
n3 = 2   # number runs of each GCM
val = array(0,dim=c(16,1))
for(WID in 1:12){
  s = array(0,dim=c(11,1))
  for(i in 1:ncol(aa)){ #hydro model
    for(j in 1:ncol(bb)) { #para
      for(k in 1:ncol(cc)){  # GCMs
        for(l in 1:ncol(dd)){  #RCP
          val[1] = input[((aa[,i][1]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][1]-1)*2+dd[,1][1]),WID]
          val[2] = input[((aa[,i][1]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][1]-1)*2+dd[,1][2]),WID]
          val[3] = input[((aa[,i][1]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][2]-1)*2+dd[,1][1]),WID]
          val[4] = input[((aa[,i][1]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][2]-1)*2+dd[,1][2]),WID]
          val[5] = input[((aa[,i][1]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][1]-1)*2+dd[,1][1]),WID]
          val[6] = input[((aa[,i][1]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][1]-1)*2+dd[,1][2]),WID]
          val[7] = input[((aa[,i][1]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][2]-1)*2+dd[,1][1]),WID]
          val[8] = input[((aa[,i][1]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][2]-1)*2+dd[,1][2]),WID]
          val[9] = input[((aa[,i][2]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][1]-1)*2+dd[,1][1]),WID]
          val[10] = input[((aa[,i][2]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][1]-1)*2+dd[,1][2]),WID]
          val[11] = input[((aa[,i][2]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][2]-1)*2+dd[,1][1]),WID]
          val[12] = input[((aa[,i][2]-1)*n1+(bb[,j][1]-1)*20+(cc[,k][2]-1)*2+dd[,1][2]),WID]
          val[13] = input[((aa[,i][2]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][1]-1)*2+dd[,1][1]),WID]
          val[14] = input[((aa[,i][2]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][1]-1)*2+dd[,1][2]),WID]
          val[15] = input[((aa[,i][2]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][2]-1)*2+dd[,1][1]),WID]
          val[16] = input[((aa[,i][2]-1)*n1+(bb[,j][2]-1)*20+(cc[,k][2]-1)*2+dd[,1][2]),WID]
          SST = 0
          #
          if(sum(is.na(val))>0){
            print(c(i,j,k,l,sum(is.na(val))))
          }
            
          #print(val)
          for(ii in 1:16){
            if(is.na(val[ii])==0){
              SST = SST + (val[ii]-mean(val,na.rm=T))^2
            }
            
          }
          SST_hyd = 2*2*2*((mean(val[1:8],na.rm=T)-mean(val,na.rm=T))^2+(mean(val[9:16],na.rm=T)-mean(val,na.rm=T))^2)
          
          SST_para = 2*2*2*((mean(val[c(1:4,9:12)],na.rm=T)-mean(val,na.rm=T))^2
                            +(mean(val[c(5:8,13:16)],na.rm=T)-mean(val,na.rm=T))^2)
          SST_GCM = 2*2*2*((mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)-mean(val,na.rm=T))^2
                           +(mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)-mean(val,na.rm=T))^2)
          SST_RCP = 2*2*2*((mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)-mean(val,na.rm=T))^2
                            +(mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)-mean(val,na.rm=T))^2)
          SST_hyd_para = 2*2*((mean(val[c(1:4)],na.rm=T)-mean(val[c(1:4,9:12)],na.rm=T)-mean(val[1:8],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(5:8)],na.rm=T)-mean(val[c(5:8,13:16)],na.rm=T)-mean(val[1:8],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(9:12)],na.rm=T)-mean(val[c(1:4,9:12)],na.rm=T)-mean(val[9:16],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(13:16)],na.rm=T)-mean(val[c(5:8,13:16)],na.rm=T)-mean(val[9:16],na.rm=T)+mean(val,na.rm=T))^2)
          
          SST_hyd_GCM = 2*2*((mean(val[c(1:2,5:6)],na.rm=T)-mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)-mean(val[1:8],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(3:4,7:8)],na.rm=T)-mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)-mean(val[1:8],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(9:10,13:14)],na.rm=T)-mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)-mean(val[9:16],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(11:12,15:16)],na.rm=T)-mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)-mean(val[9:16],na.rm=T)+mean(val,na.rm=T))^2)
          
          SST_hyd_RCP = 2*2*((mean(val[c(1,3,5,7)],na.rm=T)-mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)-mean(val[1:8],na.rm=T)+mean(val,na.rm=T))^2
                             +(mean(val[c(2,4,6,8)],na.rm=T)-mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)-mean(val[1:8],na.rm=T)+mean(val,na.rm=T))^2
                             +(mean(val[c(9,11,13,15)],na.rm=T)-mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)-mean(val[9:16],na.rm=T)+mean(val,na.rm=T))^2
                             +(mean(val[c(10,12,14,16)],na.rm=T)-mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)-mean(val[9:16],na.rm=T)+mean(val,na.rm=T))^2)
          
          SST_para_GCM = 2*2*((mean(val[c(1,2,9,10)],na.rm=T)-mean(val[c(1:4,9:12)],na.rm=T)-mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)+mean(val,na.rm=T))^2
                             +(mean(val[c(3,4,11,12)],na.rm=T)-mean(val[c(1:4,9:12)],na.rm=T)-mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)+mean(val,na.rm=T))^2
                             +(mean(val[c(5,6,13,14)],na.rm=T)-mean(val[c(5:8,13:16)],na.rm=T)-mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)+mean(val,na.rm=T))^2
                             +(mean(val[c(7,8,15,16)],na.rm=T)-mean(val[c(5:8,13:16)],na.rm=T)-mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)+mean(val,na.rm=T))^2)
          
          SST_para_RCP = 2*2*((mean(val[c(1,3,9,11)],na.rm=T)-mean(val[c(1:4,9:12)],na.rm=T)-mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(2,4,10,12)],na.rm=T)-mean(val[c(1:4,9:12)],na.rm=T)-mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(5,7,13,15)],na.rm=T)-mean(val[c(5:8,13:16)],na.rm=T)-mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(6,8,14,16)],na.rm=T)-mean(val[c(5:8,13:16)],na.rm=T)-mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)+mean(val,na.rm=T))^2)
          
          SST_GCM_RCP = 2*2*((mean(val[c(1,5,9,13)],na.rm=T)-mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)-mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(2,6,10,14)],na.rm=T)-mean(val[c(1:2,5:6,9:10,13:14)],na.rm=T)-mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(3,7,11,15)],na.rm=T)-mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)-mean(val[c(1,3,5,7,9,11,13,15)],na.rm=T)+mean(val,na.rm=T))^2
                              +(mean(val[c(4,8,12,16)],na.rm=T)-mean(val[c(3:4,7:8,11:12,15:16)],na.rm=T)-mean(val[c(2,4,6,8,10,12,14,16)],na.rm=T)+mean(val,na.rm=T))^2)
          
          SST_2_3 = SST - SST_hyd - SST_para - SST_GCM - SST_RCP - SST_hyd_para - SST_hyd_GCM 
                    - SST_hyd_RCP - SST_para_GCM - SST_para_RCP - SST_GCM_RCP 
          
          s[1] = s[1]+SST_hyd/SST
          s[2] = s[2]+SST_para/SST
          s[3] = s[3]+SST_GCM/SST
          s[4] = s[4]+SST_RCP/SST
          s[5] = s[5]+SST_hyd_para/SST
          s[6] = s[6]+SST_hyd_GCM/SST
          s[7] = s[7]+SST_hyd_RCP/SST
          s[8] = s[8]+SST_para_GCM/SST
          s[9] = s[9]+SST_para_RCP/SST
          s[10] = s[10]+SST_GCM_RCP/SST
          s[11] = s[11]+SST_2_3/SST
          
           }
        
      }
    }
  }
  for(m in 1:11){
    s[m]=s[m]/nn
  }
  #12 columns for 12 months, 11 rows for 11 sources of uncertainties
  write.table(t(s),'tr_monthly_uncertainty_19.txt',quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
}

