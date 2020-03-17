rm(list=ls())
library(zoo)
library(forecast)
library(MASS)
# Set working folder path, put all nc files and this R code in fdir folder
setwd("C:/Research/Uncertainty/PaperOutput/future simulation")

tlist = c('11119745','11119750','11119500','11119940','11120000')

area = c(17.1, 21.7,33.9,	14.3,	49.0)
modelnm = c('RCM', 'VIC','STP')

for (j in c(1,2,4,5)){
  
  title = tlist[j] #change here to the gauge ID
  print(title)
  title1 = paste('X',title,sep='')
  x=1:7303    # 1986-2005
  date=as.Date(x,  origin = "1985/12/31")
  
  Qotxt <- "USGS1986.txt"
  data1<-read.table(Qotxt, header = TRUE)
  data_qo = data.frame()
  data_qo[1:length(x),1] <- data1[x,title1]
  
  data_qs = data.frame()
  k1=1
  for (modelID in 1:3){ #hydro models
    for(i in 1:3){   #para sets
      for (j in 1:10){  # GCMs
        Qh <- paste(modelnm[modelID],'_P',i,'_M',j,'_00','.txt',sep='')
        data1<-read.table(Qh, header = TRUE)
        data_qs[1:length(x),k1] <- data1[x,title1]
        k1 = k1+1
      }
    }
  }
  
  
  ###Box-Cox transformation
  nm = 90
  z1 <- aggregate(zoo(data_qo), cut(date,'y'), max) 
  P_annual <-coredata(z1)
  
  lambda <- BoxCox.lambda(P_annual,lower=0)
  
  qtrans = (P_annual^lambda-1)/lambda
  qo = array(0, dim=c(length(P_annual),1))
  qo[,1] = qtrans
  qs = array(0, dim=c(length(P_annual),nm))
  for (k in 1:nm){
    z1 <- aggregate(zoo(data_qs[,k]), cut(date,'y'), max) 
    P_annual <-coredata(z1)
    qtrans = (P_annual^lambda-1)/lambda
    qs[,k] = qtrans
  }
  
  ### Rank data from high to low
  
  qo = qo[order(-qo, na.last=TRUE),]
  for (k in 1:nm){
    qs[,k] = qs[order(-qs[,k], na.last=TRUE),k]
  }
  
  ###BMA start
  el = 100
  w = array(1/nm, dim=c(nm,1))
  zt = array(0, dim=c(nm,1))
  a = qo
  for (kk in 1:nm){
    for (t in 1:(length(a)-1)){
      zt[kk] = zt[kk]+(qo[t]-qs[t,kk])^2
    }
  }
  
  zt = zt/kk/t
  lp = 100
  iter = 1
  
  while(el>0.0000001){
    print(iter)
    l = 0.0
    for (k in 1:nm){
      lt = 0.0
      for (t in 1:(length(a)-1)){
        lt = lt + dnorm(qo[t],mean=qs[t,k],sd=sqrt(zt[k]))
        #print(lt)
      }
      l = l + w[k]*lt
    }
    l = log(l)
    #lp = log(lp)
    el = abs(l-lp)
    print(c(l,lp))
    lp = l
    ziter = array(0,dim=c(nm,length(a)-1))
    for (k in 1:nm){
      zzt = 0.0
      for (t in 1:(length(a)-1)){
        z = dnorm(qo[t],mean=qs[t,k],sd=sqrt(zt[k]))
        #print(c(k,t,zt[k]))
        zsum = 0.0
        for(m in 1:nm){
          yy = dnorm(qo[t],mean=qs[t,m],sd=sqrt(zt[m]))
          #print(c(t,m,data_qo[t,1],data_qs[t,m],zt[m]))
          
          zsum = zsum + yy
          #print(zsum)
        }
        
        ziter[k,t] = z/zsum
        if(is.na(ziter[k,t])){
          print(c(k,t,qo[t],qs[t,k],z,zsum))
          ziter[k,t] = 0.0
        }
        zzt = zzt + ziter[k,t]*(qo[t]-qs[t,k])^2
        #print(c(k,t,ziter[k,t]))
      }
      
      w[k] = mean(ziter[k,],na.rm = TRUE)
      zt[k] = zzt/sum(ziter[k,],na.rm = TRUE)
      #print(c(zsum,zzt))
    }
    #print(c(w[k],zt[k]))
    iter = iter +1
  }
  write.table(t(w), 'weights_BMA_max.txt', sep = "\t",
              quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE) 
  write.table(t(zt), 'weights_BMA_max.txt', sep = "\t",
              quote = FALSE, row.names = FALSE, col.names=FALSE,append=TRUE)
}
print('Done')
print(w)
print(zt)
