wetseason = function(d1,date1){
  m = length(seq(from=date1[1], to=date1[length(x)], by='year'))-1
  Pc1<-data.frame()
  wsl = data.frame()
  sumday1=0
  c1=0.1
  c2=0.9
  
  for (i in 1:m){
    m11=0
    m12=0
    yr1=as.numeric(format(date1[sumday1+1],'%Y'))
    
    if(((yr1+1)%%4)==0){
      lp11=366
    }else{
      lp11=365
    }
    if(((yr1)%%4)==0){
      lp12=274
    }else{
      lp12=273
    }
    
    for (j in 1:lp11){
      if(sum(d1[(lp12+sumday1+1):(lp12+sumday1+lp11)])>0){
        Pc1[i,j]=sum(d1[(lp12+sumday1+1):(lp12+sumday1+j)])/sum(d1[(lp12+sumday1+1):(lp12+sumday1+lp11)])
        
        if(Pc1[i,j]>=c1&m11==0){
          d_start1=j
          m11=1
        }
        if(Pc1[i,j]>=c2&m12==0){
          d_end1=j
          m12=1
        }
      }
      else{
        d_start1=NA
        d_end1=NA
        next
      }
      
    }
    
    sumday1=sumday1+lp11 
    outp=cbind(d_start1,d_end1)
    wsl = rbind(wsl, outp)
    #print(c(d_start1,d_end1))
  }
  
  cc=array()
  for (n in 1:2){
    cc[n]=mean(wsl[,n],na.rm=T)
  }
  return(cc)
}  