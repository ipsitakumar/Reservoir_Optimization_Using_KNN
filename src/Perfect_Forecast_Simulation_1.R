Inflow<-array(data=0 , c(nR,nT))
Inflow[1,]<-Juca[(541-nT):540]*1e-6
Inflow[2,]<-Macha[(541-nT):540]*1e-6
Inflow[3,]<-Eng[(541-nT):540]*1e-6
Inflow[4,]<-Poco[(541-nT):540]*1e-6
Inflow[5,]<-Prata[(541-nT):540]*1e-6

withdrawalsim<-apply(rQ,MARGIN=c(1,3),sum)
releasesim<-rP
evap<-e_rts[,,1]
Failuresim<-array(data=0 , c(nR,nT))
Storagesim<-array(data=0 , c(nR,nT))
Failurereleasesim<-array(data=0 , c(nR,nT))
Failurewithdrawalsim<-array(data=0 , c(nR,nT))
Initpercentsim<-percent_init_reservoir
IniStor0sim<- Initpercentsim*SCmax_rt[,1]


for (res in 1:nR){
    Storagesim[res,1] <- (1-evap[res,1])*IniStor0sim[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
    if (Storagesim[res,1] >= 0){
      Failuresim[res,1] = 0
    }else{
      Failuresim[res,1]<- - Storagesim[res,1]
      Storagesim[res,1]<-0
      if (releasesim[res,1] > Failuresim[res,1]){
        Failurereleasesim[res,1]<-Failuresim[res,1]
        releasesim[res,1]= releasesim[res,1] - Failuresim[res,1]
        Failurewithdrawalsim[res,1]=0
      }else{
        Failurereleasesim[res,1]=releasesim[res,1]
        releasesim[res,1]=0
        Failurewithdrawalsim[res,1]= Failuresim[res,1]-Failurereleasesim[res,1]
        if (withdrawalsim[res,1]>=Failurewithdrawalsim[res,1]){
          withdrawalsim[res,1]<- withdrawalsim[res,1] - Failurewithdrawalsim[res,1]
        } else{
          print("SOMETHING IS WRONG -- ERROR")
        }}}
    for(t in 2:nT){
      Storagesim[res,t] <- (1-evap[res,t])*Storagesim[res,t-1]+Inflow[res,t]-withdrawalsim[res,t]-releasesim[res,t]
      if (Storagesim[res,t] >= 0){
        Failuresim[res,t] = 0
      }else{
        Failuresim[res,t]<- - Storagesim[res,t]
        Storagesim[res,t]<-0
        if (releasesim[res,t] > Failuresim[res,t]){
          Failurereleasesim[res,t]<-Failuresim[res,t]
          releasesim[res,t]= releasesim[res,t] - Failuresim[res,t]
          Failurewithdrawalsim[res,1]=0
        }else{
          Failurereleasesim[res,t]=releasesim[res,t]
          releasesim[res,t]=0
          Failurewithdrawalsim[res,t]= Failuresim[res,t]-Failurereleasesim[res,t]
          if (withdrawalsim[res,t]>=Failurewithdrawalsim[res,t]){
            withdrawalsim[res,t]<- withdrawalsim[res,t] - Failurewithdrawalsim[res,t]
          } else{
            print("SOMETHING IS WRONG -- ERROR")
            print(paste(res,nT))
          }}}
    }
}


TotalDD<-sum(D_mt[,1])
Demandmet<-(colSums(Failurewithdrawalsim)+colSums(withdrawalsim))
Importsim<-array(data=0,c(1,nT))

if(with_rioSF == TRUE){
  Importsim[1,]<-(TotalDD-Demandmet)
  Trucks<-colSums(colSums(rQIMP[1:nTrucks,,]))
  ImportsimRioSF<-Importsim - Trucks
  Imp_All<-rbind(ImportsimRioSF, Trucks)
} else {
  Trucks<- (TotalDD - Demandmet)
  Imp_All<-  Trucks
}





