Inflow<-array(data=0 , c(nR,nT))
Inflow[1,]<-Juca[(541-nT):540]*1e-6
Inflow[2,]<-Macha[(541-nT):540]*1e-6
Inflow[3,]<-Eng[(541-nT):540]*1e-6
Inflow[4,]<-Poco[(541-nT):540]*1e-6
Inflow[5,]<-Prata[(541-nT):540]*1e-6


Initpercentsim<-percent_init_reservoir
IniStor0sim<- Initpercentsim*SCmax_rt[,1]

rQsim <- apply(rQ,MARGIN=c(1,3),sum)
evap<-e_rts[,,1]
Failuresim<-array(data=0 , c(nR,nT))
Storagesim<-array(data=0 , c(nR,nT))
FailurerPsim<-array(data=0 , c(nR,nT))
FailurerQsim<-array(data=0 , c(nR,nT))

resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata") # you don't use this anywhere

###

# you ahve twice, more or less, exactly the same code: lines 56-76 and lines 79-102
for (res in 1:nR){
  Storagesim[res,1] <- (1-evap[res,1])*IniStor0sim[res]+Inflow[res,1]-rQsim[res,1]-rP[res,1]
  
  if (Storagesim[res,1 ] >= 0){
    Failuresim[res,1] <- 0
  }else{
    Failuresim[res,1] <- - Storagesim[res,1]
    Storagesim[res,1] <- 0
    if (rP[res,1] > Failuresim[res,1]){
      FailurerPsim[res,1] <- Failuresim[res,1]
      rP[res,1] <- rP[res,1] - Failuresim[res,1]
      FailurerQsim[res,1] <- 0
    }else{
      FailurerPsim[res,1] <- rP[res,1]
      rP[res,1] <- 0
      FailurerQsim[res,1] <- Failuresim[res,1]-FailurerPsim[res,1]
      if (rQsim[res,1]>=FailurerQsim[res,1]){
        rQsim[res,1] <- rQsim[res,1] - FailurerQsim[res,1]
      } else{
        print("SOMETHING IS WRONG -- ERROR")
      }}}
}


for (res in 1:nR){
  for (t in 2:nT){
    Storagesim[res,t] <- (1-evap[res,t])*Storagesim[res,t-1]+Inflow[res,t]-rQsim[res,t]-rP[res,t]
    if (Storagesim[res,t] >= 0){
      Failuresim[res,t] <- 0
    }else{
      Failuresim[res,t] <- - Storagesim[res,t]
      Storagesim[res,t] <- 0
      if (rP[res,t] > Failuresim[res,t]){
        FailurerPsim[res,t] <- Failuresim[res,t]
        rP[res,t] <- rP[res,t] - Failuresim[res,t]
        FailurerQsim[res,1] <- 0
      }else{
        FailurerPsim[res,t] <- rP[res,t]
        rP[res,t] <- 0
        FailurerQsim[res,t] <- Failuresim[res,t]-FailurerPsim[res,t]
        if (rQsim[res,t]>=FailurerQsim[res,t]){
          rQsim[res,t] <- rQsim[res,t] - FailurerQsim[res,t]
        } else{
          print("SOMETHING IS WRONG -- ERROR")
          print(paste(res,nT))
        }}}
  }
}

TotalDD<-sum(D_mt[,1])
Demandmet<-(colSums(Failuresimfinal)+colSums(rQsim))



if(with_rioSF == TRUE){
  Importsim<-(TotalDD-Demandmet)
  Trucks<-colSums(colSums(rQIMP[1:nTrucks,,]))
  ImportsimRioSF<-Importsim - Trucks
  Imp_All<-rbind(ImportsimRioSF, Trucks)
} else {
  Trucks<- (TotalDD - Demandmet)
  Imp_All<-Trucks
}
