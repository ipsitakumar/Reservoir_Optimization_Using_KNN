timesim<-12
totalressim<-5
resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata")
Jucafixed<-read.csv("../data/Juca.csv",header=T)
Jucafixed<-as.matrix(Jucafixed[1:12,],nrow=12,ncol=45)

Jucafixed<-Jucafixed
Machafixed<-read.csv("../data/Macha.csv",header=T)
Machafixed<-as.matrix(Machafixed,nrow=24,ncol=45)
Engfixed<-read.csv("../data/Eng.csv",header=T)
Engfixed<-as.matrix(Engfixed,nrow=24,ncol=45)
Pocofixed<-read.csv("../data/Poco.csv",header=T)
Pocofixed<-as.matrix(Pocofixed,nrow=24,ncol=45)
Pratafixed <- read.csv("../data/Prata.csv",header=T)
Pratafixed<-as.matrix(Pratafixed,nrow=24,ncol=45)

yearselectedfixedpolicy<-1982+fixedpolicytime
whichyearfixed <- paste0("X",yearselectedfixedpolicy)
whichcol <- which(whichyearfixed == colnames(Jucafixed))
col <- 1:dim(Jucafixed)[2]
colselect <- whichcol
Jucafixed1<-Jucafixed[,colselect]
Machafixed1<-Machafixed[,colselect]
Engfixed1<-Engfixed[,colselect]
Pocofixed1<-Pocofixed[,colselect]
Pratafixed1<-Pratafixed[,colselect]

Inflow<-array(data=0 , c(totalressim,timesim))
Inflow[1,]<-Jucafixed1[1:timesim]*1e-6
Inflow[2,]<-Machafixed1[1:timesim]*1e-6
Inflow[3,]<-Engfixed1[1:timesim]*1e-6
Inflow[4,]<-Pocofixed1[1:timesim]*1e-6
Inflow[5,]<-Pratafixed1[1:timesim]*1e-6

Initpercentsim<-percent_init_reservoir
IniStor0sim<- Initpercentsim*SCmax_rt[,1]

withdrawalsim<-apply(rQfinal,MARGIN=c(1,3),sum)

releasesim<-rPfinal
evap<-e_rts
Failuresim<-array(data=0 , c(totalressim,timesim))
Storagesim<-array(data=0 , c(totalressim,timesim))
Failurereleasesim<-array(data=0 , c(totalressim,timesim))
Failurewithdrawalsim<-array(data=0 , c(totalressim,timesim))

for (res in 1:totalressim){

  if(fixedpolicytime == 1){
  Storagesim[res,1] <- (1-evap[res,1,s])*IniStor0sim[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
  }else{
    Storagesim[res,1] <- (1-evap[res,1,s])*initialstorage[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
  }
  
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
}


for (res in 1:totalressim){
  for (t in 2:timesim){
    Storagesim[res,t] <- (1-evap[res,t,s])*Storagesim[res,t-1]+Inflow[res,t]-withdrawalsim[res,t]-releasesim[res,t]
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
          print(paste(res,timesim))
        }}}
  }
}
initialstorage<-Storagesim[,timesim]

if (fixedpolicytime == 1){
  Failuresimfinal<-array(data=0 , c(totalressim,timesim*25))
  Storagesimfinal<-array(data=0 , c(totalressim,timesim*25))
  withdrawalsimfinal<-array(data=0,c(5,300))
  releasesimfinal=array(data=0,c(totalressim,timesim*25))
  Importsim<-array(data=0,c(1,timesim*25))
}
yearindexfixed<-(1:12)+(fixedpolicytime-1)*12
Failuresimfinal[,yearindexfixed]=Failurewithdrawalsim
Storagesimfinal[,yearindexfixed]=Storagesim
withdrawalsimfinal[,yearindexfixed]<-withdrawalsim
releasesimfinal[,yearindexfixed]<-releasesim
TotalDD<-sum(D_mt[,1])
Demandmet<-(colSums(Failuresimfinal)+colSums(withdrawalsimfinal))
Demandmet<-array(Demandmet,c(1,timesim*25))

withdrawalsimJuca<-withdrawalsimfinal[1,]
withdrawalsimothers<-colSums(withdrawalsimfinal[2:5,])
withdrawalsimallres<-colSums(withdrawalsimfinal)
FailuresimallFP<-colSums(Failuresimfinal)



if(with_rioSF == TRUE){
  Importsim[1,]<-(TotalDD-Demandmet)
  Trucks<-rep((colSums(colSums(rQIMPfinal[1:nTrucks,,]))), 25)
  ImportsimRioSF<-Importsim - Trucks
} else {
  Trucks<- (TotalDD - Demandmet)
}

