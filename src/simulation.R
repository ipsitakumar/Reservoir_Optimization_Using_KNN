require(data.table)
whichyearsim <- paste0("X",yearpredictingfor,".",(yearpredictingfor+1))
whichrowsim <- which(whichyearsim == rownames(reservoirs))
rowssim <- 1:dim(reservoirs)[1]
simulationyear <- whichrowsim
timesim<-12
totalressim<-5
resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata")
Juca<-read.csv("../data/Juca.csv",header=T)
Juca<-as.matrix(Juca,nrow=24,ncol=45)
Macha<-read.csv("../data/Macha.csv",header=T)
Macha<-as.matrix(Macha,nrow=24,ncol=45)
Eng<-read.csv("../data/Eng.csv",header=T)
Eng<-as.matrix(Eng,nrow=24,ncol=45)
Poco<-read.csv("../data/Poco.csv",header=T)
Poco<-as.matrix(Poco,nrow=24,ncol=45)
Prata <- read.csv("../data/Prata.csv",header=T)
Prata<-as.matrix(Prata,nrow=24,ncol=45)
Inflow<-array(data=0 , c(totalressim,timesim))
Inflow[1,]<-Juca[1:timesim,simulationyear]*1e-6
Inflow[2,]<-Macha[1:timesim,simulationyear]*1e-6
Inflow[3,]<-Eng[1:timesim,simulationyear]*1e-6
Inflow[4,]<-Poco[1:timesim,simulationyear]*1e-6
Inflow[5,]<-Prata[1:timesim,simulationyear]*1e-6

Juca_stream <- Juca[1:12,21:45]*1e-6
Juca_stream<-melt(Juca_stream)
Juca_stream<-Juca_stream[,3]
Macha_stream <-Macha[1:12,21:45]*1e-6
Macha_stream<-melt(Macha_stream)
Macha_stream<-Macha_stream[,3]
Eng_stream <-Eng[1:12,21:45]*1e-6
Eng_stream<-melt(Eng_stream)
Eng_stream<-Eng_stream[,3]
Poco_stream <-Poco[1:12,21:45]*1e-6
Poco_stream<-melt(Poco_stream)
Poco_stream<-Poco_stream[,3]
Prata_stream <-Prata[1:12,21:45]*1e-6
Prata_stream<-melt(Prata_stream)
Prata_stream<-Prata_stream[,3]

withdrawalsim<-apply(rQ,MARGIN=c(1,3),sum)
withdrawalsim<-withdrawalsim[,1:timesim]

releasesim<-rP
evap<-e_rts
Failuresim<-array(data=0 , c(totalressim,timesim))
Failurereleasesim<-array(data=0 , c(totalressim,timesim))
Failurewithdrawalsim<-array(data=0 , c(totalressim,timesim))
Storagesim<-array(data=0 , c(totalressim,timesim))


for (res in 1:totalressim){
  Storagesim[res,1] <- (1-evap[res,1,s])*IniStor0sim[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
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
