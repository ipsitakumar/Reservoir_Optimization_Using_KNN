# this code needs commenting and dividing into broad sections

require(data.table)
# HEAD
if (year ==1){
  yearpredictingfor=1982+year
  yearselected=yearpredictingfor-2
}else{
  yearpredictingfor=1982+year
  yearselected=yearpredictingfor-2
}


resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata")
Juca_sim<-read.csv("../data/Juca.csv",header=T)
Juca_sim<-as.matrix(Juca_sim,nrow=24,ncol=45)
Macha_sim<-read.csv("../data/Macha.csv",header=T)
Macha_sim<-as.matrix(Macha_sim,nrow=24,ncol=45)
Eng_sim<-read.csv("../data/Eng.csv",header=T)
Eng_sim<-as.matrix(Eng_sim,nrow=24,ncol=45)
Poco_sim<-read.csv("../data/Poco.csv",header=T)
Poco_sim<-as.matrix(Poco_sim,nrow=24,ncol=45)
Prata_sim <- read.csv("../data/Prata.csv",header=T)
Prata_sim<-as.matrix(Prata_sim,nrow=24,ncol=45)
whichyear <- paste0("X",yearpredictingfor)
whichcol <- which(whichyear == colnames(Juca_sim))
col <- 1:dim(Juca_sim)[2]
colselect <- whichcol
timesim<-12
totalressim<-5


if(KNN == TRUE){
  Inflow<-array(data=0 , c(totalressim,timesim))
  Inflow[1,]<-Juca_sim[1:timesim,colselect]*1e-6
  Inflow[2,]<-Macha_sim[1:timesim,colselect]*1e-6
  Inflow[3,]<-Eng_sim[1:timesim,colselect]*1e-6
  Inflow[4,]<-Poco_sim[1:timesim,colselect]*1e-6
  Inflow[5,]<-Prata_sim[1:timesim,colselect]*1e-6
  
  Juca_stream <- Juca_sim[1:12,21:45]*1e-6
  Juca_stream<-melt(Juca_stream)
  Juca_stream<-Juca_stream[,3]
  Macha_stream <-Macha_sim[1:12,21:45]*1e-6
  Macha_stream<-melt(Macha_stream)
  Macha_stream<-Macha_stream[,3]
  Eng_stream <-Eng_sim[1:12,21:45]*1e-6
  Eng_stream<-melt(Eng_stream)
  Eng_stream<-Eng_stream[,3]
  Poco_stream <-Poco_sim[1:12,21:45]*1e-6
  Poco_stream<-melt(Poco_stream)
  Poco_stream<-Poco_stream[,3]
  Prata_stream <-Prata_sim[1:12,21:45]*1e-6
  Prata_stream<-melt(Prata_stream)
  Prata_stream<-Prata_stream[,3]
  
  withdrawalsim<-apply(rQ,MARGIN=c(1,3),sum)
  withdrawalsim<-withdrawalsim[,1:timesim]
  releasesim<-rP
  
}else{
  Juca_sim1<-Juca_sim[,colselect]
  Macha_sim1<-Macha_sim[,colselect]
  Eng_sim1<-Eng_sim[,colselect]
  Poco_sim1<-Poco_sim[,colselect]
  Prata_sim1<-Prata_sim[,colselect]
  
  Inflow<-array(data=0 , c(totalressim,timesim))
  Inflow[1,]<-Juca_sim1[1:timesim]*1e-6
  Inflow[2,]<-Macha_sim1[1:timesim]*1e-6
  Inflow[3,]<-Eng_sim1[1:timesim]*1e-6
  Inflow[4,]<-Poco_sim1[1:timesim]*1e-6
  Inflow[5,]<-Prata_sim1[1:timesim]*1e-6
  
  Initpercentsim<-percent_init_reservoir
  IniStor0sim<- Initpercentsim*SCmax_rt[,1]
  withdrawalsim<-apply(rQfinal,MARGIN=c(1,3),sum)
  releasesim<-rPfinal
}

evap<-e_rts
Failuresim<-array(data=0 , c(totalressim,timesim))
Storagesim<-array(data=0 , c(totalressim,timesim))
Failurereleasesim<-array(data=0 , c(totalressim,timesim))
Failurewithdrawalsim<-array(data=0 , c(totalressim,timesim))
#
whichyearsim <- paste0("X",yearpredictingfor,".",(yearpredictingfor+1))
whichrowsim <- which(whichyearsim == rownames(reservoirs))
rowssim <- 1:dim(reservoirs)[1]
simulationyear <- whichrowsim
timesim <- 12
totalressim <- 5
resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata") # you don't use this anywhere
Juca <- read.csv("../data/Juca.csv",header=T) # I think here you are re-reading data that you already needed for the optimization - am I wrong'? What you can at least do is not re-reading things. This is lines 11 to 42, it's quite a lot
Juca <- as.matrix(Juca,nrow=24,ncol=45)
Macha <- read.csv("../data/Macha.csv",header=T)
Macha <- as.matrix(Macha,nrow=24,ncol=45)
Eng <- read.csv("../data/Eng.csv",header=T)
Eng <- as.matrix(Eng,nrow=24,ncol=45)
Poco <- read.csv("../data/Poco.csv",header=T)
Poco <- as.matrix(Poco,nrow=24,ncol=45)
Prata <- read.csv("../data/Prata.csv",header=T)
Prata <- as.matrix(Prata,nrow=24,ncol=45)
Inflow <- array(data=0 , c(totalressim,timesim))
Inflow[1,] <- Juca[1:timesim,simulationyear]*1e-6
Inflow[2,] <- Macha[1:timesim,simulationyear]*1e-6
Inflow[3,] <- Eng[1:timesim,simulationyear]*1e-6
Inflow[4,] <- Poco[1:timesim,simulationyear]*1e-6
Inflow[5,] <- Prata[1:timesim,simulationyear]*1e-6

Juca_stream <- Juca[1:12,21:45]*1e-6
Juca_stream <- melt(Juca_stream)
Juca_stream <- Juca_stream[,3]
Macha_stream <- Macha[1:12,21:45]*1e-6
Macha_stream <- melt(Macha_stream)
Macha_stream <- Macha_stream[,3]
Eng_stream <- Eng[1:12,21:45]*1e-6
Eng_stream <- melt(Eng_stream)
Eng_stream <- Eng_stream[,3]
Poco_stream <- Poco[1:12,21:45]*1e-6
Poco_stream <- melt(Poco_stream)
Poco_stream <- Poco_stream[,3]
Prata_stream <- Prata[1:12,21:45]*1e-6
Prata_stream <- melt(Prata_stream)
Prata_stream <- Prata_stream[,3]

withdrawalsim <- apply(rQ,MARGIN=c(1,3),sum)
withdrawalsim <- withdrawalsim[,1:timesim]

releasesim <- rP
evap <- e_rts # you can use e_rts directly
Failuresim <- array(data=0 , c(totalressim,timesim))
Failurereleasesim <- array(data=0 , c(totalressim,timesim))
Failurewithdrawalsim <- array(data=0 , c(totalressim,timesim))
Storagesim <- array(data=0 , c(totalressim,timesim))
###

# you ahve twice, more or less, exactly the same code: lines 56-76 and lines 79-102
for (res in 1:totalressim){
  
  if(KNN == TRUE){
    Storagesim[res,1] <- (1-evap[res,1,s])*IniStor0sim[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
  }else{
    if(year == 1){
        Storagesim[res,1] <- (1-evap[res,1,s])*IniStor0sim[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
      }else{
        Storagesim[res,1] <- (1-evap[res,1,s])*initialstorage[res]+Inflow[res,1]-withdrawalsim[res,1]-releasesim[res,1]
  }
  }
  
  if (Storagesim[res,1] >= 0){
    Failuresim[res,1] <- 0
  }else{
    Failuresim[res,1] <- - Storagesim[res,1]
    Storagesim[res,1] <- 0
    if (releasesim[res,1] > Failuresim[res,1]){
      Failurereleasesim[res,1] <- Failuresim[res,1]
      releasesim[res,1] <- releasesim[res,1] - Failuresim[res,1]
      Failurewithdrawalsim[res,1] <- 0
    }else{
      Failurereleasesim[res,1] <- releasesim[res,1]
      releasesim[res,1] <- 0
      Failurewithdrawalsim[res,1] <- Failuresim[res,1]-Failurereleasesim[res,1]
      if (withdrawalsim[res,1]>=Failurewithdrawalsim[res,1]){
        withdrawalsim[res,1] <- withdrawalsim[res,1] - Failurewithdrawalsim[res,1]
      } else{
        print("SOMETHING IS WRONG -- ERROR")
      }}}
}


for (res in 1:totalressim){
  for (t in 2:timesim){
    Storagesim[res,t] <- (1-evap[res,t,s])*Storagesim[res,t-1]+Inflow[res,t]-withdrawalsim[res,t]-releasesim[res,t]
    if (Storagesim[res,t] >= 0){
      Failuresim[res,t] <- 0
    }else{
      Failuresim[res,t] <- - Storagesim[res,t]
      Storagesim[res,t] <- 0
      if (releasesim[res,t] > Failuresim[res,t]){
        Failurereleasesim[res,t] <- Failuresim[res,t]
        releasesim[res,t] <- releasesim[res,t] - Failuresim[res,t]
        Failurewithdrawalsim[res,1] <- 0
      }else{
        Failurereleasesim[res,t] <- releasesim[res,t]
        releasesim[res,t] <- 0
        Failurewithdrawalsim[res,t] <- Failuresim[res,t]-Failurereleasesim[res,t]
        if (withdrawalsim[res,t]>=Failurewithdrawalsim[res,t]){
          withdrawalsim[res,t] <- withdrawalsim[res,t] - Failurewithdrawalsim[res,t]
        } else{
          print("SOMETHING IS WRONG -- ERROR")
          print(paste(res,timesim))
        }}}
  }
}


    
    if(KNN == FALSE){
      initialstorage<-Storagesim[,timesim]
      
      if (year == 1){
        Failuresimfinal<-array(data=0 , c(totalressim,timesim*25))
        Storagesimfinal<-array(data=0 , c(totalressim,timesim*25))
        withdrawalsimfinal<-array(data=0,c(5,300))
        releasesimfinal=array(data=0,c(totalressim,timesim*25))
        Importsim<-array(data=0,c(1,timesim*25))
      }
      yearindexfixed<-(1:12)+(year-1)*12
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
      
      
    }
