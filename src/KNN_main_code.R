#setwd("/Users/ipsitakumar/Documents/GitHub/Reservoir_Optimization_Using_KNN_Corrrected/src") ##

with_rioSF <- T  ## This is T if we have Rio Sao Francisco providing water F if it is not. Rio Sao Francisco represents future infrastructure.
reservoirs_k=20  ## This represents the k nearest neighbor in the KNN Run
reservoirs_ns=10  ## This is the number of ensembles chosen within the 20 K nearest neighbors. 
nT <- 24  ## The number of months the model is running for 
nS <- reservoirs_ns  ## The number of ensemble runs we are doing for the optimization 
nTrucks <- 5  ## Number of trucks we are importing from
percent_init_reservoir <- c(0.5, 0.8, 0.8, 0.8, 0.8) ## This is the initial reservoir storage in percentage
nR <- 5 ## This is the number of reservoirs in the run
nM <- 19  ## This is the number of municipalities in the run
if(with_rioSF){
  nIMP <- nTrucks+1 ## This is the total number of import sources, with Rio Sao Francisco, we will have an additional import source
}else{
  nIMP <- nTrucks
}

newknnrun<-FALSE  ## Here we say it is TRUE if you want to run KNN again, FALSE if you want to use the previous data
if(newknnrun){
  source("KNN_Running_KNN.R") 
}else{
  load("../data/InflowJu.Rdata")
  load("../data/InflowMa.Rdata")
  load("../data/InflowEn.Rdata")
  load("../data/InflowPo.Rdata")
  load("../data/InflowPr.Rdata")
  load("../data/reservoirs.Rdata")
}

## load position function
source("positionfunction.R")

for (year in 1:25){  ## Here we are running the model for the past 25 years (i.e. 1983-2007)


  if (year ==1){
    source("Input_data_All.R") ### Here, we are uploading all the cost, demand, connectivity, etc. data from the csv files.
    source("KNN_Streamflow_Input.R") ## Here, we are converting the KNN streamflow runs into matrices for 
    ##Intial Storage
    S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS))
  }else{
    source("Input_data_All.R")
    S_r0s = array(data = endstorage, dim = c(nR, nS))
  }
  
  source("generate_matrix_doublefailure.R")


  ## solving
  require(lpSolve)
  lpsol <- lp(direction = "min", objective.in=Obj, const.mat=A, const.dir=sense, const.rhs=rhs, compute.sens = TRUE)
  xval <- lpsol$solution
  objval <- lpsol$objval
  duality <-lpsol$duals #### This is the code to find the duality
  
  rQ=array(data=xval[index_Q],dim=c(nR,nM,nT))   ### The withdrawals from all reservoirs to all municipalities
  rQIMP=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))  ## Withdrawals from all import sources to all municipalities
  rF=array(data=xval[index_F],dim=c(nR,nT,nS))  ## Failure at each reservoir for each ensemble forecast
  rS=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))  ## Storage for each reservoir
  rP=array(data=xval[index_releases],dim=c(nR,nT))  ## Releases from each reservoir
  r2F=array(data=xval[index_2F],dim=c(nR,nT,nS))  ## Failure 2 -- Ignore this **** Laureline, can you figure out a better way to write this?
  
  KNN=TRUE
  if (year == 1){   ### In this case, we are using the optimization results to run with the actual streamflows in the past 25 years,
    IniStor0sim = S_r0s[,1]
    source("Simulation.R")
  }else{
    IniStor0sim=endstorage
    source("Simulation.R")
  }
  
  endstorage=Storagesim[,12]  #We are running KNN for every 2 years at the end of every year. So, our initial storage at the end is at the end of month 12
  
  print(year)
  
  if (year == 1){  ### Here we are getting the results from the run for each of the 25 runs conducted.
    rQfinalKNN=array(data=0,dim=c(nR,nM,nT*25/2))
    rQIMPfinalKNN=array(data=0,dim=c(nIMP,nM,nT*25/2))
    rFfinalKNN=array(data=0,dim=c(nR,nT*25/2,nS))
    rSfinalKNN=array(data=0,dim=c(nR,nT*25/2,nS))
    rPfinalKNN=array(data=0,dim=c(nR,nT*25/2))
    r2FfinalKNN=array(data=0,dim=c(nR,nT*25/2,nS))
    FailureKNNsimfinal<-array(data=0 , c(totalressim,timesim*25))
    StorageKNNsimfinal<-array(data=0 , c(totalressim,timesim*25))
    withdrawalKNNsimfinal<-array(data=0,c(5,300))
    releaseKNNsimfinal=array(data=0,c(totalressim,timesim*25))
    Importsimknn<-array(data=0,c(1,timesim*25))
    OneFailureKNNsimfinal<-array(data=0,c(1,timesim*25))
    ImportsimRioSFknn<-array(data=0, c(1,timesim*25))
    
    }
  yearindex<-(1:12)+(year-1)*12
  rQfinalKNN[,,yearindex]=rQ[,,1:12]
  rQIMPfinalKNN[,,yearindex]=rQIMP[,,1:12]
  rFfinalKNN[,yearindex,]=rF[,1:12,]
  rSfinalKNN[,yearindex,]=rS[,1:12,]
  rPfinalKNN[,yearindex]=rP[,1:12]
  r2FfinalKNN[,yearindex,]=r2F[,1:12,]
  FailureKNNsimfinal[,yearindex]=Failurewithdrawalsim[,1:12]
  StorageKNNsimfinal[,yearindex]=Storagesim[,1:12]
  withdrawalKNNsimfinal[,yearindex]<-withdrawalsim[,1:12]
  releaseKNNsimfinal[,yearindex]<-releasesim[,1:12]
  TotalDDKNN<-sum(D_mt[,1])
  DemandmetKNN<-(colSums(FailureKNNsimfinal)+colSums(withdrawalKNNsimfinal))
  DemandmetKNN<-array(DemandmetKNN,c(1,timesim*25))
  TESTTotalDDKNN<-sum(D_mt[,1])
  TESTDemandmetKNN<-colSums(withdrawalKNNsimfinal)
  TESTDemandmetKNN<-array(TESTDemandmetKNN,c(1,timesim*25))
  
  TESTDDEF<-as.matrix(TESTTotalDDKNN-TESTDemandmetKNN)
  
  if(with_rioSF == TRUE){  ### Here we are finding the withdrawals from the trucks, and from Rio Sao Francisco
    for (time in 1:300){
      if (TESTDDEF[,time]<=1.31){
        ImportsimRioSFknn[,time]<-(TESTDDEF[,time])
        Truckssimknn<-(colSums(colSums(rQIMPfinalKNN[1:nTrucks,,])))
        OneFailureKNNsimfinal[,time]<-(TESTDDEF[,time])-ImportsimRioSFknn[,time]
      }else{
        ImportsimRioSFknn[,time]<-1.31
        Truckssimknn<-(colSums(colSums(rQIMPfinalKNN[1:nTrucks,,])))
        OneFailureKNNsimfinal[,time]<-(TESTDDEF[1,time])-ImportsimRioSFknn[1,time]
      }
    }
  } else {
    Truckssimknn<- (TotalDDKNN - DemandmetKNN)
  }
  
  source("KNN_Analysis.R")  ## In this case, we are creating plots for the runs
  
}
