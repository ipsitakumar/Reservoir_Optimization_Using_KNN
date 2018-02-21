## KNNƒ
setwd("/Users/ipsitakumar/Documents/GitHub/Reservoir_Optimization/src") ##
with_rioSF <- T
reservoirs_k=20
reservoirs_ns=10
chosensolver <- "lpsolve"
nT <- 24
nS <- reservoirs_ns
nTrucks <- 5


newknnrun<-FALSE
if(newknnrun){
  source("KNN_Simulation.R")
}else{
  load("../data/InflowJu.Rdata")
  load("../data/InflowMa.Rdata")
  load("../data/InflowEn.Rdata")
  load("../data/InflowPo.Rdata")
  load("../data/InflowPr.Rdata")
  load("../data/reservoirs.Rdata")
}

#reservoirs<-cbind(Jucaknn,Machaknn,Engknn,Pocoknn,Prataknn)


for (timeknn in 1:25){
 
  if (timeknn ==1){
    yearpredictingfor=1982+timeknn
    yearselected=yearpredictingfor-2
  }else{
    yearpredictingfor=1982+timeknn
    yearselected=yearpredictingfor-2
  }
  
  
  percent_init_reservoir <- c(0.5, 0.8, 0.8, 0.8, 0.8)
  source("positionfunction_5.R")
  if (timeknn ==1){
    source("input_data.R")
    ##Intial Storage
    S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS))
  }else{
    source("input_data.R")
    S_r0s = array(data = endstorage, dim = c(nR, nS))
  }
  
  source("generate_matrix_doublefailure_5.R")


  ## solving
  if(chosensolver == "lpsolve"){
    require(lpSolve)
    lpsol <- lp(direction = "min", objective.in=Obj, const.mat=A, const.dir=sense, const.rhs=rhs)
    xval <- lpsol$solution
    objval <- lpsol$objval
  }else{
    require(gurobi)
    model=list()
    model$A <- A
    model$obj <- Obj
    model$modelsense <- "min"
    model$rhs <- rhs
    model$sense <- sense
    result <- gurobi(model)
    objval <- result$objval
    xval <- result$x
  }
  
  rQ=array(data=xval[index_Q],dim=c(nR,nM,nT))
  rQIMP=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
  rF=array(data=xval[index_F],dim=c(nR,nT,nS))
  rS=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
  rP=array(data=xval[index_releases],dim=c(nR,nT))
  r2F=array(data=xval[index_2F],dim=c(nR,nT,nS))
  
  if (timeknn == 1){
    IniStor0sim = S_r0s[,1]
    source("simulation.R")
  }else{
    IniStor0sim=endstorage
    source("simulation.R")
  }
  endstorage=Storagesim[,12]
  print(timeknn)
  if (timeknn == 1){
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
  yearindex<-(1:12)+(timeknn-1)*12
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
  
  if(with_rioSF == TRUE){
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
  
  source("analysis_for_plots.R")
  
  }

