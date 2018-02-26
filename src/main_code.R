## KNNƒ <- what is this?

# The start of this main part of the code could actually be moved to the input. The idea is that the parameters that the users can play with are grouped in one place. The inputs that are specific to a test case are in another script, while all of the other files are not to be touched by anyone. This is what should be clarified in this code. 

with_rioSF <- T # TRUE if the Rio San Francisco project is to be considered, FALSE for the current situation
nT <- 24 # number of months for decision making
nS <- reservoirs_ns # why is there twice the same variable? 
nTrucks <- 5 # number of trucks for deliveries
newknnrun<-FALSE # If you want to run KNN again, you write TRUE. If you want to use the old KNN run for the 25 years, FALSE
reservoirs_k <- 20
reservoirs_ns <- 10
percent_init_reservoir <- c(0.5, 0.8, 0.8, 0.8, 0.8) # this is the initial reservoir storage for the 5 reservoirs

if(newknnrun){
  source("KNN_Simulation.R")
}else{ 
  load("../data/InflowJu.Rdata") # KNN was run for the 25 years previously, and the results are stored in the Data section
  load("../data/InflowMa.Rdata") # If you do not want to run it again, you can upload the data
  load("../data/InflowEn.Rdata")
  load("../data/InflowPo.Rdata")
  load("../data/InflowPr.Rdata")
  load("../data/reservoirs.Rdata")
}

source("positionfunction.R") # renamed and also should be outside of the loop !


for (timeknn in 1:25){ #timeknn is the number of years you are running the model for. The model in this case is run for 25 years
  
  if (timeknn ==1){
    yearpredictingfor = 1982+timeknn
    yearselected = yearpredictingfor-2
  }else{
    yearpredictingfor = 1982+timeknn
    yearselected = yearpredictingfor-2
  }
  
  
  
  
  
  if (timeknn ==1){
    source("input_data.R") # i am pretty sure we need this to be run before the position function are generated. Also, it can be moved outside of the loop, except fo rthe streamflows, which should be in a different files. You could have the three set-ups in the streamflow script and then the user would decide which streamflow scenario they want to run. 
    ##Intial Storage
    S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS))
  }else{
    source("input_data.R") # again, if it is in both "if" parts, it should not be in it =)
    S_r0s <- array(data = endstorage, dim = c(nR, nS))
  }
  
  source("generate_matrix_doublefailure.R") # renamed
  
  
  ## solving
  require(lpSolve)
  lpsol <- lp(direction = "min", objective.in = Obj, const.mat = A, const.dir = sense, const.rhs = rhs)
  xval <- lpsol$solution
  objval <- lpsol$objval
  
  rQ <- array(data = xval[index_Q],dim = c(nR,nM,nT))
  rQIMP <- array(data = xval[index_IMP],dim = c(nIMP,nM,nT))
  rF <- array(data = xval[index_F],dim = c(nR,nT,nS))
  rS <- array(data = xval[index_reservoirs],dim = c(nR,nT,nS))
  rP <- array(data = xval[index_releases],dim = c(nR,nT))
  r2F <- array(data = xval[index_2F],dim = c(nR,nT,nS))
  
  if (timeknn == 1){ # this is specific to the KNN set-up, what about the others? i am confused
    IniStor0sim <- S_r0s[,1]
    source("simulation.R")
  }else{
    IniStor0sim <- endstorage
    source("simulation.R") # running simulation should be out of the if ...
  }
  
  endstorage <- Storagesim[,12]
  print(timeknn)
  if (timeknn == 1){ # this is just declaring the arrays, no need to have inside the loop
    rQfinalKNN <- array(data = 0,dim = c(nR,nM,nT*25/2))
    rQIMPfinalKNN <- array(data = 0,dim = c(nIMP,nM,nT*25/2))
    rFfinalKNN <- array(data = 0,dim = c(nR,nT*25/2,nS))
    rSfinalKNN <- array(data = 0,dim = c(nR,nT*25/2,nS))
    rPfinalKNN <- array(data = 0,dim = c(nR,nT*25/2))
    r2FfinalKNN <- array(data = 0,dim = c(nR,nT*25/2,nS))
    FailureKNNsimfinal <- array(data = 0 , c(totalressim,timesim*25))
    StorageKNNsimfinal <- array(data = 0 , c(totalressim,timesim*25))
    withdrawalKNNsimfinal <- array(data = 0,c(5,300))
    releaseKNNsimfinal <- array(data = 0,c(totalressim,timesim*25))
    Importsimknn <- array(data = 0,c(1,timesim*25))
    OneFailureKNNsimfinal <- array(data = 0,c(1,timesim*25))
    ImportsimRioSFknn <- array(data = 0, c(1,timesim*25))
    
  }
  
  # all of this is KNN specific ... I am not sure it should be in the main file. 
  yearindex <- (1:12)+(timeknn-1)*12
  rQfinalKNN[,,yearindex] <- rQ[,,1:12]
  rQIMPfinalKNN[,,yearindex] <- rQIMP[,,1:12]
  rFfinalKNN[,yearindex,] <- rF[,1:12,]
  rSfinalKNN[,yearindex,] <- rS[,1:12,]
  rPfinalKNN[,yearindex] <- rP[,1:12]
  r2FfinalKNN[,yearindex,] <- r2F[,1:12,]
  FailureKNNsimfinal[,yearindex] <- Failurewithdrawalsim[,1:12]
  StorageKNNsimfinal[,yearindex] <- Storagesim[,1:12]
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
      if (TESTDDEF[,time]<=1.31){ # I am confused as to what this is
        ImportsimRioSFknn[,time]<-TESTDDEF[,time]
        Truckssimknn<-(colSums(colSums(rQIMPfinalKNN[1:nTrucks,,])))
        OneFailureKNNsimfinal[,time]<-TESTDDEF[,time]-ImportsimRioSFknn[,time]
      }else{
        ImportsimRioSFknn[,time]<-1.31
        Truckssimknn<-(colSums(colSums(rQIMPfinalKNN[1:nTrucks,,]))) # this line seems to be the same as in the other part of the if
        OneFailureKNNsimfinal[,time]<-TESTDDEF[1,time]-ImportsimRioSFknn[1,time]
      }
    }
  } else {
    Truckssimknn<- (TotalDDKNN - DemandmetKNN)
  }
  source("analysis_for_plots.R")
}

