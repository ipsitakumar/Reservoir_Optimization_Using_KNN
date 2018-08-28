## FIXED POLICY

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## SCENARIOS 
setwd("C:/Users/Ipsita Kumar/Desktop/IDB Research Project/Reservoir Optimization Model for Pernambuco -- Data and code/src") ##

with_rioSF <- F  ## This is T if we have Rio Sao Francisco providing water F if it is not. Rio Sao Francisco represents future infrastructure.
nT <- 36  ## The number of months the model is running for 
nS <- 43  ## The number of ensemble runs we are doing for the optimization 
nTrucks <- 5  ## Number of trucks we are importing from
percent_init_reservoir <- c(0.5, 0.8, 0.8, 0.8, 0.8) ## This is the initial reservoir storage in percentage
nR <- 5 ## This is the number of reservoirs in the run
nM <- 19  ## This is the number of municipalities in the run
if(with_rioSF){
  nIMP <- nTrucks+1 ## This is the total number of import sources, with Rio Sao Francisco, we will have an additional import source
}else{
  nIMP <- nTrucks
}


## load position function
source("positionfunction.R")

## read inputs 
source("Input_data_All.R")
S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS)) ## This is the initial storage data
costF_rts <- array(data = 200, dim = c(nR, nT, nS))
source(("Fixed_Policy_Streamflow_Input.R"))

## generate matrix for optimization problem
source("generate_matrix_doublefailure.R")


## solving
require(lpSolve)
lpsol <- lp(direction = "min", objective.in=Obj, const.mat=A, const.dir=sense, const.rhs=rhs, compute.sens = TRUE)
xval <- lpsol$solution
objval <- lpsol$objval


## process the results
rQ=array(data=xval[index_Q],dim=c(nR,nM,nT))
rQIMP=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
rF=array(data=xval[index_F],dim=c(nR,nT,nS))
rS=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
rP=array(data=xval[index_releases],dim=c(nR,nT))
r2F=array(data=xval[index_2F],dim=c(nR,nT,nS))

rQfinal=array(data=0,dim=c(nR,nM,12))
rQIMPfinal=array(data=0,dim=c(nIMP,nM,12))
rFfinal=array(data=0,dim=c(nR,12,nS))
rSfinal=array(data=0,dim=c(nR,12,nS))
rPfinal=array(data=0,dim=c(nR,12))
r2Ffinal=array(data=0,dim=c(nR,12,nS))


rQfinal=rQ[,,13:24]
rQIMPfinal=rQIMP[,,13:24]
rFfinal=rF[,13:24,]
rSfinal=rS[,13:24,]
rPfinal=rP[,13:24]
r2Ffinal=r2F[,13:24,]

if (with_rioSF ==TRUE){
  if (sum(rQIMPfinal[1:5,,])>0){
    print ("THERE IS IMPORT FROM TRUCKS")
  } else {
    print ("THERE IS NO IMPORT FROM TRUCKS")
  }
}


for (year in 1:25){ ## This is where the fixed policy results is run from the optimization for the 25 years
  source("simulation fixed policy.R")
}

source("Fixed_Policy_Analysis.R")  ## In this case, we are creating plots for the runs


