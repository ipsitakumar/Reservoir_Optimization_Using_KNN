### PERFECT FORECAST
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## SCENARIOS 
setwd("/Users/ipsitakumar/Documents/GitHub/Reservoir_Optimization_Using_KNN_Corrrected/src")

with_rioSF <- T  ## This is T if we have Rio Sao Francisco providing water F if it is not. Rio Sao Francisco represents future infrastructure.
nT <- 25*12  ## The number of months the model is running for 
nS <- 1  ## The number of ensemble runs we are doing for the optimization 
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
source("Perfect_Forecast_Streamflow_Input.R")

## generate matrix for optimization problem
source("generate_matrix_doublefailure.R")

## solving
require(lpSolve)
lpsol <- lp(direction = "min", objective.in=Obj, const.mat=A, const.dir=sense, const.rhs=rhs)
xval <- lpsol$solution
objval <- lpsol$objval


## process the results
rQPF=array(data=xval[index_Q],dim=c(nR,nM,nT))
rQIMPPF=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
rFPF=array(data=xval[index_F],dim=c(nR,nT,nS))
rSPF=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
rPPF=array(data=xval[index_releases],dim=c(nR,nT))
r2FPF=array(data=xval[index_2F],dim=c(nR,nT,nS))

source("Perfect_Forecast_analysis.R")   ## In this case, we are creating plots for the runs

