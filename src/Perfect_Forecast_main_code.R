### PERFECT FORECAST
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## SCENARIOS 
setwd("C:/Users/Ipsita Kumar/Desktop/IDB Research Project/Reservoir Optimization Model for Pernambuco -- Data and code/src")

with_rioSF <- F ## This is T if we have Rio Sao Francisco providing water F if it is not. Rio Sao Francisco represents future infrastructure.
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
costF_rts <- array(data = 7.1, dim = c(nR, nT, nS))
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
rQ=array(data=xval[index_Q],dim=c(nR,nM,nT))
rQIMP=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
rF=array(data=xval[index_F],dim=c(nR,nT,nS))
rS=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
rP=array(data=xval[index_releases],dim=c(nR,nT))
r2F=array(data=xval[index_2F],dim=c(nR,nT,nS))

IniStor0sim = S_r0s[,1]
source("Perfect_Forecast_Simulation_1.R")


rQPF=withdrawalsim
rQIMPPF=Imp_All
rFPF=Failurewithdrawalsim
rSPF=Storagesim
rPPF=releasesim

source("Perfect_Forecast_analysis.R")   ## In this case, we are creating plots for the runs

