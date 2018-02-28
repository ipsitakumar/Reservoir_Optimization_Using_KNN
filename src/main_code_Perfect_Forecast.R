### PERFECT FORECAST
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## SCENARIOS 
setwd("~/Google Drive/IDB Project/Latest Model with APAC Cost Data/cleaned code - Optimization Model")

renewmatrices <- TRUE
chosensolver <- "lpsolve"

nT <- 25*12
nS <- 1
nTrucks <- 5

percent_init_reservoir <- c(0.5, 0.8, 0.8, 0.8, 0.8)
with_rioSF <- F


## load position function
source("positionfunction_5.R")

## read inputs 
source("input_data_Perfect_Forecast.R")

source("Streamflow_data_Perfect_Forecast.R")

## generate matrix for optimization problem
if(renewmatrices){
  source("generate_matrix_doublefailure_5.R")
  source("save_matrix_failure_5.R")
}else{
  source("load_matrix_failure_5.R")
}

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


## process the results
rQPF=array(data=xval[index_Q],dim=c(nR,nM,nT))
rQIMPPF=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
rFPF=array(data=xval[index_F],dim=c(nR,nT,nS))
rSPF=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
rPPF=array(data=xval[index_releases],dim=c(nR,nT))
r2FPF=array(data=xval[index_2F],dim=c(nR,nT,nS))

source("analysis_Perfect_Forecast.R")

if (with_rioSF == TRUE){
  withdrawalPF<-mLOC
  costwithdrawal<-c(1.05,0.04,0.65,0.98,1.38)
  RioSFPF<-riosf
  costrioSF<-1.67
  FailurePF<-t(colSums(rFPF))
  Failurecost<-20
  withdrawalPFCost<-withdrawalPF*costwithdrawal
  RioSFPFCost<-RioSFPF*costrioSF
  FailurePFCost<-FailurePF*Failurecost
  TOTALCOSTPF<-rbind(withdrawalPFCost, RioSFPFCost, FailurePFCost)
  PFFINAL<-rbind(withdrawalPF, RioSFPF,FailurePF)
  write.csv(TOTALCOSTPF,file = paste0("../Results/FINAL COSTS Perfect with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
  write.csv(PFFINAL,file = paste0("../Results/FINAL AMT Perfect with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}else{
  withdrawalPF<-mLOC
  costwithdrawal<-c(1.05,0.04,0.65,0.98,1.38)
  FailurePF<-t(colSums(rFPF))
  Failurecost<-20
  withdrawalPFCost<-withdrawalPF*costwithdrawal
  FailurePFCost<-FailurePF*Failurecost
  TOTALCOSTPF<-rbind(withdrawalPFCost, FailurePFCost)
  PFFINAL<-rbind(withdrawalPF,FailurePF)
  write.csv(TOTALCOSTPF,file = paste0("../Results/FINAL COSTS Perfect without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
  write.csv(PFFINAL,file = paste0("../Results/FINAL AMT Perfect without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}
