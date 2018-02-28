## FIXED POLICY

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## SCENARIOS 
setwd("~/Google Drive/IDB Project/Latest Model with APAC Cost Data/cleaned code - Optimization Model")

renewmatrices <- TRUE
chosensolver <- "lpsolve"

nT <- 36
nS <- 43
nTrucks <- 5

percent_init_reservoir <- c(0, 0, 0, 0, 0)
with_rioSF <- T


## load position function
source("positionfunction_5.R")

## read inputs 
source("input_data_Fixed_Policy.R")
source(("Streamflow_run_Fixed_Policy.R"))

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


for (fixedpolicytime in 1:25){
  source("simulation_Fixed_Policy.R")
}

source("Analysis_Fixed_Policy.R")


if (with_rioSF == TRUE){
  withdrawalFP<-withdrawalsimfinal
  costwithdrawal<-c(1.05,0.04,0.65,0.98,1.38)
  RioSFFP<-ImportsimRioSF
  costrioSF<-1.67
  FailureFP<-FailuresimallFP
  Failurecost<-20
  withdrawalFPCost<-withdrawalFP*costwithdrawal
  RioSFFPCost<-RioSFFP*costrioSF
  FailureFPCost<-FailureFP*Failurecost
  TOTALCOSTFPF<-rbind(withdrawalFPCost, RioSFFPCost, FailureFPCost)
  FPFINAL<-rbind(withdrawalFP, RioSFFP,FailureFP)
  write.csv(TOTALCOSTFPF,file = paste0("../Results/FINAL COSTS FP with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
  write.csv(FPFINAL,file = paste0("../Results/FINAL AMT FP with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}else{
  withdrawalFP<-withdrawalsimfinal
  costwithdrawal<-c(1.05,0.04,0.65,0.98,1.38)
  costrioSF<-1.67
  FailureFP<-FailuresimallFP
  Failurecost<-20
  withdrawalFPCost<-withdrawalFP*costwithdrawal
  FailureFPCost<-FailureFP*Failurecost
  TOTALCOSTFPFin<-rbind(withdrawalFPCost, FailureFPCost)
  FPFINAL<-rbind(withdrawalFP,FailureFP)
  write.csv(TOTALCOSTFPFin,file = paste0("../Results/FINAL COSTS FP without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
  write.csv(FPFINAL,file = paste0("../Results/FINAL AMT FP without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}

