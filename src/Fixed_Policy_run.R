## FIXED POLICY

nT <- 36  ## The number of months the model is running for 
nS <- 43  ## The number of ensemble runs we are doing for the optimization 

source("inputData.R")

# load the streamflows
for (ss in 1:nS){
  indexthree <- 1:nT+(ss-1)*12
  
  I_rts[1,,ss] <- Juca[indexthree]
  I_rts[2,,ss] <- Macha[indexthree]
  I_rts[3,,ss] <- Eng[indexthree]
  I_rts[4,,ss] <- Poco[indexthree]
  I_rts[5,,ss] <- Prata[indexthree]
}

## load position function
source("positionfunction.R")

## generate matrix for optimization problem
source("generate_matrix_doublefailure.R")

## solving
source("solving.R")

# adapting and loading input parameters for the simulation
rQ <- rQ[,,13:24]
rQIMP <- rQIMP[,,13:24]
rP <- rP[,13:24]
e_rts <- e_rts[,13:24,]
costF_rts <- costF_rts[,13:24,]
costIMP_jmt <- costIMP_jmt[,,13:24]
costQ_rmt <- costQ_rmt[,,13:24]

nT <- 12
startmonthsave <- startmonth
IniStor0sim <- S_r0s[,1]
for (year in 1:(nTotalTime/12)){ ## This is where the fixed policy results is run from the optimization for the 25 years
  source("simulation.R")
  if(year==1){
    rQ_FP <- withdrawalsim[,1:12]
    cQ_FP <- apply(CostWithdrawalsim, MARGIN = c(1,3), sum)[,1:12]
    rQIMP_FP <- apply(rQIMP, MARGIN = c(1,3), sum)[,1:12]
    cQIMP_FP <- apply(CostImpsim, MARGIN = c(1,3), sum)[,1:12]
    rF_FP <- Failuresim[,1:12]
    cF_FP <- CostFailuresim[,1:12]
  }else{
    rQ_FP <- cbind(rQ_FP, withdrawalsim[,1:12])
    cQ_FP <- cbind(cQ_FP, apply(CostWithdrawalsim, MARGIN = c(1,3), sum)[,1:12])
    rQIMP_FP <- cbind(rQIMP_FP, apply(rQIMP, MARGIN = c(1,3), sum)[,1:12])
    cQIMP_FP <- cbind(cQIMP_FP, apply(CostImpsim, MARGIN = c(1,3), sum)[,1:12])
    rF_FP <- cbind(rF_FP, Failuresim[,1:12])
    cF_FP <- cbind(cF_FP, CostFailuresim[,1:12])
  }
  startmonth <- startmonth + 12
  IniStor0sim <- Storagesim[,12]
}

startmonth <- startmonthsave