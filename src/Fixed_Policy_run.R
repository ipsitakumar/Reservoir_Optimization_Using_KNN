## FIXED POLICY

nT <- nTotalTime  ## The number of months the model is running for 
nS <- 1  ## The number of ensemble runs we are doing for the optimization 

source("inputData.R")

# load the streamflows
idx <- 1:nT+startmonth
I_rts<-array(data=0, c(nR,nT,nS))
I_rts[1,,] <- Juca[idx]
I_rts[2,,] <- Macha[idx]
I_rts[3,,] <- Eng[idx]
I_rts[4,,] <- Poco[idx]
I_rts[5,,] <- Prata[idx]

## load position function
source("positionfunction.R")

## generate matrix for optimization problem
source("generate_matrix_fixed_policy.R")

## solving
source("solving.R")

## simulate
IniStor0sim <- percent_init_reservoir*SCmax_rt[,1]
source("simulation.R")

## summarizing the results
rQ_FP <- withdrawalsim
cQ_FP <- CostWithdrawalsim
rQIMP_FP <- apply(rQIMP, MARGIN = c(1,3), sum)
cQIMP_FP <- apply(CostImpsim, MARGIN = c(1,3), sum)
rF_FP <- Failuresim
cF_FP <- CostFailuresim

