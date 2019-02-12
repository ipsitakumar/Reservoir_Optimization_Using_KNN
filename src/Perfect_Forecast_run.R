# Perfect forecast run
nT <- nTotalTime # running everything at once
nS <- 1 # with only the true inflows

source("inputData.R")

# load the streamflows
I_rts<-array(data=0 , c(nR,nT,nS))
I_rts[1,,]<-Juca[startmonth+(1:nT)]
I_rts[2,,]<-Macha[startmonth+(1:nT)]
I_rts[3,,]<-Eng[startmonth+(1:nT)]
I_rts[4,,]<-Poco[startmonth+(1:nT)]
I_rts[5,,]<-Prata[startmonth+(1:nT)]

## load position function
source("positionfunction.R")

## generate matrix for optimization problem
source("generate_matrix_doublefailure.R")

## solving
source("solving.R")

## simulate
IniStor0sim <- percent_init_reservoir*SCmax_rt[,1]
source("simulation.R")

## summarizing the results
rQ_PF <- withdrawalsim
cQ_PF <- CostWithdrawalsim
rQIMP_PF <- apply(rQIMP, MARGIN = c(1,3), sum)
cQIMP_PF <- apply(CostImpsim, MARGIN = c(1,3), sum)
rF_PF <- Failuresim
cF_PF <- CostFailuresim

