##PERFECT FORECAST
## PROBLEM DIMENSIONS
nR <- 5 
nM <- 19
if(with_rioSF){
  nIMP <- nTrucks+1
}else{
  nIMP <- nTrucks
}

#__________________________________________________________________________________________________
## COSTS OF WATER SOURCES
if(with_rioSF){
  costcsv<-read.csv("../data/Cost_with.csv",header=T)
}else{
  costcsv<-read.csv("../data/Cost_without.csv",header=T)
}
cost<-as.matrix(costcsv[,2:(nM+1)])
# cost from reservoirs to municipalities
costQ_rmt <- array(data = cost[1:nR, 1:nM], dim = c( nR, nM, nT))
# cost of imports to municipalities
costIMP_jmt <- array(data = cost[nR + 1:nIMP, 1:nM], dim = c(nIMP, nM, nT))
#cost of reservoir failure
costF_rts <- array(data = 20, dim = c(nR, nT, nS))

#__________________________________________________________________________________________________
## MUNICIPAL DEMANDS
TotalDemand <- read.csv("../data/Total_Demand.csv",header=T)
D_mt <- array(data = TotalDemand[,2], dim = c(nM, nT)) * 1e-6 #in mio m3

#__________________________________________________________________________________________________
## CONNECTIVITY MATRICES
# RESERVOIRS TO MUNICIPALITIES
m_connectivity <- read.csv("../data/mij.csv",header=T)
Mrm <- m_connectivity[1:nR, 2:(nM+1)] 
# IMPORTS TO MUNICIPALITIES
Mjm <- m_connectivity[nR + 1:nIMP, 2:(nM+1)]
# RESERVOIRS NETWORK
r_connectivity <- read.csv("../data/mii.csv",header=T)
Mrr <- r_connectivity[1:nR, 1+1:nR]


#__________________________________________________________________________________________________
## RELEASES/CHANNELS CONSTRAINTS
with_Pconstraints <- FALSE
#PCmin_rt <- array(0, dim = c(nR, nT))
#PCmax_rt <- array(Inf, dim = c(nR, nT))

#__________________________________________________________________________________________________
## RESERVOIRS
# RESERVOIR CAPACITY CONSTRAINTS
with_Smin_constraints <- FALSE
#SCmin_rt <- array(data=0,dim=(c(nR,nT)))
with_Smax_constraints <- TRUE
resmaxcapacity <- read.csv("../data/SCMax.csv",header=T)
SCmax_rt<-array(data = resmaxcapacity[,2] * 1e-6, dim = c(nR, nT))

# INITIAL STORAGE
S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS))

# EVAPORATION AND LOSSES PROPORTIONAL TO STORED VOLUME
evaporation <- read.csv("../data/Eits.csv",header=T)
e_rts <- array(data = evaporation[,2], dim = c(nR, nT, nS))


#__________________________________________________________________________________________________
## IMPORT CONSTRAINTS
# max imports per import source at each time step
IMPmax_jt <- array(data = 0.6*1e-3, dim = c(nIMP, nT))
if(with_rioSF==TRUE){
  IMPmax_jt[nIMP,] <- 1.31 #in mio m3
}
