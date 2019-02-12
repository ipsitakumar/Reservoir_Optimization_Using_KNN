# SYSTEM DIMENSIONS
nR <- 5 ## This is the number of reservoirs in the run
nM <- 19  ## This is the number of municipalities in the run
if(with_rioSF){
  nIMP <- nTrucks+1 ## This is the total number of import sources, with Rio Sao Francisco, we will have an additional import source
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

# EVAPORATION AND LOSSES PROPORTIONAL TO STORED VOLUME
evaporation <- read.csv("../data/Eits.csv",header=T)
e_rts <- array(data = evaporation[,2], dim = c(nR, nT, nS))

# INITIALIZATION OF RESERVOIRS
percent_init_reservoir <- c(0.5, 0.8, 0.8, 0.8, 0.8) ## This is the initial reservoir storage in percentage
S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS)) ## This is the initial storage data

# RESERVOIR NAMES
resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata")


#__________________________________________________________________________________________________
## IMPORT CONSTRAINTS
# max imports per import source at each time step
IMPmax_jt <- array(data = 0.6*1e-3, dim = c(nIMP, nT))
if(with_rioSF==TRUE){
  IMPmax_jt[nIMP,] <- 1.31 #in mio m3
}


#__________________________________________________________________________________________________
## STREAMFLOW DATA
require(data.table)
I_rts <- array(NA, dim = c(nR, nT, nS))
Juca<-read.csv("../data/Juca.csv",header=T)
Juca<-as.matrix(Juca,nrow=24,ncol=45)
Juca<-Juca[1:12,]
Juca<-melt(Juca)
Juca<-Juca[,3]*1e-6

Macha<-read.csv("../data/Macha.csv",header=T)
Macha<-as.matrix(Macha,nrow=24,ncol=45)
Macha<-Macha[1:12,]
Macha<-melt(Macha)
Macha<-Macha[,3]*1e-6

Eng<-read.csv("../data/Eng.csv",header=T)
Eng<-as.matrix(Eng,nrow=24,ncol=45)
Eng<-Eng[1:12,]
Eng<-melt(Eng)
Eng<-Eng[,3]*1e-6

Poco<-read.csv("../data/Poco.csv",header=T)
Poco<-as.matrix(Poco,nrow=24,ncol=45)
Poco<-Poco[1:12,]
Poco<-melt(Poco)
Poco<-Poco[,3]*1e-6

Prata <- read.csv("../data/Prata.csv",header=T)
Prata<-as.matrix(Prata,nrow=24,ncol=45)
Prata<-Prata[1:12,]
Prata<-melt(Prata)
Prata<-Prata[,3]*1e-6