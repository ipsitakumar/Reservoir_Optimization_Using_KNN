# SIMULATING THE WATER ALLOCATION STRATEGY'S PERFORMANCES
# loading the true streamflows
Inflow<-array(data=0 , c(nR,nT))
Inflow[1,] <- Juca[startmonth+(1:nT)]
Inflow[2,] <- Macha[startmonth+(1:nT)]
Inflow[3,] <- Eng[startmonth+(1:nT)]
Inflow[4,] <- Poco[startmonth+(1:nT)]
Inflow[5,] <- Prata[startmonth+(1:nT)]

# loading input parameters for the simulation
withdrawalsim <- apply(rQ, MARGIN=c(1,3), sum)
releasesim <- rP
evap <- e_rts[,,1]

# initialisation of the variables to be simulated
Failuresim <- array(data=0 , c(nR,nT))
Storagesim <- array(data=0 , c(nR,nT))
Failurereleasesim <- array(data=0 , c(nR,nT))
Failurewithdrawalsim <- array(data=0 , c(nR,nT))

# simulation
for (res in 1:nR){
  for(t in 1:nT){
    if(t == 1){
      Storagesim[res,t] <- (1-evap[res,t])*IniStor0sim[res]+Inflow[res,t]-withdrawalsim[res,t]-releasesim[res,t]
    }else{
      Storagesim[res,t] <- (1-evap[res,t])*Storagesim[res,t-1]+Inflow[res,t]-withdrawalsim[res,t]-releasesim[res,t]
    }
    if(!is.na(Storagesim[res,t])){ 
      if(Storagesim[res,t] > SCmax_rt[res,t]){
        Failuresim[res,t] <- 0
        releasesim[res,t] <- releasesim[res,t] + Storagesim[res,t] - SCmax_rt[res,t]
        Storagesim[res,t] <- SCmax_rt[res,t]
      }else if(Storagesim[res,t] >= -1e-14){
      Failuresim[res,t] = 0
    }else{
      Failuresim[res,t]<- - Storagesim[res,t]
      Storagesim[res,t]<- 0
      if (releasesim[res,t] > Failuresim[res,t]){
        Failurereleasesim[res,t] <- Failuresim[res,t]
        releasesim[res,t] <- releasesim[res,t] - Failuresim[res,t]
        Failurewithdrawalsim[res,1] <- 0
      }else{
        Failurereleasesim[res,t] <- releasesim[res,t]
        releasesim[res,t] <- 0
        Failurewithdrawalsim[res,t] <- Failuresim[res,t]-Failurereleasesim[res,t]
        if (withdrawalsim[res,t] >= Failurewithdrawalsim[res,t]){
          withdrawalsim[res,t]<- withdrawalsim[res,t] - Failurewithdrawalsim[res,t]
        }
      }
    }
  }}
}


## COMPUTE THE COST CORRESPONDING THE ALLOCATION STRATEGY
CostFailuresim <- costF_rts[,1:nT,1]*Failuresim
CostImpsim <- rQIMP*costIMP_jmt
CostWithdrawalsim <- array(data=0 , c(nR,nM,nT))
for(rr in 1:nR){for(mm in 1:nM){for(tt in 1:nT){ # spreading evenly failure at the reservoir level to the municipalities
  if(withdrawalsim[rr,tt]>0){
  CostWithdrawalsim[rr,mm,tt] <- (withdrawalsim[rr,tt]/(Failurewithdrawalsim[rr,tt]+withdrawalsim[rr,tt]))*rQ[rr,mm,tt]*costQ_rmt[rr,mm,tt]
}}}}

