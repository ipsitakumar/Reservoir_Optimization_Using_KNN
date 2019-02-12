nT <- 24  ## The number of months the model is running for, 2 years for KNN
nS <- nScenarios  ## This is the number of ensembles chosen within the 20 K nearest neighbors and the number of ensemble runs we are doing for the optimization

reservoirs_k <- 20  ## This represents the k nearest neighbor in the KNN Run
newknnrun <- FALSE  ## Here we say it is TRUE if you want to run KNN again, FALSE if you want to use the previous data
startmonth_save <- startmonth

if(newknnrun){
  source("KNN_Running_KNN.R") 
}else{
  load("../data/InflowJu.Rdata")
  load("../data/InflowMa.Rdata")
  load("../data/InflowEn.Rdata")
  load("../data/InflowPo.Rdata")
  load("../data/InflowPr.Rdata")
  load("../data/reservoirs.Rdata")
}

source("inputData.R") ### Here, we are uploading all the cost, demand, connectivity, etc. data from the csv files.

## load position function
source("positionfunction.R")

for (year in 1:(nTotalTime/12)){
  # load the streamflow data
  inflowyearindex = (startmonth_save - 20*12) + (1:nT)+(year-1)*24
  I_rts<-array(data=0 , c(nR,nT,nS))
  I_rts[1,,] <- InflowJu[inflowyearindex,]*1e-6
  I_rts[2,,] <- InflowMa[inflowyearindex,]*1e-6
  I_rts[3,,] <- InflowEn[inflowyearindex,]*1e-6
  I_rts[4,,] <- InflowPo[inflowyearindex,]*1e-6
  I_rts[5,,] <- InflowPr[inflowyearindex,]*1e-6

  # initiate the reservoirs
  if (year == 1){
    S_r0s <- array(data = resmaxcapacity[,2] * 1e-6 * percent_init_reservoir, dim = c(nR, nS))
  }else{
    S_r0s = array(data = Storagesim[,12], dim = c(nR, nS))
  }
  
  ## generate matrix for optimization problem
  source("generate_matrix_doublefailure.R")
  ## solving
  source("solving.R")
  
  ## simulate
  if (year == 1){
    IniStor0sim <- S_r0s[,1]
  }else{
    IniStor0sim <- Storagesim[,12] # We are running KNN for every 2 years at the end of every year. So, our initial storage at the end is at the end of month 12
  }
  source("simulation.R")

  # par(mfrow=c(2,1))
  # for(rr in 1:1){
  #   plot(I_rts[rr,,1], ylim=c(0, 2*max(c(rF[rr,,],colSums(rQ[rr,,]),Storagesim[rr,]))), type = "l")
  #   for(ss in 1:nS){lines(I_rts[rr,,ss])
  #     lines((rF[rr,,ss]), col= "red", lwd=1)
  #     lines(rS[rr,,ss], col="pink")
  #   }
  #   lines(colSums(rQ[rr,,]), col= "orange", lwd=2)
  #   abline(h=S_r0s[rr,1], col="cyan")
  #   lines(SCmax_rt[rr,], col="brown")
  #   lines(Storagesim[rr,], col="blue")
  #   lines(Inflow[rr,], lty=3,lwd=3)
  # 
  # legend("topleft", legend=c("failure","withdrawals","inflow scenario","init storage","storage"), lty=1, col = c("red","orange","black","cyan","blue"))
  # }
  
  if(year==1){
    rQ_KNN <- withdrawalsim[,1:12]
    cQ_KNN <- apply(CostWithdrawalsim, MARGIN = c(1,3), sum)[,1:12]
    rQIMP_KNN <- apply(rQIMP, MARGIN = c(1,3), sum)[,1:12]
    cQIMP_KNN <- apply(CostImpsim, MARGIN = c(1,3), sum)[,1:12]
    rF_KNN <- Failuresim[,1:12]
    cF_KNN <- CostFailuresim[,1:12]
  }else{
    rQ_KNN <- cbind(rQ_KNN, withdrawalsim[,1:12])
    cQ_KNN <- cbind(cQ_KNN, apply(CostWithdrawalsim, MARGIN = c(1,3), sum)[,1:12])
    rQIMP_KNN <- cbind(rQIMP_KNN, apply(rQIMP, MARGIN = c(1,3), sum)[,1:12])
    cQIMP_KNN <- cbind(cQIMP_KNN, apply(CostImpsim, MARGIN = c(1,3), sum)[,1:12])
    rF_KNN <- cbind(rF_KNN, Failuresim[,1:12])
    cF_KNN <- cbind(cF_KNN, CostFailuresim[,1:12])
  }
  startmonth <- startmonth + 12
}

startmonth <- startmonth_save