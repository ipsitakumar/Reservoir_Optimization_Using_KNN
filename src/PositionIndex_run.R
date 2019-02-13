nT <- 24  ## The number of months the model is running for, 2 years for position index
nS <- 45-nT/12*2  ## This is taking all of the years in the input in sequence, minus the ones that we are currently simulating.
startmonth_save <- startmonth


source("inputData.R") ### Here, we are uploading all the cost, demand, connectivity, etc. data from the csv files.
## load position function
source("positionfunction.R")

for (year in 1:(nTotalTime/12)){
  # load the streamflow data
  I_rts<-array(data=0 , c(nR,nT,nS))
  indexminusyear <- (1:(45*12))[-(startmonth+1:nT)]
  for(ss in 1:nS){
    inflowyearindex = indexminusyear[(1:nT)+(ss-1)*12]
    #print((inflowyearindex[1]-1)/12)
    I_rts[1,,ss] <- Juca[inflowyearindex]
    I_rts[2,,ss] <- Macha[inflowyearindex]
    I_rts[3,,ss] <- Eng[inflowyearindex]
    I_rts[4,,ss] <- Poco[inflowyearindex]
    I_rts[5,,ss] <- Prata[inflowyearindex]
  }

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
    IniStor0sim <- Storagesim[,12] # We are running PI for every 2 years at the end of every year. So, our initial storage at the end is at the end of month 12
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
    rQ_PI <- withdrawalsim[,1:12]
    cQ_PI <- apply(CostWithdrawalsim, MARGIN = c(1,3), sum)[,1:12]
    rQIMP_PI <- apply(rQIMP, MARGIN = c(1,3), sum)[,1:12]
    cQIMP_PI <- apply(CostImpsim, MARGIN = c(1,3), sum)[,1:12]
    rF_PI <- Failuresim[,1:12]
    cF_PI <- CostFailuresim[,1:12]
  }else{
    rQ_PI <- cbind(rQ_PI, withdrawalsim[,1:12])
    cQ_PI <- cbind(cQ_PI, apply(CostWithdrawalsim, MARGIN = c(1,3), sum)[,1:12])
    rQIMP_PI <- cbind(rQIMP_PI, apply(rQIMP, MARGIN = c(1,3), sum)[,1:12])
    cQIMP_PI <- cbind(cQIMP_PI, apply(CostImpsim, MARGIN = c(1,3), sum)[,1:12])
    rF_PI <- cbind(rF_PI, Failuresim[,1:12])
    cF_PI <- cbind(cF_PI, CostFailuresim[,1:12])
  }
  startmonth <- startmonth + 12
}

startmonth <- startmonth_save
