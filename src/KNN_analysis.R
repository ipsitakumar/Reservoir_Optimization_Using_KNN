
if(with_rioSF == TRUE){
  pdf(file=paste("../results/results from KNN Sim with RSF_",percent_init_reservoir,"initstorage for 25 years.pdf"),onefile=TRUE,width = 12,height=6)
}else{
  pdf(file=paste("../results/results from KNN Sim without RSF_",percent_init_reservoir,"initstorage for 25 years.pdf"),onefile=TRUE,width = 12,height=6)
}


for(r in 1:nR){ ### This is the plot for the storage over time for all reservoirs
  plot(rSfinalKNN[r,,1],type="l",ylim=c(min(rSfinalKNN[r,,]),max(rSfinalKNN[r,,])),ylab="storage [mio m3]",xlab = "months")
  for(s in 2:nS){lines(rSfinalKNN[r,,s])}
  abline(h=SCmax_rt[r,1],col="red")
  title(paste(resname[r], "reservoir"))
}

sumfailuresim<-sum(Failuresim)
## In this case, we are making a historgram of the amount of failure in the nS ensembles
hist(colSums(colSums(rFfinalKNN)),xlab = "Failure [mio m3]",main = " ", xlim = c(0 , 120))
abline(v=mean(colSums(colSums(rFfinalKNN))),col="red",lwd=3)
abline(v=quantile(colSums(colSums(rFfinalKNN)),probs = c(0.1,0.5,0.9)),col=c(3:nR),lwd=3,lty=2:4)
abline(v=sumfailuresim,col=6,lwd=3,lty=5)
legend("topleft",c("Mean","10th Percentile","Median","90th Percentile","Simulated Failure"), col=c(2:(nR+1)),lty=1:5,lwd=3,cex=1,bty="n")
if(with_rioSF == TRUE){
  title(paste0("Distribution failure for 25 years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"),cex.main=1)
} else {
  title(paste0("Distribution failure for 25 years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"), cex.main=1)
}

### This is the plot for the total inflows coming into the reservoirs over time
for(r in 1:nR){ 
  plot(I_rts[r,,1],type="l",ylim=c(min(I_rts[r,,]),max(I_rts[r,,])),ylab="storage [mio m3]",xlab = "months")
  for(s in 2:nS){lines(I_rts[r,,s])}
  abline(h=SCmax_rt[r,1],col="red")
  title(paste("inflow reservoir", resname[r]))
}


## This is the plot of the amount of failure 
plot(colSums(rFfinalKNN[,,1]),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(rFfinalKNN[,,])),max(colSums(rFfinalKNN[,,]))))
for(s in 2:nS){lines(colSums(rFfinalKNN[,,s]))}
title("Withdrawal failure")



mLOC=apply(rQfinalKNN,MARGIN=c(1,3),sum)
mIMP=apply(rQIMPfinalKNN,MARGIN=c(1,3),sum)
mALLres<-colSums(mLOC)
if(with_rioSF==TRUE){
  imp<-mIMP[1:nTrucks,]
  imp<-apply(imp,MARGIN=c(2),sum)
  Jucazinho<-mLOC[1,]
  otherreservoirs<-mLOC[2:nR,]
  otherreservoirs<-apply(otherreservoirs,MARGIN=c(2),sum)
  riosf<-mIMP[nIMP,]
  withdrawaloptim<-rbind(Jucazinho,otherreservoirs,imp,riosf)
}else{
  imp<-mIMP[1:nTrucks,]
  imp<-apply(imp,MARGIN=c(2),sum)
  Jucazinho<-mLOC[1,]
  otherreservoirs<-mLOC[2:nR,]
  otherreservoirs<-apply(otherreservoirs,MARGIN=c(2),sum)
  withdrawaloptim<-rbind(Jucazinho,otherreservoirs,imp)
}
if(with_rioSF==TRUE){
  AB<-4
}else{
  AB=3
}

rFmedian<-apply(rFfinalKNN,MARGIN=c(1,2),median)
rFmedianJuca<-rFmedian[1,]
rFmedianothers<-colSums(rFmedian[2:nR,])
rFmedianAll<-colSums(rFmedian[1:nR,])
rFall<-colSums(rFfinalKNN)
rFallmedian<-apply(rFall,1,median)
withdrawalsimJuca<-withdrawalKNNsimfinal[1,]
withdrawalsimothers<-colSums(withdrawalKNNsimfinal[2:nR,])
withdrawalsimALL<-colSums(withdrawalKNNsimfinal[1:nR,])
Failuresimulated<-colSums(FailureKNNsimfinal)

## Here we are making plots for withdrawal from reservoirs, import source and failure.
plot((withdrawalsimALL),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
if (with_rioSF == TRUE){
  lines(ImportsimRioSFknn[1,], col = 2,lwd=3,lty=3)
}
if (with_rioSF == TRUE){
  lines(OneFailureKNNsimfinal[1,], col=3, lwd=3,lty=6)
}else{
  lines(Failuresimulated, col=3, lwd=3,lty=6)
}
if(with_rioSF == TRUE){
  legend("topleft", c("","All Reservoirs","Rio Sao Francisco","Failure"),  col = c(0,1,2,3),lty=c(0,1,2,3),pt.cex=0.5,lwd=3, cex=0.75,bty="n")
}else{
  legend("topleft",c("","All Reservoirs Sim","Failure"), col = c(0,1,3),pt.cex=0.5,lwd=3, lty=c(0,1,3), cex=0.75,bty="n")
}
if(with_rioSF == TRUE){
  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
} else {
  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
}

dev.off()

## Here we get the results from the costs for the different sources of income.
if(with_rioSF==TRUE){
  rQLOCAL<-rQ[,,1:12]
  rQimport<-rQIMP[1:nTrucks,,1:12]
  rQRSF<-rQIMP[nTrucks+1,,1:12]
  Failure=colSums(rF)
  Failure<-apply(Failure,1,median)
  rFfinalmedian<-apply(rFall,1,median)
  costQ<-costQ_rmt
  costImport <- costIMP_jmt[1:nTrucks,,]
  costRSF<-costIMP_jmt[nTrucks+1,,]
  costFail <- 20
  
  Failuresim_one<-Failurewithdrawalsim[,1:12]
  RioSFsim_one<-ImportsimRioSFknn[,1:12]
  Truckssim_one<-Truckssimknn[1:12]
  
  TotLOCALCost=sum(rQLOCAL*costQ[,,1:12])
  TotTRUCKCost=sum(rQimport*costImport[,,1:12])
  TotRSFCost=sum(rQRSF*costRSF[,1:12])
  TotalDEFICITCost=sum(Failure*costFail)
  #LOCALsimCost=sum(withdrawalsim_one*costQ[,1:12])
  FailuresimCost=sum(Failuresim_one*costFail)
  RioSFsimCost=sum(RioSFsim_one*costRSF[,1:12])
  TruckssimCost=sum(Truckssim_one*costImport[,,1:12])
  TOTALCOST=TotLOCALCost+TotTRUCKCost+TotRSFCost+TotalDEFICITCost
  
  print(TotLOCALCost)
  print(TotTRUCKCost)
  print(TotRSFCost)
  print(TotalDEFICITCost)
  print(TOTALCOST)
  COST<-rbind(TotLOCALCost,TotTRUCKCost,TotRSFCost,TotalDEFICITCost,TOTALCOST,FailuresimCost,RioSFsimCost,TruckssimCost) #LOCALsimCost,not includes
#  write.csv(COST,file = paste0("TT/Each year cost with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage in year ", yearpredictingfor,".csv"))
}else{ 
  rQLOCAL<-rQ[,,1:12]
  rQimport<-rQIMP[1:nTrucks,,1:12]
  Failure=colSums(rF)
  Failure<-apply(Failure,1,median)
  rFfinalmedian<-apply(rFall,1,median)
  costQ<-costQ_rmt
  costImport <- costIMP_jmt[1:nTrucks,,]
  costFail <- 20
  
  Failuresim_one<-Failurewithdrawalsim[,1:12]
  Truckssim_one<-Truckssimknn[1:12]

  TotLOCALCost=sum(rQLOCAL*costQ[,,1:12])
  TotTRUCKCost=sum(rQimport*costImport[,,1:12])
  TotalDEFICITCost=sum(Failure*costFail)
  FailuresimCost=sum(Failuresim_one*costFail)
  TruckssimCost=sum(Truckssim_one*costImport[,,1:12])
  TOTALCOST=TotLOCALCost+TotTRUCKCost+TotalDEFICITCost
  
  print(TotLOCALCost)
  print(TotTRUCKCost)
  print(TotalDEFICITCost)
  print(TOTALCOST)
  COST<-rbind(TotLOCALCost,TotTRUCKCost,TotalDEFICITCost,TOTALCOST,FailuresimCost,TruckssimCost) #LOCALsimCost,not includes
  } ### this 4 next lines have to be changed too.

if (year == 1){
  if (with_rioSF==1){
    COSTFINAL<-array(data=0,c(8,25)) 
  }else{
    COSTFINAL<-array(data=0,c(6,25)) 
  }
}

COSTFINAL[,year]<-COST[,1]