
if(with_rioSF == TRUE){
  pdf(file=paste("../Results/Fixed Policy Results for all years (for ", nT/12, " years) ", " with Rio Sao Francisco and ", costF_rts[1,1,1],"Failure Cost.pdf"),onefile=TRUE,width = 12,height=6)
}else{
  pdf(file=paste("../Results/Fixed Policy Results for all years (for ", nT/12, " years) ", " without Rio Sao Francisco and ", costF_rts[1,1,1],"Failure Cost.pdf"),onefile=TRUE,width = 12,height=6)
}

### This is the plot for the storage over time for all reservoirs
for(r in 1:5){   
  plot(Storagesimfinal[r,],type="l",ylim=c(min(Storagesimfinal[r,]),max(Storagesimfinal[r,])),ylab="storage [mio m3]",xlab = "months")
  abline(h=SCmax_rt[r,1],col="red")
  title(paste(resname[r], "reservoir"))
}

### This is the plot for the total inflows coming into the reservoirs over time
for(r in 1:5){  
  plot(I_rts[r,,1],type="l",ylim=c(min(I_rts[r,,]),max(I_rts[r,,])),ylab="storage [mio m3]",xlab = "months")
  for(s in 2:nS){lines(I_rts[r,,s])}
  abline(h=SCmax_rt[r,1],col="red")
  title(paste("inflow reservoir", resname[r]))
}


## This is the plot of the amount of failure 
plot(colSums(Failuresimfinal),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(Failuresimfinal[,])),max(colSums(Failuresimfinal[,]))))
title("Withdrawal failure")



## This is the plot for the withdrawal from the reservoirs and import sources, and deifict
plot((withdrawalsimallres),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
if (with_rioSF == TRUE){
  lines(Importsim[1,], col = 2,lwd=2,lty=3)}
lines(colSums(Failuresimfinal), col=3, lwd=4,lty=6)
if(with_rioSF == TRUE){
  legend("topleft", c("", "All Reservoirs","Rio Sao Francisco","Failure"),  col = c(0,1,2,3),lty=(1:6),pt.cex=0.5,lwd=2, cex=0.75,bty="n")
}else{
  legend("topleft",c("", "All Reservoirs","Failure"), col = c(0,1,3),pt.cex=0.5,lwd=2, lty=(1:6), cex=0.75,bty="n")
}
if(with_rioSF == TRUE){
  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
} else {
  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
}

dev.off()



### We are getting the results for the costs from here
if(with_rioSF==TRUE){
  rQimport<-(colSums(rQIMPfinal[1:nTrucks,,1:12]))
  rQRSF<-rQIMPfinal[nTrucks+1,,1:12]
  Failure=(FailuresimallFP)
  costQ<-costQ_rmt
  costImport <- 7
  costRSF<-costIMP_jmt[nTrucks+1,,1:12]
  costFail <- 20
  TotTRUCKCost=(rQimport*costImport)
  TotRSFCost=(rQRSF*costRSF)
  TotalDEFICITCost=(Failure*costFail)
  print(TotTRUCKCost)
  print(TotRSFCost)
  print(TotalDEFICITCost)
}else{ 
  rQimport<-(colSums(rQIMPfinal[1:nTrucks,,1:12]))
  Failure=(FailuresimallFP)
  costQ<-costQ_rmt
  costImport <- 7
  costFail <- 20
  TotTRUCKCost=(rQimport*costImport)
  TotalDEFICITCost=(Failure*costFail)
 print(TotTRUCKCost)
  print(TotalDEFICITCost)
} 

for (fixedpolicytime in 1:25){
  yearindexFP<-(1:12)+(fixedpolicytime-1)*12
if (with_rioSF == TRUE){
  
  if (fixedpolicytime == 1){
    TotTRUCKCostFP<- array(data=0, c(1,25))
    TotRSFCostFP<- array(data=0, c(1,25))
    TotalDEFICITCostFP<- array(data=0, c(1,25))
  }
  
  TotTRUCKCostFP[,fixedpolicytime]=sum(TotTRUCKCost)
  TotRSFCostFP[,fixedpolicytime]=sum(TotRSFCost)
  TotalDEFICITCostFP[,fixedpolicytime]=sum(TotalDEFICITCost[yearindexFP])


TOTALCOST<-TotTRUCKCostFP+TotRSFCostFP+TotalDEFICITCostFP
COSTFINALFP<-rbind(TotTRUCKCostFP,TotRSFCostFP,TotalDEFICITCostFP)}
else{

  if (fixedpolicytime == 1){
    TotTRUCKCostFP<- array(data=0, c(1,25))
    TotalDEFICITCostFP<- array(data=0, c(1,25))
  }
    TotTRUCKCostFP[,fixedpolicytime]=sum(TotTRUCKCost)
    TotalDEFICITCostFP[,fixedpolicytime]=sum(TotalDEFICITCost[yearindexFP])
    
    
    TOTALCOST<-TotTRUCKCostFP+TotalDEFICITCostFP
    COSTFINALFP<-rbind(TotTRUCKCostFP,TotalDEFICITCostFP)
}}

if (with_rioSF==TRUE){
  write.csv(COSTFINALFP,file = paste0("../Results/Fixed Policy cost with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}else{
  write.csv(COSTFINALFP,file = paste0("../Results/Fixed Policy cost without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}


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
  write.csv(TOTALCOSTFPF,file = paste0("../Results/FINAL COSTS Fixed with Rio Sao Francisco and ", costF_rts[1,1,1],"Failure Cost.csv"))
  write.csv(FPFINAL,file = paste0("../Results/FINAL AMT Fixed with Rio Sao Francisco and ", costF_rts[1,1,1],"Failure Cost.csv"))
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
  write.csv(TOTALCOSTFPFin,file = paste0("../Results/FINAL COSTS Fixed without Rio Sao Francisco and ", costF_rts[1,1,1],"Failure Cost.csv"))
  write.csv(FPFINAL,file = paste0("../Results/FINAL AMT Fixed without Rio Sao Francisco and ", costF_rts[1,1,1],"Failure Cost.csv"))
}

