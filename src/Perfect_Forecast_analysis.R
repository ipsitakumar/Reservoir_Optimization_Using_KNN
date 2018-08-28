
if(with_rioSF == TRUE){
  pdf(file=paste("../Results/Perfect Forecast Results for all years (for ", nT/12, " years) ", " with Rio Sao Francisco and ", costF_rts[1,1,],"Failure Cost.pdf"),onefile=TRUE,width = 12,height=6)
}else{
  pdf(file=paste("../Results/Perfect Forecast Results for all years (for ", nT/12, " years) ", " without Rio Sao Francisco and ", costF_rts[1,1,],"Failure Cost.pdf"),onefile=TRUE,width = 12,height=6)
}

### This is the plot for the total inflows coming into the reservoirs over time
for(r in 1:5){
  plot(I_rts[r,,1],type="l",ylim=c(min(I_rts[r,,]),max(I_rts[r,,])),ylab="storage [mio m3]",xlab = "months")
  abline(h=SCmax_rt[r,1],col="red")
  title(paste("inflow reservoir", resname[r]))
}

## This is the plot of the amount of failure 
plot(colSums(rFPF),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(rFPF)),max(colSums(rFPF))))
title("Withdrawal failure")


mLOC=rQPF
mALLres<-colSums(mLOC)
if(with_rioSF==TRUE){
  mIMP=rQIMPPF[1,]
  Jucazinho<-mLOC[1,]
  otherreservoirs<-mLOC[2:nR,]
  otherreservoirs<-apply(otherreservoirs,MARGIN=c(2),sum)
  riosf<-mIMP
  withdrawaloptim<-rbind(Jucazinho,otherreservoirs,riosf)
}else{
  mIMP=rQIMPPF
  Jucazinho<-mLOC[1,]
  otherreservoirs<-mLOC[2:nR,]
  otherreservoirs<-apply(otherreservoirs,MARGIN=c(2),sum)
  withdrawaloptim<-rbind(Jucazinho,otherreservoirs)
}
if(with_rioSF==TRUE){
  AB<-3
}else{
  AB=2
}


## This is the plot for the withdrawal from the reservoirs and import sources, and deifict
plot((mALLres-colSums(rFPF[1:5,])),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
if (with_rioSF == TRUE){
  lines(withdrawaloptim[AB,], col = 2,lwd=3,lty=3)}
if (with_rioSF == TRUE){
  lines(colSums(rFPF), col=3, lwd=3,lty=4)
} else {
  lines(colSums(rFPF), col=3, lwd=3,lty=4)
}
if(with_rioSF == TRUE){
  legend("topleft", c("","All Reservoirs","Rio Sao Francisco","Failure"), col = c(0,1,2,3),pt.cex=0.5,lty=1:4,lwd=3, cex=0.75,bty="n")
}else{
  legend("topleft",c("","All Reservoirs", "Failure"), col = c(0,1,3),pt.cex=0.5,lwd=3, lty=1:3,cex=0.75,bty="n")
}
if(with_rioSF == TRUE){
  title(paste0("Withdrawals from reservoirs and imports and failure for ", nT/12, " years ", " \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
} else {
  title(paste0("Withdrawals from reservoirs and imports and failure for ", nT/12, " years ", " \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
}

dev.off()



### We are getting the results for the costs from here
if (with_rioSF == TRUE){
  withdrawalFP<-mLOC
  costwithdrawal<-c(1.05,0.04,0.65,0.98,1.38)
  RioSFFP<-mIMP
  costrioSF<-1.67
  FailureFP<-colSums(rFPF)
  Failurecost<-20
  withdrawalFPCost<-withdrawalFP*costwithdrawal
  RioSFFPCost<-RioSFFP*costrioSF
  FailureFPCost<-FailureFP*Failurecost
  TOTALCOSTFPF<-rbind(withdrawalFPCost, RioSFFPCost, FailureFPCost)
  FPFINAL<-rbind(withdrawalFP, RioSFFP,FailureFP)
  write.csv(TOTALCOSTFPF,file = paste0("../Results/FINAL COSTS Perfect with Rio Sao Francisco and ", costF_rts[1,1,],"Failure Cost.csv"))
  write.csv(FPFINAL,file = paste0("../Results/FINAL AMT Perfect with Rio Sao Francisco and ", costF_rts[1,1,],"Failure Cost.csv"))
}else{
  withdrawalFP<-mLOC
  costwithdrawal<-c(1.05,0.04,0.65,0.98,1.38)
  costrioSF<-1.67
  FailureFP<-colSums(rFPF)
  Failurecost<-20
  withdrawalFPCost<-withdrawalFP*costwithdrawal
  FailureFPCost<-FailureFP*Failurecost
  TOTALCOSTFPFin<-rbind(withdrawalFPCost, FailureFPCost)
  FPFINAL<-rbind(withdrawalFP,FailureFP)
  write.csv(TOTALCOSTFPFin,file = paste0("../Results/FINAL COSTS Perfect without Rio Sao Francisco and ", costF_rts[1,1,],"Failure Cost.csv"))
  write.csv(FPFINAL,file = paste0("../Results/FINAL AMT Perfect without Rio Sao Francisco and ", costF_rts[1,1,],"Failure Cost.csv"))
}

