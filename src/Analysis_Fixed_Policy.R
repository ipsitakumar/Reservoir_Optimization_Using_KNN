
if(with_rioSF == TRUE){
  pdf(file=paste("RESULT FIXED POLICY/results from fixed policy", "with RSF_",percent_init_reservoir,"initstorage for 25 years.pdf"),onefile=TRUE, width = 12,height=6)
}else{
  pdf(file=paste("RESULT FIXED POLICY/results from fixed policy", "without RSF_",percent_init_reservoir,"initstorage for 25 years.pdf"),onefile=TRUE,width = 12,height=6)
}

#par(mfrow=c(2,2))
#par(mar=c(4,4,4,1))
for(r in 1:5){
  plot(Storagesimfinal[r,],type="l",ylim=c(min(Storagesimfinal[r,]),max(Storagesimfinal[r,])),ylab="storage [mio m3]",xlab = "months")
  abline(h=SCmax_rt[r,1],col="red")
  #lines(Storagesimfinal[r,],type="l", legend("topright",c("Simlated Storage", col="red"),col="red"))
  title(paste(resname[r], "reservoir"))
}

#sumfailuresim<-sum(Failuresim)
#hist(colSums(colSums(rFfinal)),xlab = "Failure [mio m3]",main = " ", xlim = c(0 , 120))
#abline(v=mean(colSums(colSums(rFfinal))),col="red",lwd=2)
#abline(v=quantile(colSums(colSums(rFfinal)),probs = c(0.1,0.5,0.9)),col=c(3:nR),lwd=2,lty=2:4)
#abline(v=sumfailuresim,col=6,lwd=2,lty=5)
#legend("topright",c("Mean","10th Percentile","Median","90th Percentile","Simulated Failure"), col=c(2:(nR+1)),lty=1:5,lwd=2,cex=1,bty="n")
#if(with_rioSF == TRUE){
#  title(paste0("Distribution failure for 25 years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"),cex.main=1)
#} else {
#  title(paste0("Distribution failure for 25 years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"), cex.main=1)
#}


for(r in 1:5){
  plot(I_rts[r,,1],type="l",ylim=c(min(I_rts[r,,]),max(I_rts[r,,])),ylab="storage [mio m3]",xlab = "months")
  for(s in 2:nS){lines(I_rts[r,,s])}
  abline(h=SCmax_rt[r,1],col="red")
  title(paste("inflow reservoir", resname[r]))
}

plot(colSums(Failuresimfinal),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(Failuresimfinal[,])),max(colSums(Failuresimfinal[,]))))
title("Withdrawal failure")
plot(colSums(r2Ffinal[,,1]),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(r2Ffinal[,,])),max(colSums(r2Ffinal[,,]))))
for(s in 2:nS){lines(colSums(r2Ffinal[,,s]))}
title("Release failure")
#plot(colSums((rFfinal+r2Ffinal)[,,1]),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums((rFfinal+r2Ffinal)[,,])),max(colSums((rFfinal+r2Ffinal)[,,]))))
#for(s in 2:nS){lines(colSums((rFfinal+r2Ffinal)[,,s]))}
#title("Total failure")



#mLOC=apply(rQfinal,MARGIN=c(1,3),sum)
#mIMP=apply(rQIMPfinal,MARGIN=c(1,3),sum)
#if(with_rioSF==TRUE){
#  imp<-mIMP[1:nTrucks,]
#  imp<-apply(imp,MARGIN=c(2),sum)
#  Jucazinho<-mLOC[1,]
#  otherreservoirs<-mLOC[2:nR,]
#  otherreservoirs<-apply(otherreservoirs,MARGIN=c(2),sum)
#  riosf<-mIMP[nIMP,]
#  withdrawaloptim<-rbind(Jucazinho,otherreservoirs,imp,riosf)
#}else{
#  imp<-mIMP[1:nTrucks,]
#  imp<-apply(imp,MARGIN=c(2),sum)
#  Jucazinho<-mLOC[1,]
#  otherreservoirs<-mLOC[2:nR,]
#  otherreservoirs<-apply(otherreservoirs,MARGIN=c(2),sum)
#  withdrawaloptim<-rbind(Jucazinho,otherreservoirs,imp)
#}

#if(with_rioSF==TRUE){
#  AB<-4
#}else{
#  AB=3
#}

#rFmedian<-apply(rFfinal,MARGIN=c(1,2),median)
#rFmedianJuca<-rFmedian[1,]
#rFmedianothers<-colSums(rFmedian[2:nR,])
#rFall<-colSums(rFfinal)
#rFallmedian<-apply(rFall,1,median)
#withdrawalsimJuca<-withdrawalsimfinal[1,]
#withdrawalsimothers<-colSums(withdrawalsimfinal[2:nR,])
#Failuresimulated<-colSums(Failuresimfinal)

#plot((withdrawalsimJuca),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
#lines(withdrawalsimothers,col=2,lwd=3,lty=2)
#if (with_rioSF == TRUE){
#  lines(Importsim[1,], col = 3,lwd=3,lty=3)}
#lines(colSums(Failuresimfinal), col=4, lwd=4,lty=6)
#if(with_rioSF == TRUE){
#  legend("topleft", c("Jucazinho","Other Reservoirs","Rio Sao Francisco","Failure"),  col = c(1,2,3,4),lty=(1:6),pt.cex=0.5,lwd=2, cex=0.75,bty="n")
#}else{
#  legend("topleft",c("Jucazinho","Other Reservoirs Sim","Failure"), col = c(1,2,4),pt.cex=0.5,lwd=2, lty=(1:6), cex=0.75,bty="n")
#}
#if(with_rioSF == TRUE){
#  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
#} else {
#  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
#}

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


plot((withdrawalsimallres),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
if (with_rioSF == TRUE){
  lines(Importsim[1,], col = 2,lwd=2,lty=3)}
lines(colSums(Failuresimfinal), col=3, lwd=4,lty=6)
if(with_rioSF == TRUE){
  legend("topleft", c("", "ALl Reservoirs","Rio Sao Francisco","Failure"),  col = c(0,1,2,3),lty=(1:6),pt.cex=0.5,lwd=2, cex=0.75,bty="n")
}else{
  legend("topleft",c("", "All Reservoirs","Failure"), col = c(0,1,3),pt.cex=0.5,lwd=2, lty=(1:6), cex=0.75,bty="n")
}
if(with_rioSF == TRUE){
  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
} else {
  title(paste0("Withdrawals from reservoirs and imports and failure for 25 years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
}


if(with_rioSF==TRUE){
  #rQLOCAL<-rQ[,,1:12]
  rQimport<-(colSums(rQIMPfinal[1:nTrucks,,1:12]))
  rQRSF<-rQIMPfinal[nTrucks+1,,1:12]
  Failure=(FailuresimallFP)
  costQ<-costQ_rmt
  costImport <- 7
  costRSF<-costIMP_jmt[nTrucks+1,,1:12]
  costFail <- 20
  
  #Failuresim_one<-Failuresim[,1:12]
  # withdrawalsim_one<-withdrawalsim[,1:12]
  
  
  #TotLOCALCost=sum(rQLOCAL*costQ[1,,1:12])
  TotTRUCKCost=(rQimport*costImport)
  TotRSFCost=(rQRSF*costRSF)
  TotalDEFICITCost=(Failure*costFail)
  #LOCALsimCost=sum(withdrawalsim_one*costQ[,1:12])
  #FailuresimCost=sum(Failuresim_one*costFail)
  #print(TotLOCALCost)
  print(TotTRUCKCost)
  print(TotRSFCost)
  print(TotalDEFICITCost)
#  COST<-rbind(TotTRUCKCost,TotRSFCost,TotalDEFICITCost,TOTALCOSTWITHOUTLOCAL) #LOCALsimCost,not includes
  #  write.csv(COST,file = paste0("TT/Each year cost with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage in year ", yearpredictingfor,".csv"))
}else{ 
  #rQLOCAL<-rQ[,,1:12]
  rQimport<-(colSums(rQIMPfinal[1:nTrucks,,1:12]))
  Failure=(FailuresimallFP)
  costQ<-costQ_rmt
  costImport <- 7
  costFail <- 20
  # withdrawalsim_one<-withdrawalsim[,1:12]
  
  
  #TotLOCALCost=sum(rQLOCAL*costQ[1,,1:12])
  TotTRUCKCost=(rQimport*costImport)
  TotalDEFICITCost=(Failure*costFail)
  #LOCALsimCost=sum(withdrawalsim_one*costQ[,1:12])
  #FailuresimCost=sum(Failuresim_one*costFail)
  #TOTALCOSTWITHOUTLOCAL=TotTRUCKCost+TotalDEFICITCost
  
  #print(TotLOCALCost)
  print(TotTRUCKCost)
  print(TotalDEFICITCost)
  #print(TOTALCOSTWITHOUTLOCAL)
  #COST<-rbind(TotTRUCKCost,TotalDEFICITCost,TOTALCOSTWITHOUTLOCAL)
} ### this 4 next lines have to be changed too.


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
  write.csv(COSTFINALFP,file = paste0("RESULT FIXED POLICY/Fixed Policy cost with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}else{
  write.csv(COSTFINALFP,file = paste0("RESULT FIXED POLICY/Fixed Policy cost without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}