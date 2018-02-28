##PERFECT FORECAST

#rQPF=array(data=xval[index_Q],dim=c(nR,nM,nT))
#rQIMPPF=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
#rFPF=array(data=xval[index_F],dim=c(nR,nT,nS))
#rSPF=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
#rPPF=array(data=xval[index_releases],dim=c(nR,nT))
#r2FPF=array(data=xval[index_2F],dim=c(nR,nT,nS))
## making sure demand is met at each municipality, at each time step
demandmet = TRUE
for(m in 1:nM){
  for (t in 1:nT){
    if (sum(rQPF[,m,t]) - D_mt[m,t] > 1e-3){
      demandmet = FALSE
    }}}
if(demandmet){print("demands are met")}else{print("ERROR !")}

## are inflows larger than reservoir capacities ?
for(r in 1:nR){
  print(paste("========= reservoir",r, ", max capacities", SCmax_rt[r,1]))
  for(t in 1:nT){
    for(s in 1:nS){
      if(I_rts[r,t,s] > SCmax_rt[r,1]){
        print(I_rts[r,t,s])
      }}
  }
}



## failure is correct?
for(s in 1:nS){
  for(t in 1:nT){
    #    print(sum(rFPF[,t,s]))
    #    print(sum(D_mt[,t]*1e-6))
    #    print(sum(rPPF[,t]))
    if(sum(rFPF[,t,s])>sum(D_mt[,t])){
      print("failure exceed demand")
    }
    for(r in 1:nR){
      if(rFPF[r,t,s]>rPPF[r,t] + sum(rQPF[r,,t])){
        print(paste(r, t, s))
        print("failure exceeds withdrawals plus release")
        print(rFPF[r,t,s] -rPPF[r,t] - sum(rQPF[r,,t]))
      }}
  }}


if(with_rioSF == TRUE){
  pdf(file=paste("../Results/Perfect Forecast Results for all years (for ", nT/12, " years) ", " with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage.pdf"),onefile=TRUE,width = 12,height=6)
}else{
  pdf(file=paste("../Results/Perfect Forecast Results for all years (for ", nT/12, " years) ", " without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage.pdf"),onefile=TRUE,width = 12,height=6)
}



#par(mfrow=c(2,2))
#par(mar=c(4,4,4,1))
#hist(colSums(colSums(rFPF)),xlab = "Failure [mio m3]",main = "Distribution failure")
#hist(colSums(colSums(r2FPF)),xlab = "Failure [mio m3]",main = "Distribution failure")
#hist(colSums(colSums(rFPF)),xlab = "Failure [mio m3]",main = "Distribution failure", )
#abline(v=mean(colSums(colSums(rFPF))),col="red",lwd=3)
#abline(v=quantile(colSums(colSums(rFPF)),probs = c(0.1,0.5,0.9)),col="blue",lwd=2)

for(r in 1:5){
  plot(I_rts[r,,1],type="l",ylim=c(min(I_rts[r,,]),max(I_rts[r,,])),ylab="storage [mio m3]",xlab = "months")
 # for(s in 2:nS){lines(I_rts[r,,s])}
  abline(h=SCmax_rt[r,1],col="red")
  title(paste("inflow reservoir", resname[r]))
}

plot(colSums(rFPF[,,1]),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(rFPF[,,])),max(colSums(rFPF[,,]))))
#for(s in 2:nS){lines(colSums(rFPF[,,s]))}
title("Withdrawal failure")
plot(colSums(r2FPF[,,1]),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums(r2FPF[,,])),max(colSums(r2FPF[,,]))))
#for(s in 2:nS){lines(colSums(r2FPF[,,s]))}
title("Release failure")
plot(colSums((rFPF+r2FPF)[,,1]),type="l",xlab="months",ylab="in mio m3",ylim=c(min(colSums((rFPF+r2FPF)[,,])),max(colSums((rFPF+r2FPF)[,,]))))
#for(s in 2:nS){lines(colSums((rFPF+r2FPF)[,,s]))}
title("Total failure")



mLOC=apply(rQPF,MARGIN=c(1,3),sum)
mIMP=apply(rQIMPPF,MARGIN=c(1,3),sum)
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

#plot((withdrawaloptim[1,]-rFPF[1,,]),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
#lines((withdrawaloptim[2,]-colSums(rFPF[2:5,,])),col=2,lwd=3,lty=2)
#if (with_rioSF == TRUE){
#  lines(withdrawaloptim[AB,], col = 3,lwd=3,lty=3)}
#if (with_rioSF == TRUE){
#  lines(colSums(rFPF), col=4, lwd=3,lty=4)
#} else {
#  lines(colSums(rFPF), col=4, lwd=3,lty=4)
#}
#if(with_rioSF == TRUE){
#  legend("topleft", c("Jucazinho","Other Reservoirs","Rio Sao Francisco","Failure"), col = c(1,2,3,4),pt.cex=0.5,lty=1:4,lwd=3, cex=0.75,bty="n")
#}else{
#  legend("topleft",c("Jucazinho","Other Reservoirs", "Failure"), col = c(1,2,4),pt.cex=0.5,lwd=3, lty=1:3,cex=0.75,bty="n")
#}
#if(with_rioSF == TRUE){
#  title(paste0("Withdrawals from reservoirs and imports and failure for ", nT/12, " years ", " \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
#} else {
#  title(paste0("Withdrawals from reservoirs and imports and failure for ", nT/12, " years ", " \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
#}


plot((mALLres-colSums(rFPF[1:5,,])),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
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

plot((mALLres-colSums(rFPF[1:5,,])),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
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


for (time45 in 1:25){
  yearindex45<-(1:12)+(time45-1)*12

  if(with_rioSF==TRUE){
  rQLOCALcost25<-rQPF[,,yearindex45]
  rQimportcost25<-rQIMPPF[1:nTrucks,,yearindex45]
  rQRSFcost25<-rQIMPPF[nTrucks+1,,yearindex45]
  Failurecost25=colSums(rFPF)
  Failurecost25=Failurecost25[yearindex45,]
  costQ25<-costQ_rmt
  costImport25 <- costIMP_jmt[1:nTrucks,,]
  costRSF25<-costIMP_jmt[nTrucks+1,,]
  costFail25 <- 7.7
  
  if (time45==1){
  TotLOCALCost25=array(data=0,dim=c(1,25))
  TotTRUCKCost25=array(data=0,dim=c(1,25))
  TotRSFCost25=array(data=0,dim=c(1,25))
  TotalDEFICITCost25=array(data=0,dim=c(1,25))
  }
  TotLOCALCost25[,time45]=sum(rQLOCALcost25*costQ25[,,1:12])
  TotTRUCKCost25[,time45]=sum(rQimportcost25*costImport25[,,1:12])
  TotRSFCost25[,time45]=sum(rQRSFcost25*costRSF25[,1:12])
  TotalDEFICITCost25[,time45]=sum(Failurecost25*costFail25)
  TOTALCOST25=TotLOCALCost25+TotTRUCKCost25+TotRSFCost25+TotalDEFICITCost25
  
  print(TotLOCALCost25)
  print(TotTRUCKCost25)
  print(TotRSFCost25)
  print(TotalDEFICITCost25)
  print(TOTALCOST25)
  if (time45 ==1){
    COST45<-array(data=0,dim=c(5,25))}
  
  COST45<-rbind(TotLOCALCost25,TotTRUCKCost25,TotRSFCost25,TotalDEFICITCost25,TOTALCOST25) #LOCALsimCost,not includes
  }else{ 
  rQLOCALcost25<-rQPF[,,yearindex45]
  rQimportcost25<-rQIMPPF[1:nTrucks,,yearindex45]
  Failurecost25=colSums(rFPF)
  Failurecost25=Failurecost25[yearindex45,]
  costQ25<-costQ_rmt
  costImport25 <- costIMP_jmt[1:nTrucks,,]
  costFail25 <- 7.7
  
  if (time45==1){
  TotLOCALCost25=array(data=0,dim=c(1,25))
  TotTRUCKCost25=array(data=0,dim=c(1,25))
  TotalDEFICITCost25=array(data=0,dim=c(1,25))
  }
  TotLOCALCost25[,time45]=sum(rQLOCALcost25*costQ25[,,1:12])
  TotTRUCKCost25[,time45]=sum(rQimportcost25*costImport25[,,1:12])
  TotalDEFICITCost25[,time45]=sum(Failurecost25*costFail25)
  TOTALCOST25=TotLOCALCost25+TotTRUCKCost25+TotalDEFICITCost25
  
  print(TotLOCALCost25)
  print(TotTRUCKCost25)
  print(TotalDEFICITCost25)
  print(TOTALCOST25)
  if (time45 ==1){
    COST45<-array(data=0,dim=c(5,25))}
  
  COST45<-rbind(TotLOCALCost25,TotTRUCKCost25,TotalDEFICITCost25,TOTALCOST25) #LOCALsimCost,not includes
} ### this 4 next lines have to be changed too.


if (with_rioSF==TRUE){
  write.csv(COST45,file = paste0("../Results/PERFECT FORECAST Each year cost with Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}else{write.csv(COST45,file = paste0("../Results/PERFECT FORECAST Each year cost without Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage for 25 years.csv"))
}
}