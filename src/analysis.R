# Analyzing the results
plotresults <- function(Withd, Imp, Fail){
  if(with_rioSF == TRUE){
    #  pdf(file=paste0("../Results/", scenariotype," Results for all years (for ", nTotalTime/12, " years) with Rio Sao Francisco and ", costF_rts[1,1,1]," Failure Cost.pdf"),onefile=TRUE,width = 12,height=6)
  }else{
    #  pdf(file=paste0("../Results/", scenariotype," Results for all years (for ", nTotalTime/12, " years) without Rio Sao Francisco and ", costF_rts[1,1,1]," Failure Cost.pdf"),onefile=TRUE,width = 12,height=6)
  } 
  
  ## This is the plot for the withdrawal from the reservoirs and import sources, and deficit
  plot(colSums(Withd),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1)
  if (with_rioSF == TRUE){
    lines(Imp[nTrucks+1,], col = 2,lwd=2,lty=3)}
  lines(colSums(Fail), col=3, lwd=4,lty=6)
  if(with_rioSF == TRUE){
    legend("topleft", c("", "All Reservoirs","Rio Sao Francisco","Failure"),  col = c(0,1,2,3),lty=(1:6),pt.cex=0.5,lwd=2, cex=0.75,bty="n")
  }else{
    legend("topleft",c("", "All Reservoirs","Failure"), col = c(0,1,3),pt.cex=0.5,lwd=2, lty=(1:6), cex=0.75,bty="n")
  }
  
  if(with_rioSF == TRUE){
    title(paste0("Withdrawals and failure for ", nTotalTime/12, " years \nwith Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage" ),cex.main=1)
  } else {
    title(paste0("Withdrawals and failure for ", nTotalTime/12, " years \nwithout Rio Sao Francisco and ", percent_init_reservoir[1]*100,"% initial storage"  ), cex.main=1)
  }
  #dev.off()
}



## COMPUTE THE METRICS
compute_coeff <- function(rF){
  Z <- 1*(colSums(rF) <= 0)
  W <- Z[1:(length(Z)-1)]==0 & Z[2:(length(Z))]==1
  C_R <- sum(Z)/length(Z) # Reliability: proportion of months with the system malfunctioning 
  C_RS <- sum(W)/(length(Z)-sum(Z)) # Resiliency: Measure of the capacity of the system to come back to functioning after failing
  maxCv <- 0
  C_V <- 0
  for(tt in 1:length(Z)){
    if(Z[tt]==0){
      maxCv <- maxCv + colSums(rF)[tt]
    }else{
      if(C_V < maxCv){C_V <- maxCv}
      maxCv <- 0
    }
  }
  if(C_V < maxCv){C_V <- maxCv} # Vulnerability: max volume of failure experienced during a single failing event
  return(cbind(C_R, C_RS, C_V))
}




