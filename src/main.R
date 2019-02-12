#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## SCENARIOS 
nTotalTime <- 25*12  ## The number of months the model is running for 
startmonth <- 540-nTotalTime
nScenarios <- 10  ## The number of ensemble runs we are doing for the KNN optimization 
nTrucks <- 5


with_rioSF <- FALSE
source("Perfect_Forecast_run.R")
source("KNN_run.R")
source("Fixed_Policy_run.R")

## read functions for analysis
source("analysis.R")

## summarizing the results
par(mfrow=c(3,1))
par(mar=c(4,4,4,1))
plotresults(rQ_PF, rQIMP_PF, rF_PF)
plotresults(rQ_FP, rQIMP_FP, rF_FP)
plotresults(rQ_KNN, rQIMP_KNN, rF_KNN)

## COMPUTE THE METRICS
Coef_KNN <- compute_coeff(rF_KNN)
Coef_PF <- compute_coeff(rF_PF)
Coef_FP <- compute_coeff(rF_FP)

## produce table of results
volnoSF <- round(cbind(c(rowSums(rQ_PF), NA, sum(rQ_PF), sum(rF_PF)),
            c(rowSums(rQ_FP), NA, sum(rQ_FP), sum(rF_FP)),
            c(rowSums(rQ_KNN), NA, sum(rQ_KNN), sum(rF_KNN)))/nTotalTime*12,
      digits = 1)

metricnoSF <- t(signif(rbind(Coef_PF, Coef_FP, Coef_KNN),
       digits = 2))

volnoSF <- rbind(volnoSF, metricnoSF)
colnames(volnoSF) <- c("PF", "FP", "KNN")
rownames(volnoSF) <- c(resname, "Rio SF", "Total all supply", "Failure", "Reliability", "Resiliency", "Vulnerability")

print(volnoSF)

with_rioSF <- TRUE
source("Perfect_Forecast_run.R")
source("KNN_run.R")
source("Fixed_Policy_run.R")

par(mfrow=c(3,1))
plotresults(rQ_PF, rQIMP_PF, rF_PF)
plotresults(rQ_FP, rQIMP_FP, rF_FP)
plotresults(rQ_KNN, rQIMP_KNN, rF_KNN)

## COMPUTE THE METRICS

Coef_KNN <- compute_coeff(rF_KNN)
Coef_PF <- compute_coeff(rF_PF)
Coef_FP <- compute_coeff(rF_FP)


## produce table of results
volwSF <- round(cbind(c(rowSums(rQ_PF), sum(rQIMP_PF[nTrucks+1,]), sum(rQ_PF), sum(rF_PF)),
                       c(rowSums(rQ_FP), sum(rQIMP_FP[nTrucks+1,]), sum(rQ_FP), sum(rF_FP)),
                       c(rowSums(rQ_KNN), sum(rQIMP_KNN[nTrucks+1,]), sum(rQ_KNN), sum(rF_KNN)))/nTotalTime*12,
                 digits = 1)

metricwSF <- t(signif(rbind(Coef_PF, Coef_FP, Coef_KNN),
                       digits = 2))

volwSF <- rbind(volwSF, metricwSF)
colnames(volwSF) <- c("PF", "FP", "KNN")
rownames(volwSF) <- c(resname, "Rio SF", "Total all supply", "Failure", "Reliability", "Resiliency", "Vulnerability")

print(volnoSF)
print(volwSF)




## from which reservoirs
par(mfrow=c(5,1))
for(rr in 1:nR){
  plot(rQ_PF[rr,], col="blue", type="l")
  lines(rQ_FP[rr,], col="red")
  lines(rQ_KNN[rr,], col="orange")
}


for(rr in 1:nR){
  barplot(rbind(turntoyr(rQ_PF[rr,]), turntoyr(rQ_FP[rr,]), turntoyr(rQ_KNN[rr,])), beside = TRUE, col = c("blue", "red", "orange"))
}

par(mfrow=c(5,3))
for(rr in 1:nR){
  br = seq(0, max(c(turntoyr(rQ_PF[rr,]), 
                           turntoyr(rQ_FP[rr,]), 
                           turntoyr(rQ_KNN[rr,]))),
                  max(c(turntoyr(rQ_PF[rr,]),
                        turntoyr(rQ_FP[rr,]),
                        turntoyr(rQ_KNN[rr,])))/10)
  hist(turntoyr(rQ_PF[rr,]), col = "blue", breaks=br)
  hist(turntoyr(rQ_FP[rr,]), col = "red", breaks=br)
  hist(turntoyr(rQ_KNN[rr,]), col = "orange", breaks=br)
}


turntoyr <- function(valeur){
  valyr<-matrix(valeur, nrow=12, ncol=length(valeur)/12)
  return(colSums(valyr))
}

