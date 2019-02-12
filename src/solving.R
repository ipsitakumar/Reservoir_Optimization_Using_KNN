# SOLVING THE GENERATEDR MATRIX

require(lpSolve)
lpsol <- lp(direction = "min", objective.in=Obj, const.mat=A, const.dir=sense, const.rhs=rhs)
xval <- lpsol$solution
objval <- lpsol$objval

## reshaping the results
rQ <- array(data=xval[index_Q],dim=c(nR,nM,nT))
rQIMP <- array(data=xval[index_IMP],dim=c(nIMP,nM,nT))
rF <- array(data=xval[index_F],dim=c(nR,nT,nS))
rS <- array(data=xval[index_reservoirs],dim=c(nR,nT,nS))
rP <- array(data=xval[index_releases],dim=c(nR,nT))
r2F <- array(data=xval[index_2F],dim=c(nR,nT,nS))