#################################################################################################################
######## Matrix generator in R - Laureline
######## laureline.josset@gmail.com
#################################################################################################################
dim_Q <- nM*nR*nT
dim_IMP <- nM*nIMP*nT
dim_F <- nR*nT*nS
dim_releases <- nR*nT
dim_reservoirs <- nR*nT*nS
dim_2F <- nR*nT*nS
dim_tot <- dim_Q + dim_F + dim_releases + dim_reservoirs + dim_IMP + dim_2F

start_Q <- 0
index_Q <- 1:dim_Q
start_F <- dim_Q
index_F <- dim_Q + (1:dim_F)
start_releases <- dim_Q+dim_F
index_releases <- start_releases + (1:dim_releases)
start_reservoirs <- dim_Q+dim_F+dim_releases
index_reservoirs <- start_reservoirs + (1:dim_reservoirs)
start_IMP <- dim_Q+dim_F+dim_releases+dim_reservoirs
index_IMP <- start_IMP + (1:dim_IMP)
start_2F <- start_IMP + dim_IMP
index_2F <- start_2F + (1:dim_2F)

#__________________________________________________________________________________________________
### OBJECTIVE
Obj <- matrix(data=0,ncol=1,nrow=dim_tot)
Obj[index_Q,] <- array(costQ_rmt,dim=c(dim_Q,1))
Obj[index_F,] <- array(costF_rts,dim=c(dim_F,1))
Obj[index_2F,] <- array(0*costF_rts,dim=c(dim_2F,1))
Obj[index_IMP,] <- array(costIMP_jmt,dim=c(dim_IMP,1))

#__________________________________________________________________________________________________
## GENERATE CONSTRAINTS MATRIX
#-- DEMANDS
rhs_d <- array(D_mt,dim=c(nM*nT,1))
s_d <- array(">=",dim=c(nM*nT,1))
A_d <- array(0,dim=c(nM*nT,dim_tot))
for(t in 1:nT){for(m in 1:nM){
  for(r in 1:nR){
    A_d[posMT(m,t),start_Q + posRMT(r,m,t)] <- Mrm[r,m]  ## extraction from reservoir m to mun r
  }
  for(j in 1:nIMP){
    A_d[posMT(m,t),start_IMP + posJMT(j,m,t)] <- Mjm[j,m]
  }
}}


#-- STREAMS
if(with_Pconstraints){
  # MIN
  rhs_pmin <- array(PCmin_rt,dim=c(dim_releases,1))
  s_pmin <- array(">",dim=c(dim_releases,1))
  A_pmin <- array(0,dim=c(dim_releases,dim_tot))
  for(l in 1:(nR*nT)){
    A_pmin[l, start_releases + l] <- 1
  }
  # MAX
  rhs_pmax <- array(PCmax_rt,dim=c(dim_releases,1))
  s_pmax <- array("<",dim=c(dim_releases,1))
  A_pmax <- array(0,dim=c(nR*nT,dim_tot))
  for(l in 1:(nR*nT)){
    A_pmax[l, start_releases + l] <- 1
  }
}

#-- IMPORTS
if(nIMP > 0){
  rhs_impmax <- array(IMPmax_jt,dim=c(nIMP*nT,1))
  s_impmax <- array("<",dim=c(nIMP*nT,1))
  A_impmax <- array(0,dim=c(nIMP*nT,dim_tot))
  for(j in 1:nIMP){for(t in 1:nT){for(m in 1:nM){
    A_impmax[posJT(j,t), start_IMP + posJMT(j,m,t)] <- 1
  }}}}

#-- RESERVOIR
# MIN
if(with_Smin_constraints){
  rhs_smin <- array(SCmin_rt,dim=c(dim_reservoirs,1))
  s_smin <- array(">",dim=c(dim_reservoirs,1))
  A_smin <- array(0,dim=c(dim_reservoirs,dim_tot))
  for(l in 1:(nR*nT*nS)){
    A_smin[l,start_reservoirs+l] <- 1
  }
}
# MAX
if(with_Smax_constraints){
  rhs_smax <- array(SCmax_rt,dim=c(dim_reservoirs,1))
  s_smax <- array("<",dim=c(dim_reservoirs,1))
  A_smax <- array(0,dim=c(dim_reservoirs,dim_tot))
  for(l in 1:(nR*nT*nS)){
    A_smax[l,l+start_reservoirs] <- 1
  }
} 

# RESERVOIR STATE EQUATIONS
rhs_s <- array(I_rts,dim=c(dim_reservoirs,1))
for(r in 1:nR){for(s in 1:nS){
  rhs_s[posRTS(r,1,s)] <- S_r0s[r,s]*(1-e_rts[r,1,s]) + I_rts[r,1,s]}}

s_s  <- array("=",dim=c(dim_reservoirs,1))

A_sQ <- array(0,dim=c(dim_reservoirs,dim_Q))
A_sF <- array(0,dim=c(dim_reservoirs,dim_F))
A_s2F <- array(0,dim=c(dim_reservoirs,dim_2F))
A_sP <- array(0,dim=c(dim_reservoirs,dim_releases))
A_sS <- array(0,dim=c(dim_reservoirs,dim_reservoirs))

for(r in 1:nR){
  for(s in 1:nS){
    t <- 1
    for(m in 1:nM){
      A_sQ[posRTS(r,t,s),posRMT(r,m,t)] <- Mrm[r,m]
    }
    A_sF[posRTS(r,t,s),posRTS(r,t,s)]<- -1
    A_s2F[posRTS(r,t,s),posRTS(r,t,s)]<- 1
    A_sS[posRTS(r,t,s),posRTS(r,t,s)]<- 1
    for(r_ in 1:nR){
      A_sP[posRTS(r,t,s),posRT(r_,t)] <- (r==r_)-Mrr[r,r_]}
    
    for(t in 2:nT){
      for(m in 1:nM){
        A_sQ[posRTS(r,t,s),posRMT(r,m,t)] <-Mrm[r,m]
      }
      A_sF[posRTS(r,t,s),posRTS(r,t,s)] <- -1
      A_s2F[posRTS(r,t,s),posRTS(r,t,s)] <- 1
      for(r_ in 1:nR){
        A_sP[posRTS(r,t,s),posRT(r_,t)] <- (r==r_)-Mrr[r,r_]
        for(t_ in 1:nT){
          A_sS[posRTS(r,t,s),posRTS(r_,t_,s)] <- (posRT(r,t)==posRT(r_,t_)) - (1-e_rts[r,t_,s])*((r==r_)*(t_+1==t))
        }}}}}

A_s <- array(0,dim=c(dim_reservoirs,dim_tot))
A_s[,index_Q] <- A_sQ
A_s[,index_F] <- A_sF
A_s[,index_2F] <- A_s2F
A_s[,index_releases] <- A_sP
A_s[,index_reservoirs] <- A_sS

#__________________________________________________________________________________________________
## COMPLETE OPTIMIZATION PROBLEM
rhs   <- rbind(rhs_d,rhs_s)
sense <- rbind(  s_d,  s_s)
A     <- rbind(  A_d,  A_s)
if(nIMP>0){
  rhs   <- rbind(rhs,rhs_impmax)
  sense <- rbind(sense,  s_impmax)
  A     <- rbind(  A,  A_impmax)  
}
if(with_Pconstraints){
  rhs   <- rbind(rhs,rhs_pmin,rhs_pmax)
  sense <- rbind(sense,  s_pmin,  s_pmax)
  A     <- rbind(  A,  A_pmin,  A_pmax)
}
if(with_Smax_constraints){
  rhs   <- rbind(rhs,rhs_smax)
  sense <- rbind(sense,  s_smax)
  A     <- rbind(  A,  A_smax)
}
if(with_Smin_constraints){
  rhs   <- rbind(rhs,rhs_smin)
  sense <- rbind(sense,  s_smin)
  A     <- rbind(  A,  A_smin)
}
