#################################################################################################################
######## Matrix generator in R - Laureline
######## laureline.josset@gmail.com
#################################################################################################################
dim_Q <- nM*nR*nT
dim_IMP <- nM*nIMP*nT
dim_F <- nR*nT*nS
dim_releases <- nR*nT
dim_reservoirs <- nR*nT*nS
dim_tot <- dim_Q + dim_F + dim_releases + dim_reservoirs + dim_IMP

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

#__________________________________________________________________________________________________
### OBJECTIVE
Obj <-matrix(data=0,ncol=1,nrow=dim_tot)
Obj[index_Q,] <-array(costQ_rmt,dim=c(dim_Q,1))
Obj[index_F,] <-array(costF_rts,dim=c(dim_F,1))
Obj[index_IMP,] <-array(costIMP_jmt,dim=c(dim_IMP,1))

#__________________________________________________________________________________________________
## GENERATE CONSTRAINTS MATRIX
#-- DEMANDS
load("A_d.Rdata")
load("rhs_d.Rdata")
load("s_d.Rdata")

#-- STREAMS
if(with_Pconstraints){
  load("A_pmin.Rdata")
  load("rhs_pmin.Rdata")
  load("s_pmin.Rdata")
  
  load("A_pmax.Rdata")
  load("rhs_pmax.Rdata")
  load("s_pmax.Rdata")
}

#-- IMPORTS
if(nIMP>0){
  load("A_impmax.Rdata")
  load("rhs_impmax.Rdata")
  load("s_impmax.Rdata")}
#-- RESERVOIR
# MIN
if(with_Smin_constraints){
  rhs_smin <- array(SCmin_rt,dim=c(dim_reservoirs,1))
  load("A_smin.Rdata")
  load("s_smin.Rdata")
}

# MAX
if(with_Smax_constraints){
  rhs_smax <- array(SCmax_rt,dim=c(dim_reservoirs,1))
  load("A_smax.Rdata")
  load("s_smax.Rdata")
} 

# RESERVOIR STATE EQUATIONS
rhs_s <- array(I_rts,dim=c(dim_reservoirs,1))
for(r in 1:nR){for(s in 1:nS){
  rhs_s[posRTS(r,1,s)] <- S_r0s[r,s]*(1-e_rts[r,1,s]) + I_rts[r,1,s]}}
load("A_s.Rdata")
load("s_s.Rdata")


#__________________________________________________________________________________________________
## COMPLETE OPTIMIZATION PROBLEM
rhs   <- rbind(rhs_d,rhs_s)
sense <- rbind(  s_d,  s_s)
A     <- rbind(  A_d,  A_s)
if(nIMP>0){
  rhs   <- rbind(rhs,rhs_impmax)
  sense <- rbind(  s,  s_impmax)
  A     <- rbind(  A,  A_impmax)  
}
if(with_Pconstraints){
  rhs   <- rbind(rhs,rhs_pmin,rhs_pmax)
  sense <- rbind(  s,  s_pmin,  s_pmax)
  A     <- rbind(  A,  A_pmin,  A_pmax)
}
if(with_Smax_constraints){
  rhs   <- rbind(rhs,rhs_smax)
  sense <- rbind(  s,  s_smax)
  A     <- rbind(  A,  A_smax)
}
if(with_Smin_constraints){
  rhs   <- rbind(rhs,rhs_smin)
  sense <- rbind(  s,  s_smin)
  A     <- rbind(  A,  A_smin)
}


  