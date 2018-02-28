# This file generates the functions that are used to keep track of the position of the various elements in the optimization matrix.

posRMT <- function(i,j,t){
  pos <- 0
  if(i<=nR){if(j<=nM){if(t<=nT){
    pos <- i+nR*(j-1)+nR*nM*(t-1)
  }}}
  return(pos)
}
posJMT <- function(i,j,t){
  pos <- 0
  if(i<=nIMP){if(j<=nM){if(t<=nT){
    pos <- i+nIMP*(j-1)+nIMP*nM*(t-1)
  }}}
  return(pos)
}
posMTS <- function(j,t,s){
  pos <- 0
  if(j<=nM){if(t<=nT){if(s<=nS){
    pos <- j+nM*(t-1)+nM*nT*(s-1)
  }}}
  return(pos)
}
posRTS <- function(i,t,s){
  pos <- 0
  if(i<=nR){if(t<=nT){if(s<=nS){
    pos <- i+nR*(t-1)+nR*nT*(s-1)
  }}}
  return(pos)
}
posRT <- function(i,t){
  pos <- 0
  if(i<=nR){if(t<=nT){
    pos <- i+nR*(t-1)
  }}
  return(pos)
}
posJT <- function(i,t){
  pos <- 0
  if(i<=nIMP){if(t<=nT){
    pos <- i+nIMP*(t-1)
  }}
  return(pos)
}
posRS <- function(i,s){
  pos <- 0
  if(i<=nR){if(s<=nS){
    pos <- i+nR*(s-1)
  }}
  return(pos)
}
posMT <- function(j,t){
  pos <- 0
  if(j<=nM){if(t<=nT){
    pos <- j+nM*(t-1)
  }}
  return(pos)
}
posRM <- function(i,j){
  pos <- 0
  if(i<=nR){if(j<=nM){
    pos <- i+nR*(j-1)
  }}
  return(pos)
}

