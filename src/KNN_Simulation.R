# Here we are running the K nearest neighbor to make streamflow forecasts
# The streamflow forecasts are made for the 5 reservoirs in this case

for (timeforknn in 1:25){
  
  if (timeforknn == 1){
    yearpredictingforknn<-1982+timeforknn
    yearselectedknn <- yearpredictingforknn-2
  }else{ # it is twice the same thing in both section
    yearpredictingforknn <- 1982+timeforknn
    yearselectedknn <- yearpredictingforknn-2
  }
  Jucaknn<-read.csv("../data/Juca1.csv",header = T) # all the file you read should be read only once. move them outside of the loop
  Jucaknn<-as.matrix(Jucaknn,nrow = 24,ncol = 45)
  Machaknn<-read.csv("../data/Macha1.csv",header = T)
  Machaknn<-as.matrix(Machaknn,nrow = 24,ncol = 45)
  Engknn<-read.csv("../data/Eng1.csv",header = T)
  Engknn<-as.matrix(Engknn,nrow = 24,ncol = 45)
  Pocoknn<-read.csv("../data/Poco1.csv",header = T)
  Pocoknn<-as.matrix(Pocoknn,nrow = 24,ncol = 45)
  Prataknn <- read.csv("../data/Prata1.csv",header = T)
  Prataknn<-as.matrix(Prataknn,nrow = 24,ncol = 45)
  
  
  Jucaknn<-t(Jucaknn) # same thing, outside of the loop
  Machaknn<-t(Machaknn)
  Engknn<-t(Engknn)
  Pocoknn<-t(Pocoknn)
  Prataknn<-t(Prataknn)
  
  reservoirs<-cbind(Jucaknn,Machaknn,Engknn,Pocoknn,Prataknn) # same thing, outside of the loop
  resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata") # same thing, outside of the loop | actually this is not used in this code ! deleting it
  whichyear <- paste0("X",yearselectedknn,".",(yearselectedknn+1))
  whichrow <- which(whichyear == rownames(reservoirs))
  rows <- 1:dim(reservoirs)[1]
  xx <-rows[c(-whichrow,-44,-45)] # what is this again?
  xtest <- whichrow
  y <- 3:dim(reservoirs)[1]
  remy <- whichrow # xx and remy have the same definition
  whichrow # this should be deleted I believe
  
  reservoirs_x<-reservoirs[xx,]
  reservoirs_xtest<-t(reservoirs[xtest,])
  reservoirs_y<-reservoirs[y,]
  reservoirs_y<-reservoirs_y[c(-remy),]
  
  #=====================
  #k=number of neighbors,ns= number of simulations
  knnsim <- function(y,x,xtest,k,ns,w = NULL){ # why are you declaring the function inside the loop for?
    x <- as.matrix(x)
    xtest <- as.matrix(xtest)
    y <- as.matrix(y)
    if(is.null(w))w <- rep(1,ncol(x))
    if(nrow(y)!= nrow(x))
      print('error: lengths of y and x differ')
    if(ncol(x)!= ncol(xtest))
      print('error: col lengths of x and xtest differ')
    
    yknn <- matrix(NA,nrow = ns,ncol(y))
    
    yk <- rep(NA,nrow(xtest))
    
    yr <- seq(1,nrow(y))
    
    for(i in 1:nrow(xtest)){
      a <- matrix(NA,nrow(x),ncol(x))
      for(n in 1:ncol(x))
        a[,n] <- 100*w[n]*(x[,n]- xtest[i,n])^2
      
      c <- rowSums(a,na.rm = T)
      
      yk[rank(c,ties.method = 'first')] <- yr
      
      j <- rank(c)		#equal prob for equidistant neighbours
      sj <- sum(j[1:k]^(-1))
      pj <- (j[1:k]^(-1))/sj
      
      ynp <- sample(yk[1:k],ns,replace = T,prob = pj)
      for(p in 1:ncol(y)) 
        yknn[,p] <- y[ynp,p]
      
    }
    return(yknn)
  }
  
  reservoirs_result <- knnsim(reservoirs_y,reservoirs_x,reservoirs_xtest,reservoirs_k,reservoirs_ns)
  
  if (timeforknn==1){ # cleaner to declare this at the start
    InflowJu <- array(data = 0, c(nT*25,nS))
    InflowMa <- array(data = 0, c(nT*25,nS))
    InflowEn <- array(data = 0, c(nT*25,nS))
    InflowPo <- array(data = 0, c(nT*25,nS))
    InflowPr <- array(data = 0, c(nT*25,nS))
  }
  
  yearforknnindex <- (1:24)+(timeforknn-1)*24
  InflowJu[yearforknnindex,] <- t(reservoirs_result[,1:24])
  InflowJu <- as.matrix(InflowJu,nrow = 24,ncol = dim(reservoirs)[,1])
  InflowMa[yearforknnindex,] <- t(reservoirs_result[,25:48])
  InflowMa <- as.matrix(InflowMa,nrow = 24,ncol = dim(reservoirs)[,1])
  InflowEn[yearforknnindex,] <- t(reservoirs_result[,49:72])
  InflowEn <- as.matrix(InflowEn,nrow = 24,ncol = dim(reservoirs)[,1])
  InflowPo[yearforknnindex,] <- t(reservoirs_result[,73:96])
  InflowPo <- as.matrix(InflowPo,nrow = 24,ncol = dim(reservoirs)[,1])
  InflowPr[yearforknnindex,] <- t(reservoirs_result[,97:120])
  InflowPr <- as.matrix(InflowPr,nrow = 24,ncol = dim(reservoirs)[,1])
  
  
  save(InflowJu, file = "../data/InflowJu_new.Rdata") # why are these inside the for loop ?
  save(InflowMa, file = "../data/InflowMa_new.Rdata")
  save(InflowEn, file = "../data/InflowEn_new.Rdata")
  save(InflowPo, file = "../data/InflowPo_new.Rdata")
  save(InflowPr, file = "../data/InflowPr_new.Rdata")
  save(reservoirs, file = "../data/reservoirs_new.Rdata")
  
}
