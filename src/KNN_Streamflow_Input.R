if (year==1){
  Juca<-as.matrix(0, nrow=nT,ncol=nS)
  Macha<-as.matrix(0, nrow=nT,ncol=nS)
  Eng<-as.matrix(0, nrow=nT,ncol=nS)
  Poco<-as.matrix(0, nrow=nT,ncol=nS)
  Prata<-as.matrix(0, nrow=nT,ncol=nS)
}

inflowyearindex = (1:24)+(year-1)*24
Juca<-InflowJu[inflowyearindex,]
Macha<-InflowMa[inflowyearindex,]
Eng<-InflowEn[inflowyearindex,]
Poco<-InflowPo[inflowyearindex,]
Prata<-InflowPr[inflowyearindex,]


I_rts<-array(data=0 , c(nR,nT,nS))
I_rts[1,,]<-Juca*1e-6
I_rts[2,,]<-Macha*1e-6
I_rts[3,,]<-Eng*1e-6
I_rts[4,,]<-Poco*1e-6
I_rts[5,,]<-Prata*1e-6


if (year==1){
  KnnJu_one<-matrix(0, nrow=300, ncol=10)
  KnnMa_one<-matrix(0, nrow=300, ncol=10)
  KnnEn_one<-matrix(0, nrow=300, ncol=10)
  KnnPo_one<-matrix(0, nrow=300, ncol=10)
  KnnPr_one<-matrix(0, nrow=300, ncol=10)
}
indexfortimeknn<-1:12 +(year-1)*12
KnnJu_one[indexfortimeknn,]<-I_rts[1,1:12,]
KnnMa_one[indexfortimeknn,]<-I_rts[2,1:12,]
KnnEn_one[indexfortimeknn,]<-I_rts[3,1:12,]
KnnPo_one[indexfortimeknn,]<-I_rts[4,1:12,]
KnnPr_one[indexfortimeknn,]<-I_rts[5,1:12,]

