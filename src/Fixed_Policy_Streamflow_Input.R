require(data.table)
for (timethree in 1:nS){
  indexthree=1:36+(timethree-1)*12
resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata")
Jucas<-read.csv("../data/Juca.csv",header=T)
Jucas<-as.matrix(Jucas,nrow=24,ncol=45)
Jucas<-Jucas[1:12,]
Jucas<-melt(Jucas)
Jucas<-Jucas[,3]
Jucas<-Jucas[indexthree]
Jucas<-as.matrix(Jucas)
if (timethree == 1){
Juca<-array(data=0,dim = c(36,nS))
}
Juca[,timethree] <- Jucas[,1]

Machas<-read.csv("../data/Macha.csv",header=T)
Machas<-as.matrix(Machas,nrow=24,ncol=45)
Machas<-Machas[1:12,]
Machas<-melt(Machas)
Machas<-Machas[,3]
Machas<-Machas[indexthree]
Machas<-as.matrix(Machas)
if (timethree == 1){
Macha<-array(data=0,dim = c(36,nS))
}
Macha[,timethree] <- Machas[,1]

Engs<-read.csv("../data/Eng.csv",header=T)
Engs<-as.matrix(Engs,nrow=24,ncol=45)
Engs<-Engs[1:12,]
Engs<-melt(Engs)
Engs<-Engs[,3]
Engs<-Engs[indexthree]
Engs<-as.matrix(Engs)
if (timethree == 1){
Eng<-array(data=0,dim = c(36,nS))
}
Eng[,timethree] <- Engs[,1]

Pocos<-read.csv("../data/Poco.csv",header=T)
Pocos<-as.matrix(Pocos,nrow=24,ncol=45)
Pocos<-Pocos[1:12,]
Pocos<-melt(Pocos)
Pocos<-Pocos[,3]
Pocos<-Pocos[indexthree]
Pocos<-as.matrix(Pocos)
if (timethree == 1){
Poco<-array(data=0,dim = c(36,nS))
}
Poco[,timethree] <- Pocos[,1]

Pratas<-read.csv("../data/Prata.csv",header=T)
Pratas<-as.matrix(Pratas,nrow=24,ncol=45)
Pratas<-Pratas[1:12,]
Pratas<-melt(Pratas)
Pratas<-Pratas[,3]
Pratas<-Pratas[indexthree]
Pratas<-as.matrix(Pratas)
if (timethree == 1){
Prata<-array(data=0,dim = c(36,nS))
}
Prata[,timethree] <- Pratas[,1]

if (timethree==1){
I_rts<-array(data=0 , c(nR,nT,nS))
}


I_rts[1,,]<-Juca[,(44-nS):43] * 1e-6
I_rts[2,,]<-Macha[,(44-nS):43] * 1e-6
I_rts[3,,]<-Eng[,(44-nS):43] * 1e-6
I_rts[4,,]<-Poco[,(44-nS):43] * 1e-6
I_rts[5,,]<-Prata[,(44-nS):43] * 1e-6

}