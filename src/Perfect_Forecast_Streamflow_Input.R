require(data.table)
resname <- c("Jucazinho","Machado","Eng Gercino Pontes","Poco Fundo","Prata")
Juca<-read.csv("../data/Juca.csv",header=T)
Juca<-as.matrix(Juca,nrow=24,ncol=45)
Juca<-Juca[1:12,]
Juca<-melt(Juca)
Juca<-Juca[,3]

Macha<-read.csv("../data/Macha.csv",header=T)
Macha<-as.matrix(Macha,nrow=24,ncol=45)
Macha<-Macha[1:12,]
Macha<-melt(Macha)
Macha<-Macha[,3]

Eng<-read.csv("../data/Eng.csv",header=T)
Eng<-as.matrix(Eng,nrow=24,ncol=45)
Eng<-Eng[1:12,]
Eng<-melt(Eng)
Eng<-Eng[,3]

Poco<-read.csv("../data/Poco.csv",header=T)
Poco<-as.matrix(Poco,nrow=24,ncol=45)
Poco<-Poco[1:12,]
Poco<-melt(Poco)
Poco<-Poco[,3]

Prata <- read.csv("../data/Prata.csv",header=T)
Prata<-as.matrix(Prata,nrow=24,ncol=45)
Prata<-Prata[1:12,]
Prata<-melt(Prata)
Prata<-Prata[,3]

I_rts<-array(data=0 , c(nR,nT,nS))


I_rts[1,,]<-Juca[(541-nT):540]*1e-6
I_rts[2,,]<-Macha[(541-nT):540]*1e-6
I_rts[3,,]<-Eng[(541-nT):540]*1e-6
I_rts[4,,]<-Poco[(541-nT):540]*1e-6
I_rts[5,,]<-Prata[(541-nT):540]*1e-6

