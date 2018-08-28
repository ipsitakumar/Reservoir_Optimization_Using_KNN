

setwd("C:/Users/Ipsita Kumar/Desktop/IDB Research Project/Reservoir Optimization Model for Pernambuco -- Data and code/results") ##


KNN_7_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 7.1Failure Cost.csv")
KNN_10_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 10Failure Cost.csv")
KNN_20_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 20Failure Cost.csv")
KNN_30_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 30Failure Cost.csv")
KNN_50_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 50Failure Cost.csv")
KNN_75_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 75Failure Cost.csv")
KNN_100_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 100Failure Cost.csv")
KNN_150_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 150Failure Cost.csv")
KNN_200_With <- read.csv("FINAL AMT KNN with Rio Sao Francisco and 200Failure Cost.csv")


KNN_7_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 7.1Failure Cost.csv")
KNN_10_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 10Failure Cost.csv")
KNN_20_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 20Failure Cost.csv")
KNN_30_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 30Failure Cost.csv")
KNN_50_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 50Failure Cost.csv")
KNN_75_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 75Failure Cost.csv")
KNN_100_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 100Failure Cost.csv")
KNN_150_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 150Failure Cost.csv")
KNN_200_without <- read.csv("FINAL AMT KNN without Rio Sao Francisco and 200Failure Cost.csv")


Fixed_7_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 7.1Failure Cost.csv")
Fixed_10_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 10Failure Cost.csv")
Fixed_20_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 20Failure Cost.csv")
Fixed_30_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 30Failure Cost.csv")
Fixed_50_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 50Failure Cost.csv")
Fixed_75_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 75Failure Cost.csv")
Fixed_100_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 100Failure Cost.csv")
Fixed_150_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 150Failure Cost.csv")
Fixed_200_With <- read.csv("FINAL AMT Fixed with Rio Sao Francisco and 200Failure Cost.csv")

Fixed_7_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 7.1Failure Cost.csv")
Fixed_10_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 10Failure Cost.csv")
Fixed_20_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 20Failure Cost.csv")
Fixed_30_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 30Failure Cost.csv")
Fixed_50_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 50Failure Cost.csv")
Fixed_75_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 75Failure Cost.csv")
Fixed_100_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 100Failure Cost.csv")
Fixed_150_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 150Failure Cost.csv")
Fixed_200_without <- read.csv("FINAL AMT Fixed without Rio Sao Francisco and 200Failure Cost.csv")

Perfect_7_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 7.1Failure Cost.csv")
Perfect_10_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 10Failure Cost.csv")
Perfect_20_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 20Failure Cost.csv")
Perfect_30_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 30Failure Cost.csv")
Perfect_50_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 50Failure Cost.csv")
Perfect_75_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 75Failure Cost.csv")
Perfect_100_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 100Failure Cost.csv")
Perfect_150_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 150Failure Cost.csv")
Perfect_200_With <- read.csv("FINAL AMT Perfect with Rio Sao Francisco and 200Failure Cost.csv")

Perfect_7_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 7.1Failure Cost.csv")
Perfect_10_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 10Failure Cost.csv")
Perfect_20_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 20Failure Cost.csv")
Perfect_30_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 30Failure Cost.csv")
Perfect_50_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 50Failure Cost.csv")
Perfect_75_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 75Failure Cost.csv")
Perfect_100_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 100Failure Cost.csv")
Perfect_150_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 150Failure Cost.csv")
Perfect_200_without <- read.csv("FINAL AMT Perfect without Rio Sao Francisco and 200Failure Cost.csv")




Fixed_All_without<-rbind(Fixed_7_without[6,-1], Fixed_10_without[6,-1], Fixed_20_without[6,-1], Fixed_30_without[6,-1], 
                         Fixed_50_without[6,-1], Fixed_75_without[6,-1], Fixed_100_without[6,-1], Fixed_150_without[6,-1], 
                         Fixed_200_without[6,-1])

Fixed_All_with<-rbind(Fixed_7_With[7,-1], Fixed_10_With[7,-1], Fixed_20_With[7,-1], Fixed_30_With[7,-1], 
                      Fixed_50_With[7,-1], Fixed_75_With[7,-1], Fixed_100_With[7,-1], Fixed_150_With[7,-1], 
                      Fixed_200_With[7,-1])

KNN_All_without<-rbind(KNN_7_without[6,-1], KNN_10_without[6,-1], KNN_20_without[6,-1], KNN_30_without[6,-1], 
                       KNN_50_without[6,-1], KNN_75_without[6,-1], KNN_100_without[6,-1], KNN_150_without[6,-1], 
                       KNN_200_without[6,-1])

KNN_All_with<-rbind(KNN_7_With[7,-1], KNN_10_With[7,-1], KNN_20_With[7,-1], KNN_30_With[7,-1], 
                    KNN_50_With[7,-1], KNN_75_With[7,-1], KNN_100_With[7,-1], KNN_150_With[7,-1], 
                    KNN_200_With[7,-1])

Perfect_All_without<-rbind(Perfect_7_without[6,-1], Perfect_10_without[6,-1], Perfect_20_without[6,-1], Perfect_30_without[6,-1], 
                           Perfect_50_without[6,-1], Perfect_75_without[6,-1], Perfect_100_without[6,-1], Perfect_150_without[6,-1], 
                           Perfect_200_without[6,-1])

Perfect_All_with<-rbind(Perfect_7_With[7,-1], Perfect_10_With[7,-1], Perfect_20_With[7,-1], Perfect_30_With[7,-1], 
                        Perfect_50_With[7,-1], Perfect_75_With[7,-1], Perfect_100_With[7,-1], Perfect_150_With[7,-1], 
                        Perfect_200_With[7,-1])


Results<-array(data=0, dim=c(9,7))
colnames(Results)<-c("Cost", "PERFECT WITH", "PERFECT WITHOUT", "FIXED WITH", "FIXED WITHOUT", "KNN WITH", "KNN WITHOUT")
Results[,1]<-c(7.1, 10, 20, 30, 50, 75, 100, 200, 300)
Results[,2]<- rowSums(Perfect_All_with)/25
Results[,3]<- rowSums(Perfect_All_without)/25
Results[,4]<- rowSums(Fixed_All_with)/25
Results[,5]<- rowSums(Fixed_All_without)/25
Results[,6]<- rowSums(KNN_All_with)/25
Results[,7]<- rowSums(KNN_All_without)/25

write.csv(Results, "RESULTS_AMOUNT.csv")
