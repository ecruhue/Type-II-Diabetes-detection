
#
z_night<-read.csv("ZR_AllData_Night.csv",header=F)
#subset 21 subjects with type2 diabetes
T2D_night<-subset(z_night,z_night[,166]==1)

#Subset 20 subjects with non-diabetic
ND_night<-subset(z_night,z_night[,166]==0)
#
ND_glc<-as.data.frame(ND_night[,1:55])
v1<-colnames(ND_glc)
ND_ins<-as.data.frame(ND_night[,56:110])
colnames(ND_ins)<-v1
ND_glg<-as.data.frame(ND_night[,111:165])
colnames(ND_glg)<-v1
ND_tmp<-rbind(ND_glc,ND_ins,ND_glg)
#
T2D_glc<-as.data.frame(T2D_night[,1:55])
v2<-colnames(T2D_glc)
T2D_ins<-as.data.frame(T2D_night[,56:110])
colnames(T2D_ins)<-v2
T2D_glg<-as.data.frame(T2D_night[,111:165])
colnames(T2D_glg)<-v2
T2D_tmp<-rbind(T2D_glc,T2D_ins,T2D_glg)


ttl<-rbind(ND_glc,ND_ins,T2D_glc,T2D_ins)
n <- 10
s <- sample(1:40, n)
idx <- c(s,s+42)
sample2 <- ttl[idx,]
observedLabels <- c(rep(0,n), rep(1,n))
library(dtw)
distMatrix <- dist(sample2, method="DTW")
# hierarchical clustering
hc <- hclust(distMatrix, method="average")
plot(hc, labels=observedLabels, main="")


library(wavelets)
wtData <- NULL
for (i in 1:nrow(ttl)) {
  a <- t(ttl[i,])
  wt <- dwt(a, filter="haar", boundary="periodic")
  wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
}
wtData <- as.data.frame(wtData)

classId <- c(rep("0",40), rep("1",42))
wtSc <- data.frame(cbind(classId, wtData))
library(gird)
library(mvtnorm)
library(modeltools)
library(stats4)
library(strucchange)
library(zoo)
library(party)

ct <- ctree(classId ~ ., data=wtSc, controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
pClassId <- predict(ct)

# check predicted classes against original class labels

table(classId, pClassId)
#       pClassId
#classId  0  1
#      0 37  3
#      1 15 27
(sum(classId==pClassId)) / nrow(wtSc)

# 0.7804878
# as for only glucose: the result is very accurate
#       pClassId
#classId  0  1
#      0 20  0
#      1  0 21