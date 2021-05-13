#As we know, determination coefficients of sequence of nested models mj based on predictors X1,…,Xj, 2≤j≤491 monotonically increase when j grows.

#Follow the two steps below.

#Step 1.
#Fit linear regression models mj
#Y=β0+β1X1+β2X2+…+βjXj+ϵ
#with increasing number j, 2≤j≤491 of regressors and calculate determination coefficient for each of these models.
#Find smallest number of regressors making determination coefficient of linear model greater than 0.9 (90%). Denote it as N.orig.

#Step 2.
#Apply method of PCA Regression (function prcomp()) using factors as meta-features to select smallest number of them making determination coefficient greater than 90%.
#Denote such number N.PCA, i.e. N.PCA is the smallest number of PCA factors that sufficient for given level of determination coefficient.
#Define model dimensionality reduction as difference N.orig - N.PCA.

#Enter model dimensionality reduction and determination coefficient of the model with N.PCA selected most important meta-features in the corresponding fields of Quiz tab.


install.packages("relaimpo")
library(relaimpo)
library(rlist)

dataPath<-"C:/Users/manya/Downloads/machine_learning_01_data"
data <- read.table(paste(dataPath,"test_sample.csv",sep="/"),header=T)

Y <- data[,1]
X <-data[,2:492]


m2<-lm(Y~., data=data.frame(Y,X[,1:2]))
completeModelDataFrame<-data.frame(Y,X[,1:491])
m490<-lm(Y~.,data=completeModelDataFrame)
#coefficients(summary(m490))
plot(coefficients(summary(m490))[-1,4],main="Coefficients' P-Values for 490 Predictors",
     xlab="Coefficient",ylab="P-Value")

summary(m2)
confint(m2)[2,]
confint(m490)[2,]

rSquared<-sapply(2:491,function(z) summary(lm(Y~.,data=data.frame(Y=Y[],X[,1:z])))$r.squared)
plot(rSquared,type="l",
     main="Improvement of Fit with Number of Predictors",xlab="Number of Predictors",ylab="Determination Coefficient")

summary(rSquared)

rSquared[330:350]
rSquared[336]
N.orig = 336

pca.X <-princomp(X)
m3<-lm(Y~., data=data.frame(Y,X[,1:2])) 

factorLoadings<-pca.X$loadings
factorScores<-pca.X$scores

#head(factorScores)

m_pca<-lm(Y~., data=data.frame(Y,factorScores))
summary(m_pca)                        

xPCA<-sapply(2:491,function(z) summary(lm(Y~.,data=data.frame(Y=Y[],factorScores[,1:z])))$r.squared)
plot(xPCA,type="l",
     main="Improvement of Fit with Number of Predictors",xlab="Number of Predictors",ylab="Determination Coefficient PCA")

metrics.PCA <- calc.relimp(m_pca, type = "first")
metrics.PCA

summary(metrics.PCA)

sum.PCA.first=sum(metrics.PCA@first)
(first.PCA.rank<-metrics.PCA@first.rank)

orderedFactors<-factorScores[,order(first.PCA.rank)]
orderedLoadings<-factorLoadings[,order(first.PCA.rank)]
colnames(orderedFactors)

orderedPCA.R2<-sapply(2:491,function(z) summary(lm(Y~.,data=data.frame(Y=Y[],orderedFactors[,1:z])))$r.squared)

matplot(2:10,cbind(originalR2.10,improvedR2.10,orderedPCA.R2),type="l",lty=1,lwd=2,col=c("black","red","blue"),
        main="Improvement of Fit with Number of Predictors",xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend=c("Original","Improved","PCA"),lty=1,lwd=2,col=c("black","red","blue"))

plot(orderedPCA.R2,type="l",
     main="Improvement of Fit with Number of Predictors, PCA",xlab="Number of Predictors",ylab="Determination Coefficient PCA")

orderedPCA.R2[134]

#xPCA[235:237]
N.PCA <-134
N.orig - N.PCA

orderedPCA.R2[134]
#xPCA[237]
