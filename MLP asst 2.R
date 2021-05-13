#Using Regularized Regression (Lasso)


install.packages("glmnet")
library(glmnet)

dataPath<-"C:/Users/manya/OneDrive/Documents/R/machine_learning_02_data" 
data <- read.csv(paste(dataPath,"test_sample.csv",sep="/"),header=T)

Y <- data[,1]
X <-as.matrix(data[,2:492])

set.seed(1)
#run lm
allPredictors.lm<-lm(Y~.,data=data)
summary(allPredictors.lm)


lm.pvalues<-summary(allPredictors.lm)$coefficients[,4]
lm.pvalues

which(lm.pvalues > 0.05)
lmindices<-c(3,8,13,21,22,23,25,47,50,67,79,105,108,140,146,151,162,166,167,168,183,189,204,206,212,216,221,225,228,239,245,251,266,267,270,274,276,293,297,298,305,307,324,334,336,338,340,341,344,347,353,366,370,371,381,435,443,447,464,474)
eliminatedByLm<-lmindices-1

#eliminatedByLm<-c(which(lm.pvalues > 0.05))
eliminatedByLm
length(eliminatedByLm)
#order(summary(allPredictors.lm)$coefficients, decreasing = TRUE) 
#not sure what to do with this yet

#run LASSO
lasso_cv<-cv.glmnet(x=X,y=Y, alpha=1)
names(lasso_cv)
plot(lasso_cv)

bestlam = lasso_cv$lambda.min
bestlam

#out=glmnet(x=X,y=Y,alpha=1,nlambda=100,lambda=bestlam,standardize=F)
#out
#names(out)

#get Coefficients
lasso.coef=predict(lasso_cv,type="coefficients",s=bestlam)
lasso.coef

#extract (eliminated) coefficients=0 
x<-as.vector(lasso.coef)
indices<-c(which(x == 0))
eliminatedByLasso<-indices-1
#length(eliminatedByLasso)

res = matrix(c("lasso","lm","",""),ncol=2)
colnames(res) <- c("model","removed_regressors")
res[,"removed_regressors"][1] = paste0(eliminatedByLasso,collapse = " ")
res[,"removed_regressors"][2] = paste0(eliminatedByLm,collapse = " ")
write.csv(res,"W2answer.csv",quote=FALSE,row.names = F)
