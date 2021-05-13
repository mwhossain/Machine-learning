##Objective: Build a neural network to predict probabilities of each class for each observation. 

install.packages("NMOF")
library(dummies)
library(xgboost)
library(NMOF)

#from sklearn.datasets import make_classification
#from sklearn.ensemble import GradientBoostingClassifier
#from sklearn.model_selection import train_test_sp

dataPath<-"C:/Users/manya/OneDrive/Documents/machine_learning_final_project/"
#data <- read.csv(paste0(dataPath,"test_sample.csv"))

Data <- read.csv(paste0(dataPath,"train_sample.csv"))
test <- read.csv(paste0(dataPath,"test_sample.csv"))
testdata <-data.matrix(test[,2:94])

set.seed(13)
testInd = sample(nrow(Data), nrow(Data)/3)
xTrain = Data[-testInd,]
xTest = Data[testInd,]
yTrain = as.factor(Data$target[-testInd])
yTest = Data$target[testInd]
head(xgbTrain)

xgbTrain = data.matrix(xTrain[,-ncol(xTrain)])
xgbTest = data.matrix(xTest[,-ncol(xTest)])
yTrain = as.integer(yTrain)-1
yTest = as.integer(yTest)-1
table(yTrain)
xgbyTrain = data.matrix(yTrain)

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  #normalize rows
  ll = sum(act*log(sweep(pred, 1, rowSums(pred), FUN="/")))
  ll = -ll/nrow(act)
  return(ll);
}


numClasses = max(yTrain) + 1
param <- list("objective" = "multi:softmax",
              "eval_metric" = "mlogloss",
              "num_class" = numClasses,
              eta=0.3, gamma=1, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=.8)

cv.nround <-150
cv.nfold <- 5
set.seed(1)
(bst.cv = xgb.cv(param=param, data = xgbTrain, label = yTrain, 
                 nfold = cv.nfold, nrounds = cv.nround,verbose=F))


#bst.cv
#plot(bst.cv$test_mlogloss_mean)

#params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=1, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=.8)

bst = xgboost(param=param, data = xgbTrain, label = yTrain,nrounds=cv.nround,verbose=F,save_period=NULL)
 #bst<-xgboost(data.matrix(xgbTrain), label = yTrain, param=list("objective" = "multi:softmax"),
 #nrounds=5, num_class=9, verbose=F,save_period=NULL)

 xgbPred <- matrix(predict(bst, xgbTest), ncol = numClasses, byrow = TRUE)
 head(xgbPred) 

 gb_target_IndMat<-dummy.data.frame(data=as.data.frame(yTest), 
                                    sep="_", verbose=F, 
                                    dummy.class="ALL")
 XGB = MultiLogLoss(gb_target_IndMat,xgbPred)
 print(XGB)
  
 #Tuning
 folds <- 5
 eval_metric = list("logloss")  # evaluation metric for validation
 
 # Parameters grid to search
 eta = c(.3,.15,0.05)
 max_depth = c(4,5,6)
 nrounds <- c(seq(from=30,to=90,by=10),26)
 
 # Table to track performance from each worker node
 res <- data.frame(Value=numeric(),Eta=numeric(),Max_Depth=numeric(),Nrounds=numeric())
 
 #Make fitting function
 xgbCV <- function (Inputs) {
   myEta<-Inputs$eta
   myMax_depth<-Inputs$max_depth
   myNRounds<-Inputs$n_Rounds
   set.seed(0)
   fit <- xgb.cv(
     params =list(eta=myEta,max_depth=myMax_depth),
     data = yTrain,
     metrics=eval_metric,
     objective = "multi:softmax",
     num_class = numClasses,
     label = xgbyTrain, 
     nfold = folds, 
     nrounds = myNRounds,
     verbose=F
   )
   mybestNR = which.min(fit$evaluation_log$test_mlogloss_mea)
   val <- fit$evaluation_log$test_mlogloss_mea[mybestNR]
   res <<- rbind(res,c(val,myEta,myMax_depth,mybestNR))
   
   return(val)
 }
 
 sol <- gridSearch(
   fun = xgbCV,
   levels = list(eta=eta,max_depth=max_depth,n_Rounds=nrounds),
   method = 'loop',
   keepNames = TRUE,
   asList = TRUE
    )
 
 Test<-model.predict_proba(bst,newdata=testdata)
 Test
 
 Test2<- round(Test, digits = 0)
 a<-test[,1]
 head(pred)
 
 pred = matrix(cbind(a,Test), nrow=19594, ncol=2) 
 head(xgbTrain)

 saveCSV((Forecast = pred), "MLFinal.csv")
 