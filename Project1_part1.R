#Import libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(randomForest)
library(xgboost)
library(gbm)
library(caret)
library(robustHD)
library(PerformanceAnalytics)
#read data
set.seed(6199)
myData = read.csv("Ames_data.csv")
Ytest = myData[test.id,"Sale_Price"]
n = dim(myData)[1]
ntest = n*0.3
test.id = sample(1:n, ntest)
Y = myData$Sale_Price
myData[test.id,]$Sale_Price = NA
#Preprocessing
#Remove variables "Longitude'' and "Latitude''.
myData$Longitude <- NULL
myData$Latitude <- NULL
#Removing variable with more than 15% missing 
NAcol <- which(colSums(is.na(myData)) > 0)
myData$Garage_Yr_Blt <- NULL
# Remove dominatnig variables
myData$Street <- NULL  #Remove Variable Street 
myData$Utilities <- NULL #Remove because allpub dominates

numericVars <- which(sapply(myData,is.numeric))
numericVarNames <- names(numericVars)

DFnumeric <- myData[, names(myData) %in% numericVarNames]
DFfactors <- myData[, !(names(myData) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'Sale_Price']


#Winsorization
for(i in 1:ncol(DFnumeric)){
  x = myData$colnumeirc[i]
  winsorize(x, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
            na.rm = FALSE, type = 7)
}
#combine numeic data and categorical data into one dataframe and split into train and test dataframe
all = cbind(DFnumeric,DFfactors)
test = all[test.id,]
train = all[-test.id,]
ntest = dim(test)[1]
train$Sale_Price = myData[-test.id,"Sale_Price"]
test$Sale_Price <- NULL
#change saleprice to log scale
train$Sale_Price <- log(train$Sale_Price)
#GBM test.err[378] = [1] 0.2020245
myfit1 = gbm(Sale_Price ~ . , data = train, 
             distribution = "gaussian", 
             n.trees = 400,
             shrinkage = 0.05, 
             interaction.depth = 2, 
             bag.fraction = 0.5,
             cv.folds = 5)
gbm.perf(myfit1)
opt.size = gbm.perf(myfit1)
size = 1:myfit1$n.trees
test.err = rep(0, length(size))

for(i in 1:length(size)){
  y.pred = predict(myfit1, test, n.trees = size[i])
  yp = log(Y[test.id])
  test.err[i] = sqrt(sum((yp - y.pred)^2)/ntest)
}    
plot(test.err, type = "n")
lines(size, test.err, lwd = 2)
abline(v = opt.size, lwd = 2, lty = 2, col = "blue")


#Randomforest - 0.1267413
rfModel = randomForest(Sale_Price ~ ., data = train, importance = T, ntree=150); 
yhat.test = predict(rfModel, test)
yp = log(Y[test.id])
sqrt(sum((yp - yhat.test)^2)/ntest)
tmp = rfModel$mse
par(mfrow=c(1, 2))
plot(rfModel)
plot(c(0, rfModel$ntree), range(tmp), type="n",
     xlab = "Number of trees", ylab="Error")
lines(tmp)

#Test on 10 dataset
all.test.id = read.table("Project1_test_id.txt",header = FALSE)
#test_err = rep(c(0,0,0,0,0,0,0,0,0,0))
#GBM
test_report = rep(NA,10)
for (i in 1:10) {
  test.id = all.test.id[,i]
  tf<- ( myData$PID %in% test.id)
  index <- which(tf)
  test_ = all[index,]
  train_ = all[-index,]
  train_$Sale_Price = Y[-index]
  Ytest_ = Y[index]
  test_$Sale_Price <- NULL
  train_$Sale_Price <- log(train_$Sale_Price)

  myfit1 = gbm(Sale_Price ~ . , data = train_, 
               distribution = "gaussian", 
               n.trees = 400,
               shrinkage = 1,  #0.05
               interaction.depth = 2, 
               bag.fraction = 0.5,
               cv.folds = 5)
  gbm.perf(myfit1)
  opt.size = gbm.perf(myfit1)
  y.pred = predict(myfit1, test_, n.trees = opt.size)
  yp = log(Y[index])
  test.err = sqrt(sum((yp - y.pred)^2)/ntest)
  print(test.err)
  test_report[i] = test.err
  PID = test_$PID
  ypred = exp(y.pred)
  report = cbind(PID,ypred)
  #test_err[i] = test.err[opt.size]
}

#Randomforest
for (i in 1:10) {
  test.id = all.test.id[,i]
  tf<- ( myData$PID %in% test.id)
  index <- which(tf)
  test_ = all[index,]
  train_ = all[-index,]
  train_$Sale_Price = Y[-index]
  Ytest_ = Y[index]
  test_$Sale_Price <- NULL
  train_$Sale_Price <- log(train_$Sale_Price)
  #Randomforest
  rfModel = randomForest(Sale_Price ~ ., data = train_, importance = T, ntree=150); 
  yhat.test = predict(rfModel, test_)
  yp = log(Ytest_)
  rmse = sqrt(sum((yp - yhat.test)^2)/ntest)
  print(rmse)
}














