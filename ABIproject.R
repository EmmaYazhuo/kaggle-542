###########################clear environment###################
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "dummy",
  "corrplot",
  "matrixStats",
  "MASS",
  "glmnet",
  "leaps",
  "plyr"
)
graphics.off()
#load data
dataset = read.csv("PRSA_data_2010.1.1-2014.12.31.csv",sep = ",",header = TRUE)
#Check orignal dimension
dim(dataset)
#detect missing value
which(is.na(dataset))   #detect NA
dataset = na.omit(dataset) #Remove NA data from dataset

day_of_year <- function(year,month,day){
  if(year%%4!=0){
    if(month==1)
      dayofyear = day
    if(month==2)
      dayofyear = 31+day
    if(month==3)
      dayofyear = 31+28+day
    if(month==4)
      dayofyear = 31+28+31+day
    if(month==5)
      dayofyear = 31+28+31+30+day
    if(month==6)
      dayofyear = 31+28+31+30+31+day
    if(month==7)
      dayofyear = 31+28+31+30+31+30+day
    if(month==8)
      dayofyear = 31+28+31+30+31+30+31+day
    if(month==9)
      dayofyear = 31+28+31+30+31+30+31+31+day
    if(month==10)
      dayofyear = 31+28+31+30+31+30+31+31+30+day
    if(month==11)
      dayofyear = 31+28+31+30+31+30+31+31+30+31+day
    if(month==12)
      dayofyear = 31+28+31+30+31+30+31+31+30+31+30+day
  }
  if(year%%4==0){
    if(month==1)
      dayofyear = day
    if(month==2)
      dayofyear = 31+day
    if(month==3)
      dayofyear = 31+29+day
    if(month==4)
      dayofyear = 31+29+31+day
    if(month==5)
      dayofyear = 31+29+31+30+day
    if(month==6)
      dayofyear = 31+29+31+30+31+day
    if(month==7)
      dayofyear = 31+29+31+30+31+30+day
    if(month==8)
      dayofyear = 31+29+31+30+31+30+31+day
    if(month==9)
      dayofyear = 31+29+31+30+31+30+31+31+day
    if(month==10)
      dayofyear = 31+29+31+30+31+30+31+31+30+day
    if(month==11)
      dayofyear = 31+29+31+30+31+30+31+31+30+31+day
    if(month==12)
      dayofyear = 31+29+31+30+31+30+31+31+30+31+30+day
  }
  return(dayofyear)
}
encode_factor <- function(dataset){
  # lable encode for cbwd
  cbwd <- c('cv'=1,'NE'=2,'NW'=3,'SE'=4)
  dataset$cbwd<-as.integer(revalue(dataset$cbwd, cbwd))
  return(dataset)
}
Remove_outliers <- function(){
  summary(dataset$pm2.5)
  plot(dataset$pm2.5)#not Guassion  Distribution
  boxplot(dataset$pm2.5)
  #delete outliers
  outliers = which(dataset$pm2.5>950) #965 16486 16487
  dataset = dataset[-outliers,]
  #Plot Y and PM2.5
  plot(dataset$Y,dataset$pm2.5)
  outliers_1 = which(dataset$Y<400&dataset$pm2.5>600)
  outliers_2 = which(dataset$Y>=600&dataset$pm2.5<400)
  outliers_3 = which(dataset$Y>800&dataset$pm2.5<600)  
  dataset = dataset[-outliers_1,]
  dataset = dataset[-outliers_2,]
  dataset = dataset[-outliers_3,]
}
#Check demision again
dim(dataset)
process_time <- function(){
  #Remove index column 
  dataset = dataset[,2:ncol(dataset)]
  Y = dataset[,'pm2.5']
  #Add Yn-1 as varaible to Yn time serie
  Y = Y[1:length(Y)-1]
  dataset = dataset[2:nrow(dataset),]
  dataset = cbind(dataset,Y)
  # encode day column
  day = rep(0,nrow(dataset)) 
  for (i in c(1:nrow(dataset)))
  {
    day[i] = day_of_year(dataset[i,1],dataset[i,2],dataset[i,3])
  }
  dataset[,'day'] = day 
  day  = dataset[,'day']
  month = dataset[,'month']
  hour = dataset[,'hour']
  sin_month = sin(month*2*pi/12)
  cos_month = cos(month*2*pi/12)
  sin_day = 0
  cos_day = 0
  for (i in c(1:nrow(dataset))){
    if(dataset[i,'year']%%4!=0)
    {
      sin_day[i] = sin(day[i]*2*pi/365)
      cos_day[i] = cos(day[i]*2*pi/365)
    }
    if(dataset[i,'year']%%4==0)
    {
      sin_day[i] = sin(day[i]*2*pi/366)
      cos_day[i] = cos(day[i]*2*pi/366)
    }
  }
  sin_hour = sin(hour*2*pi/24)
  cos_hour = cos(hour*2*pi/24)
  #Delete useless varaibles
  dataset[,'month'] <- NULL
  dataset[,'day'] <- NULL
  dataset[,'hour'] <- NULL
  #Combine all together
  dataset = cbind(dataset,sin_month,cos_month,sin_day,cos_day,sin_hour,cos_hour)
}

plots <- function(){
  par(mfrow= c(1,2))
  hist(dataset$pm2.5, freq = FALSE, col = "cyan", main = "Histogram", xlab = "")
  lines(density(dataset$pm2.5), col="darkred")
  boxplot(dataset$pm2.5, main="Boxplot", col = "red")
  title(outer = TRUE, main = "\n Distribution of pm2.5")
}
correlation_plot <- function(){
  #Plot correlation matrix
  correlation = cor(dataset)
  par(mfrow=c(1,1))
  corrplot(correlation, method="circle")
}
# Seperation subset into X and Y training and test sets  80% / 20%
Y = dataset[,'pm2.5']
# scale(X) Normalization input
X = dataset[,-2]
#train-test-split
n = nrow(dataset)
ntest = round(n * 0.2) 
test.id = sample(1:n, ntest)
train_Y = Y[-test.id]
train_X = X[-test.id,]
test_Y = Y[test.id]
test_X = X[test.id,]
start_time = proc.time()
## Multilinear Models
### Linear models
fullmodel = lm('train_Y~.', data = as.data.frame(cbind(train_X,train_Y))) 
summary(fullmodel)
sum((train_Y-fullmodel$fitted.values)^2)/(n*0.8)
variable_selection <- function(){
  stepAIC = step(fullmodel, direction="both", trace = 0)
  n = dim(train_X)[1]
  stepBIC = step(fullmodel, direction="both", k=log(n),trace = 0) 
}
# Ridge Lasso Regression
Ridge = function(){
  lam.seq = exp(seq(-4, 4, length=100))
  cv.out = cv.glmnet(as.matrix(train_X), as.matrix(train_Y), alpha = 0, lambda=lam.seq) 
  plot(cv.out)
  #lambda.min = cv.out$lambda.min
  myridge = glmnet(as.matrix(train_X), as.matrix(train_Y), alpha=0, lambda = lam.seq)
  Ytest.pred = predict(myridge, s = cv.out$lambda.min, newx=as.matrix(test_X))
  mean((Ytest.pred - test_Y)^2) #483.5684
}

Lasso = function(){
  lam.seq = exp(seq(-4, 4, length=100))
  cv.out = cv.glmnet(as.matrix(train_X), as.matrix(train_Y), alpha = 1, lambda=lam.seq) 
  plot(cv.out)
  #lambda.min = cv.out$lambda.min
  mylasso = glmnet(as.matrix(train_X), as.matrix(train_Y), alpha=1, lambda = lam.seq)
  Ytest.pred = predict(mylasso, s = cv.out$lambda.min, newx=as.matrix(test_X))
  mean((Ytest.pred - test_Y)^2) #483.7143
} 
print((proc.time() - start_time)[3])


