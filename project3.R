###########################clear environment###################
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "randomForest",
  "plyr",
  "e1071",
  "glmnet",
  "xgboost"
)
# mydata = read.csv("loan_stat542.csv")
# test.ids = read.csv("Project3_test_id.csv")
# test.id = which(mydata$id %in% test.ids$test3)
train = read.csv("train.csv")
test = read.csv("test.csv")
testid = test$id
mydata = rbind(train, test)
allid = mydata$id
missing = length(which(is.na(mydata)))
colnames(mydata)[colSums(is.na(mydata)) > 0] #print column names which contain missing values
#Missing values
#Mean ,median, mode imputation depending upon the nature of variables
mode_imputation <- function(co){ #mode_imputation - categorical variables - replace NA with "other"
  levels(co) = c(levels(co), "Other")
  co = factor(co, levels = levels(co))
  co[is.na(co)] = "Other"
  co
}
mydata$emp_title = mode_imputation(mydata$emp_title)
mydata$emp_length = mode_imputation(mydata$emp_length)
mydata$title = mode_imputation(mydata$title)
mydata$dti[is.na(mydata$dti)] = mean(mydata$dti[!is.na(mydata$dti)]) # by mean
mydata$revol_util[is.na(mydata$revol_util)] = mean(mydata$revol_util[!is.na(mydata$revol_util)]) # by mean
mydata$mort_acc[is.na(mydata$mort_acc)] = mean(mydata$mort_acc[!is.na(mydata$mort_acc)]) # by mean
mydata$pub_rec_bankruptcies[is.na(mydata$pub_rec_bankruptcies)] = 0 #by median

#Feature selection - remove variables with too many categories
remove_id = c("id", "sub_grade", "emp_title", "title", "zip_code", "addr_state") #Drop grade variable because it's a subset of grade
mydata[,remove_id] <- NULL
#change the "earliest_cr_line" to the number of months from Jan 2007
lctime <- Sys.getlocale("LC_TIME");
Sys.setlocale("LC_TIME", "C")
from = as.Date("1-1-2007", "%d-%m-%Y")
full_date = paste("1-",mydata$earliest_cr_line, sep = "")
mydata$earliest_cr_line = floor((as.Date(full_date, "%d-%b-%Y") - from) / 30)
Sys.setlocale("LC_TIME", lctime)

#encode loan status
# mydata$loan_status[mydata$loan_status=="Charged Off"] = "Default"
mydata$loan_status = as.numeric(mydata$loan_status)
mydata$loan_status[mydata$loan_status==1] = 1
mydata$loan_status[mydata$loan_status==3] = 0
mydata$loan_status[mydata$loan_status==2] = 1

#categorical - one hot encoding
numericVars <- which(sapply(mydata,is.numeric))
numericVarNames <- names(numericVars)
DFnumeric <- mydata[, names(mydata) %in% numericVarNames]
DFfactors <- mydata[, !(names(mydata) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'loan_status']
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
all = cbind(DFnumeric,DFdummies)

test.id = which(allid %in% testid)
test = all[test.id,]
train = all[-test.id,]
y = test$loan_status
test$loan_status <- NULL
# #SVM
# model.fit = svm(loan_status ~ ., data = train, probability = TRUE)
# predict(model.fit, test, probability = TRUE)
#logreg
# model.fit = glm(loan_status ~ ., data = train, family = "binomial")
# prob = predict(model.fit, test, type="response")

#lasso
X_train = train[, colnames(train) != 'loan_status']
X_train = as.matrix(X_train)#model.matrix(~., X_train)[, -1]
Y_train = train$loan_status
# cv.out = cv.glmnet(X_train, Y_train, family="binomial", alpha = 1)
# X_test = as.matrix(test)  #model.matrix(~. -id, test)[, -1]
# prob2 = predict(cv.out, s = cv.out$lambda.min, newx = X_test, type="response")
# p = as.matrix(prob1)
# testid = as.matrix(test.ids$test2)
# output = cbind(testid,p)
# colnames(output) = c("id","prob")
# write.table(output,"mysubmission.txt",row.names = FALSE, col.names = TRUE)
#xgboost
xgb_model = xgboost(data = X_train, label=Y_train,
                    objective = "binary:logistic", eval_metric = "logloss",
                    eta = 0.09,
                    nrounds = 1200,
                    verbose = TRUE)
X_test = as.matrix(test) 
prob3 = predict(xgb_model, X_test, type="response")
#write submission file
p = as.matrix(prob3)
testid = as.matrix(testid)
output = cbind(testid,p)
colnames(output) = c("id","prob")
write.table(output,"mysubmission.txt",row.names = FALSE, col.names = TRUE)


