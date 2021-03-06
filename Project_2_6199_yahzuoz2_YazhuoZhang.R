################ Load Environment ##################
# clean workspace
rm(list = ls())

# load necessary packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "lubridate",
  "forecast",
  "tidyverse"
)

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
}

# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept, num_model) {
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_month %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  if (num_model == 1) {
    test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
  } else if(num_model == 2) {
    test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
  } else {
    test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
  }
  
  test_month
}

# update forecasts in the global test dataframe
update_test <- function(test_month) {
  test <<- test %>%
    dplyr::left_join(test_month,
                     by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
    mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
    mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
    mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
    select(-Weekly_Pred1.x, -Weekly_Pred1.y,
           -Weekly_Pred2.x, -Weekly_Pred2.y,
           -Weekly_Pred3.x, -Weekly_Pred3.y)
}


##### Model Building Functions #####

# Forecasts out the last observation in the training data
snaive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # seasonal naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- snaive(store_ts, num_forecasts)$mean
  }
  test_ts
}
#basic tslm model
tslm.basic <- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    model <- tslm(store_ts ~ trend + season)
    fc <- forecast(model, h=num_forecasts)
    test_ts[, j] <- as.numeric(fc$mean)
  }
  test_ts
}

arima_forecast <- function(train_ts, test_ts){
  num_forecasts <- nrow(train_ts)
  train_ts[is.na(train_ts)] <- 0
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    model <- auto.arima(train_ts, xreg=fourier(train_ts, K = 14),
                               seasonal=FALSE)
    test_ts[, j] <- forecast(model, num_forecasts)$mean
      #snaive(store_ts, num_forecasts)$mean
  }
  test_ts
}

preprocess.svd <- function(train_ts, n.comp){
  train_ts[is.na(train_ts)] <- 0
  z <- svd(train_ts[, 2:ncol(train_ts)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train_ts[, 2:ncol(train_ts)] <- z$u %*% s %*% t(z$v)
  train_ts
}

stlf_forecast <- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- stlf(store_ts, h=num_forecasts, method='ets', ic='bic')$mean
  }
  test_ts
}
dynamic_forecast <- function(train_ts, test_ts){
  if(t > 8){
    stlf_forecast(train_ts,test_ts)
  } else {
    tslm.basic(train_ts, test_ts)
    #stlf_forecast(train_ts, test_ts)
  }
}
##### Prediction Loop #####

mypredict <- function() {
  ###### Create train and test time-series #######
  if (t > 1) {
    # append the previous periods test data to the current training data
    train <<- rbind(train, new_test)
  }
  
  # filter test data.frame for the month that needs predictions
  # backtesting starts during March 2011
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month <- test %>%
    filter(Date >= start_date & Date < end_date)
  
  # Dates are not the same across months!
  test_dates <- unique(test_month$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either
  all_stores <- unique(test_month$Store)
  num_stores <- length(all_stores)
  
  # Most importantly not all departments need predictions
  test_depts <- unique(test_month$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores)
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  #### Perform a individual forecasts for each department
  for (dept in test_depts) {
    # filter for the particular department in the training data
    train_dept_ts <- train %>%
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)
    
    # apply SVD for tr.d
    n.comp = 12
    tr.d = cbind(Date = train_dept_ts[, 1], preprocess.svd(train_dept_ts[, 2:ncol(train_dept_ts)], n.comp))
    
    ###### Model Fitting / Forecasting ######
    
    # naive forecast
    f_naive <- snaive_model(train_dept_ts, test_dept_ts)
    test_month <- update_forecast(test_month, f_naive, dept, 1)
    #tslm.basic forecast
    f_tslm_basic <- tslm.basic(train_dept_ts, test_dept_ts)
    test_month <- update_forecast(test_month, f_tslm_basic , dept, 2)
    
    train_dept_ts <- tr.d
    f_dyna <- dynamic_forecast(tr.d, test_dept_ts)
    test_month <- update_forecast(test_month, f_dyna, dept, 3)
    
    #tr.d.before = cast(train_dept_ts, Wk ~ Store, mean) 
  }
  
  # update global test dataframe
  update_test(test_month)
}
###############
