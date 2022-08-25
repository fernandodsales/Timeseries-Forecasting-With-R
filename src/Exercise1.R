#===============================================================================
# Forecasting: Exam assignment 2022
# IESEG School of Management

# Fernando Delgado - April 28
#===============================================================================

# Source: 
# Van de Bossche, F. (2022). Forecasting. [Course].
# Lille: IESEG Management School. MSc in Big Data Analytics.


# Working Driectory 
setwd("C:/Users/fdelgado/OneDrive - IESEG/Documents/01. IESEG/20. Forecasting/Exam Assignment")

#===============================================================================
# Libraries & Data
#===============================================================================

# Data manipulation
for (i in c('dplyr','tidyverse','data.table', 'readxl')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Visualization
for (i in c('ggplot2', 'gcookbook')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Forecasting
for (i in c('fpp2')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Load data
data <- read_excel("./data/raw/DataSets2022.xlsx", sheet="Airpass_BE")
abe <- ts(data[,2], frequency = 12, start = 2003)
# Set Seed
set.seed(1)

# Train Test Split
train <- window(abe, start = 2003, end = c(2017,12))
test <- window(abe, start = 2018, end = c(2020, 2))
full_data <- window(abe, start = 2003, end = c(2020, 2))

#Length of test
h = length(test)

# Timeseries Plot
plot(abe, main="Belgium Air Passenger Transport",
     ylab="Passenger Count", xlab="Year")

#===============================================================================
# Q1
#===============================================================================

# Explore the data using relevant graphs, and discuss the properties of the data.
# Include and discuss a time series plot, a seasonal plot, a seasonal subseries plot
# and a (P)ACF plot.

# Timeseries Plot
plot(full_data, main="Belgium Air Passenger Transport",
     ylab="Passenger Count", xlab="Year") + 
  abline(v = 2018, col = "blue", lty = 2, lwd = 3)

# Seasonal Plot
seasonplot(full_data, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="Passenger Count", xlab="Month", col=rainbow(20), pch=19)

# Seasonal subseries
monthplot(full_data, ylab="Passenger Count", xlab="Month", xaxt="n",
          main="Seasonal Subseries Plot",type="l") + 
  axis(1, at=1:12, labels=month.abb, cex=0.8)

# Lag pot
lag.plot(full_data)

# ACF
acf(full_data)

mx=36
acf(full_data, lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

# Partial ACF
pacf(full_data, mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

#===============================================================================
# Q2
#===============================================================================
  
# Discuss whether a transformation and/or any other adjustment of the time
# series would be useful. If so, apply the most appropriate transformation and/or
# adjustments. Also, report the optimal Box-Cox lambda value that could be used
# to transform the time series. Clarify how you will proceed with the transformation
# in the remainder of the exercise.

# Box-Cox Examples
par(mfrow=c(2,2)) # Show 4 plots together
plot(BoxCox(full_data,lambda=0.5), main = "Square Root Transform")
plot(BoxCox(full_data,lambda= -1), main = "Reciprocal Transform")
plot(BoxCox(full_data,lambda=0), main = "Log Transform")
plot(BoxCox(full_data,lambda=1), main = "No Transform")

# Get lambda
l <- BoxCox.lambda(full_data)
l
# Example with lambda
par(mfrow=c(1,1))
plot(BoxCox(full_data,lambda=l), main = "Optimal Box-Cox Transform")

# Adjustment month length
par(mfrow=c(1,2))
plot(full_data, main="Belgium Air Passenger Transport",
     ylab="Passenger Count", xlab="Year")

plot(full_data/monthdays(full_data), main="Average Passenger Count
per day", ylab="Passenger Count", xlab="Year")

#===============================================================================
# Q3
#===============================================================================

# Create forecasts using the seasonal naive method. Check the residual diagnostics
# (including the Ljung-Box test) and the forecast accuracy (on the test set).

# Save Length of Test 
h = length(test)

# Seasonal Naive
abe_m1 <- snaive(train, h = h, lambda =l)

#Plot
par(mfrow=c(1,1))
plot(abe_m1)

# Ljung-Box Test
res <- residuals(abe_m1)
Box.test(res, lag=h, fitdf=0, type="Lj")

checkresiduals(abe_m1)

# Accuracy 
accuracy(abe_m1, test)

#===============================================================================
# Q4
#===============================================================================

# Use an STL decomposition to forecast the time series. Use the various underlying
# forecasting methods for the seasonally adjusted data (naive, rwdrift, ets, arima).
# Check the residual diagnostics and the forecast accuracy and select the best
# performing STL decomposition.

#Forecasting by decomposition
abe_m2 <- stlf(train, method="naive", h=h, lambda = l, biasadj = TRUE)
abe_m3 <- stlf(train, method="rwdrift", h=h, lambda = l, biasadj = TRUE)
abe_m4 <- stlf(train, method="ets", h=h, lambda = l, biasadj = TRUE)
abe_m5 <- stlf(train, method="arima", h=h, lambda = l, biasadj = TRUE)

models = c("STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda")

n <- length(models); n  #number of models

#naming of models for the given data set
m <- "abe_m"

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

for(i in 1:n) {
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), test)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

# Display Performance
a_train[2:5,]
a_test[2:5,]
round(res_matrix, digits = 4)

# Check residuals for rwdrift
checkresiduals(abe_m2)

# Check residuals for arima
checkresiduals(abe_m5)

#===============================================================================
# Q5
#===============================================================================
# Generate forecasts using ETS. First select the appropriate models yourself and
# discuss their performance. Compare these models with the results of the automated
# ETS procedure. Check the residual diagnostics and the forecast accuracy for the
# various ETS models you've considered. Present the parameters of the final ETS
# model and show the forecasts in a graph.

#stl decompose
plot(stl(ts(full_data[1:206], frequency=12), s.window = "periodic"))

# Auto ETS
m6 <- ets(train, lambda = l)
abe_m6 <- forecast(m6, h=h)

# AAA
m7 <- ets(train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
abe_m7 <- forecast(m7, h=h)   

m8 <- ets(train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
abe_m8 <- forecast(m8, h=h)  

# Performance
models = c("Seasonal Naive",
           "STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda", 
           "Auto ETS lambda", "AAA lambda", "AAdA lambda")

#number of models
n <- length(models); n  

#naming of models for the given data set
m <- "abe_m"

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

for(i in 1:n) {
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), test)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

# Display Performance
a_train[6:8,]
a_test[6:8,]
round(res_matrix[6:8,], digits = 4)

# Auto Models 
print(m6)

#===============================================================================
# Q6
#===============================================================================

# Generate forecasts using the auto.arima procedure. Present the estimated
# using the backward shift operator. Include the parameter estimates.
# Check the residual diagnostics and the forecast accuracy. Discuss your results,
# and if necessary compare these with other possible ARIMA models (e.g. if small
# changes in the model specification improve the properties of the residuals and/or
# the forecast accuracy).

nsdiffs(train)

# ARIMA models
m9 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE)
abe_m9 <- forecast(m9, h=h)
checkresiduals(abe_m9)

m10 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE, d=1, D=1)
abe_m10 <- forecast(m10, h=h)
checkresiduals(abe_m10)

# Performance
models = c("Seasonal Naive",
           "STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda", 
           "Auto ETS lambda", "AAA lambda", "AAdA lambda", 
           "ARIMA lambda", "ARIMA dD lambda")

#number of models
n <- length(models); n  

#naming of models for the given data set
m <- "abe_m"

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

for(i in 1:n) {
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), test)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

# Display Performance
a_train[9:10,]
a_test[9:10,]
round(res_matrix[9:10,], digits = 4)

#===============================================================================
# Q7
#===============================================================================

# Full performance
a_train
a_test
round(res_matrix, digits = 4)

# Cast as Dataframe
a_test <- as.data.frame(a_test)
a_test <- a_test[order(a_test$MASE, a_test$MASE),]

# Cast as Dataframe
res_matrix <- as.data.frame(res_matrix)
res_matrix <- res_matrix[order(res_matrix$'p-value', res_matrix$'p-value'),]
res_matrix <- res_matrix %>% 
  rename(
    pvalue = 'p-value'
  )

# Plot
ggplot(a_test,aes(rownames(a_test),MASE)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_text(aes(label = round(MASE, digits = 4)), vjust = 1.5, colour = "white")

# Plot 2
ggplot(res_matrix,aes(rownames(res_matrix),pvalue)) + 
  geom_bar(stat="identity", fill = "#FF6666") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_text(aes(label = round(pvalue, digits = 4)), vjust = 1.5, colour = "white")

# Check best model 
checkresiduals(m6)

#===============================================================================
# Q8
#===============================================================================  
# Generate out of sample forecasts up to December 2022, based on the complete
# time series (January 2003 - February 2020). Present your results.

# Check Params
m6

# Re fit model
new_fit <- ets(full_data, model="AAA", damped = TRUE, lambda=l, biasadj=FALSE)
abe_new_fit <- forecast(new_fit, h=34) #34 months up to dec 2022
plot(abe_new_fit)

#===============================================================================
# Q9
#===============================================================================  

# Forecast
plot(abe_new_fit)
lines(window(abe,start=c(2020,2)),col="red")



