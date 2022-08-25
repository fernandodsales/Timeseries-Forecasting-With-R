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
data <- fread("./data/raw/EnergyUsage_byType.csv")

renewable <- ts(data$RenewableETC, frequency = 12, start = 1997)

# Set Seed
set.seed(1)

# Timeseries Plot
plot(renewable, main="Korea Total Energy Consumption",
     ylab="1000toe", xlab="Year")

# Train Test Split
train <- window(renewable, start = 1997, end = c(2017,12))
test <- window(renewable, start = 2018, end = c(2021, 11))

#Length of test
h = length(test)

# Timeseries Plot
plot(renewable, main="Korea Total Energy Consumption",
     ylab="1000toe", xlab="Year") + 
  abline(v = 2018, col = "blue", lty = 2, lwd = 3)

# Seasonal Plot
seasonplot(renewable, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="1000toe", xlab="Month", col=rainbow(20), pch=19)

# Seasonal subseries
monthplot(renewable, ylab="1000toe", xlab="Month", xaxt="n",
          main="Seasonal Subseries Plot",type="l") + 
  axis(1, at=1:12, labels=month.abb, cex=0.8)

#ACF
acf(renewable)

mx=24
acf(renewable, lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

# Partial ACF
pacf(renewable, mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

# Box-Cox Examples
par(mfrow=c(2,2)) # Show 4 plots together
plot(BoxCox(renewable,lambda=0.5), main = "Square Root Transform")
plot(BoxCox(renewable,lambda= -1), main = "Reciprocal Transform")
plot(BoxCox(renewable,lambda=0), main = "Log Transform")
plot(BoxCox(renewable,lambda=1), main = "No Transform")

l <- BoxCox.lambda(renewable)
l

# Adjustment month length
par(mfrow=c(1,2))
plot(renewable, main="Total Renewable Energy Consumption",
     ylab="1000toe", xlab="Year")

plot(renewable/monthdays(renewable), main="Average R-Energy Consumption
per day", ylab="1000toe", xlab="Year")

# decomposition
plot(stl(ts(renewable, frequency=12), s.window = "periodic"))

# Models

# Seasonal Naive
abe_m1 <- snaive(train, h = h, lambda =l)

# Forecasting by decomposition
abe_m2 <- stlf(train, method="naive", h=h, lambda = l, biasadj = TRUE)
abe_m3 <- stlf(train, method="rwdrift", h=h, lambda = l, biasadj = TRUE)
abe_m4 <- stlf(train, method="ets", h=h, lambda = l, biasadj = TRUE)
abe_m5 <- stlf(train, method="arima", h=h, lambda = l, biasadj = TRUE)

# Auto ETS
m6 <- ets(train, lambda = l)
abe_m6 <- forecast(m6, h=h)

# AAA
m7 <- ets(train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
abe_m7 <- forecast(m7, h=h)   

m8 <- ets(train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
abe_m8 <- forecast(m8, h=h) 

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

# Best model 
checkresiduals(abe_m2)

# Re fit model
new_fit <- stlf(renewable, method="naive", h=h, lambda = l, biasadj = TRUE)
abe_new_fit <- forecast(new_fit, h=34) # 34 months up to dec 2022
par(mfrow=c(1,1))
plot(abe_new_fit)

# Forecast
plot(abe_new_fit)



