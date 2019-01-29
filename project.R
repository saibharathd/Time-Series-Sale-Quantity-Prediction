# Clearing the R environment


rm(list=ls(all=T))



#loading the required libraries

library(dplyr)
library(DMwR)
library(car)
library(MASS)
library(vegan)
library(dummies)
library(infotheo)
library(caTools)
library(caret)
library(glmnet)
library(ROCR)
drat:::addRepo("dmlc")
library(corrplot)
library(ggplot2)
library(imputeTS)
library(rpart)
library(kernlab)
library(TTR)
library(DataCombine)
library(graphics)
library(data.table)
library(Quandl)
library(lubridate)
library(forecast)
library(DataCombine)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)







#Reading the data from Execel Files
#buyer_basic_info<- read.csv("buyer_basic_info.csv",header = T,sep = ",")
#buyer_historical_category15_money<- read.csv("buyer_historical_category15_money.csv",header=T,sep=",")
#buyer_historical_category15_quantity<-read.csv("buyer_historical_category15_quantity.csv",header = T,sep = ",")
#key_product_IDs <- read.csv("key_product_IDs.csv",header = T,sep= ",")
#product_features <- read.csv("product_features.csv",header = T,sep = ",")

#Actual data to work on
product_distribution_training_set <- read.csv("product_distribution_training_set.csv",header = T,sep = ",")

trade_info_training <- read.csv("trade_info_training.csv",header = T,sep = ",")




###Overview on the distribution of data
dim(product_distribution_training_set)
summary(product_distribution_training_set)

### checking if all ids are unique(compare with rows in execel)
length(unique(trade_info_training$product_id))


### Basic info about that product
dim(product_distribution_training_set)
head(product_distribution_training_set)
names(product_distribution_training_set)
str(product_distribution_training_set)
tail(product_distribution_training_set)


### Missing values in time series
# * Some times there will be missing entries in dates which will create a missing day in the data or if it is quarter,month or annual .
# * Observe the data to find if any
head(product_distribution_training_set)
str(product_distribution_training_set)

sum(is.na(product_distribution_training_set))



#####finding the sum of all days and creating a coloumn
total <- apply(product_distribution_training_set,2,sum)
#View(product_distribution_training_set)

#####Converting columns into rows
m1 <- t(product_distribution_training_set)
converted_df <- data.frame(r1= row.names(m1), m1, row.names=NULL)
converted_df <-cbind.data.frame(converted_df,total)
#View(converted_df)

####final dataframe that can be used for training and testing is
final_df <- converted_df[-1,-1]
#View(final_df)


### Splitting of the Data into train and test
# * Random split is not possible because here we will be in need of sequence where by we miss the data points
# * splitting is done by sequential splitting
Train <- final_df[1:(nrow(final_df) - 14),]
Test <- final_df[(nrow(final_df) - 13):nrow(final_df),]



################################# Doing overall sales prediction for each day#######################


### converting into time series 
overall_sales <- ts(Train$total, frequency =7)



### Vizualize the time series Data
plot(overall_sales,type="l",lwd=3,col="red",xlab="week",ylab="Overall_sales",main="Time series plot for product1")



### Decomposed Time Series
### Decompose will provide us with the info on seasonality,trend and randomness
overall_decomposed = decompose(overall_sales)
plot(overall_decomposed,col="Red")



## HoltWinters model  with trend  and Seasonality
overall_holtforecast1 <- HoltWinters(overall_sales, seasonal="additive")
head(overall_holtforecast1$fitted)

overall_holtforecast2 <- HoltWinters(overall_sales, seasonal="multiplicative")
head(overall_holtforecast2$fitted)



### Prediction on the Train
overall_holtforecastTrain1 <- data.frame(overall_holtforecast1$fitted)
overall_holtforecastTrainpredictions1 <- overall_holtforecastTrain1$xhat
head(overall_holtforecastTrainpredictions1)


### Prediction on test data
overall_holtpriceforecast1<-  forecast(overall_holtforecast1,h = 14)
plot(overall_holtpriceforecast1,ylim = c(500,3000))

overall_holtpriceforecast2<- forecast(overall_holtforecast2,h=14)
plot(overall_holtpriceforecast2,ylim = c(500,3000))

### Define the metric hw 
overall_hwTestMape <- regr.eval(Test$total,overall_holtpriceforecast1$mean)
overall_hwTestMape

overall_hwTestMape2<-regr.eval(Test$total,overall_holtpriceforecast2$mean)
overall_hwTestMape2


### ARIMA
###  Auto Arima
overall_MODEL_ARIMA <- auto.arima(overall_sales, ic='aic')
summary(overall_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(overall_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
overall_pricearimaforecasts_autArima<- forecast(overall_MODEL_ARIMA,h=14)
plot(overall_pricearimaforecasts_autArima,flwd = 2)


### Define the metric AUTO ARIMA 
overall_autoarimaTestMape <- regr.eval(Test$total,overall_pricearimaforecasts_autArima$mean)
overall_autoarimaTestMape


### Manual Arima  ("SELECTED")
Acf(overall_sales)
Pacf(overall_sales)
overall_model4 <- arima(overall_sales,c(15,2,2))
overall_pricearimaforecasts_model4<- forecast(overall_model4,h=14)
plot(overall_pricearimaforecasts_model4,flwd = 2)
overall_arimaTestMape <- regr.eval(Test$total,overall_pricearimaforecasts_model4$mean)
overall_arimaTestMape   #  RMSE=138  MAPE=0.123

###prediction of actual values
overall_actual_df=ts(final_df$total, frequency =7)
Acf(overall_actual_df)
Pacf(overall_actual_df)
actual_overall_model4 <- arima(overall_actual_df,c(15,2,2))
actual_overall_pricearimaforecasts_model4<- forecast(actual_overall_model4,h=29)
plot(actual_overall_pricearimaforecasts_model4,flwd = 2)
overall_preds=actual_overall_pricearimaforecasts_model4$mean
overall_preds=data.frame(overall_preds)
#write.csv(overall_preds, file = "overll_sales.csv",col.names = TRUE,row.names = FALSE)





################################  Lets Start doing for all Products #################################

### converting into time series 
#pall_sales=Train[,1:100]
#p <- list()
#p_sales <- list()
##for (i in 1:100) {
  ##p[[i]]=pall_sales[,i]  
  ##p_sales[[i]] <- ts(p[i], frequency =7)
##}
##pholtforecast1 <-list()
##pholtforecast2 <-list()
##temp <- HoltWinters(p_sales[1], seasonal="additive")
##for (j in 1:100) {
  ##pholtforecast1[[j]] <- HoltWinters(p_sales[[j]], seasonal="additive")
  ##head(pholtforecast1[j]$fitted)
  ##pholtforecast2 <- HoltWinters(p_sales[[j]], seasonal="multiplicative")
  ##head(pholtforecast2$fitted)
##}

pall_sales=Train[,1:100]

##Converting all products into time series models
p1=ts(pall_sales$X1, frequency =7)
p2=ts(pall_sales$X2, frequency =7)
p3=ts(pall_sales$X3, frequency =7)
p4=ts(pall_sales$X4, frequency =7)
p5=ts(pall_sales$X5, frequency =7)
p6=ts(pall_sales$X6, frequency =7)
p7=ts(pall_sales$X7, frequency =7)
p8=ts(pall_sales$X8, frequency =7)
p9=ts(pall_sales$X9, frequency =7)
p10=ts(pall_sales$X10, frequency =7)
p11=ts(pall_sales$X11, frequency =7)
p12=ts(pall_sales$X12, frequency =7)
p13=ts(pall_sales$X13, frequency =7)
p14=ts(pall_sales$X14, frequency =7)
p15=ts(pall_sales$X15, frequency =7)
p16=ts(pall_sales$X16, frequency =7)
p17=ts(pall_sales$X17, frequency =7)
p18=ts(pall_sales$X18, frequency =7)
p19=ts(pall_sales$X19, frequency =7)
p20=ts(pall_sales$X20, frequency =7)
p21=ts(pall_sales$X21, frequency =7)
p22=ts(pall_sales$X22, frequency =7)
p23=ts(pall_sales$X23, frequency =7)
p24=ts(pall_sales$X24, frequency =7)
p25=ts(pall_sales$X25, frequency =7)
p26=ts(pall_sales$X26, frequency =7)
p27=ts(pall_sales$X27, frequency =7)
p28=ts(pall_sales$X28, frequency =7)
p29=ts(pall_sales$X29, frequency =7)
p30=ts(pall_sales$X30, frequency =7)
p31=ts(pall_sales$X31, frequency =7)
p32=ts(pall_sales$X32, frequency =7)
p33=ts(pall_sales$X33, frequency =7)
p34=ts(pall_sales$X34, frequency =7)
p35=ts(pall_sales$X35, frequency =7)
p36=ts(pall_sales$X36, frequency =7)
p37=ts(pall_sales$X37, frequency =7)
p38=ts(pall_sales$X38, frequency =7)
p39=ts(pall_sales$X39, frequency =7)
p40=ts(pall_sales$X40, frequency =7)
p41=ts(pall_sales$X41, frequency =7)
p42=ts(pall_sales$X42, frequency =7)
p43=ts(pall_sales$X43, frequency =7)
p44=ts(pall_sales$X44, frequency =7)
p45=ts(pall_sales$X45, frequency =7)
p46=ts(pall_sales$X46, frequency =7)
p47=ts(pall_sales$X47, frequency =7)
p48=ts(pall_sales$X48, frequency =7)
p49=ts(pall_sales$X49, frequency =7)
p50=ts(pall_sales$X50, frequency =7)
p51=ts(pall_sales$X51, frequency =7)
p52=ts(pall_sales$X52, frequency =7)
p53=ts(pall_sales$X53, frequency =7)
p54=ts(pall_sales$X54, frequency =7)
p55=ts(pall_sales$X55, frequency =7)
p56=ts(pall_sales$X56, frequency =7)
p57=ts(pall_sales$X57, frequency =7)
p58=ts(pall_sales$X58, frequency =7)
p59=ts(pall_sales$X59, frequency =7)
p60=ts(pall_sales$X60, frequency =7)
p61=ts(pall_sales$X61, frequency =7)
p62=ts(pall_sales$X62, frequency =7)
p63=ts(pall_sales$X63, frequency =7)
p64=ts(pall_sales$X64, frequency =7)
p65=ts(pall_sales$X65, frequency =7)
p66=ts(pall_sales$X66, frequency =7)
p67=ts(pall_sales$X67, frequency =7)
p68=ts(pall_sales$X68, frequency =7)
p69=ts(pall_sales$X69, frequency =7)
p70=ts(pall_sales$X70, frequency =7)
p71=ts(pall_sales$X71, frequency =7)
p72=ts(pall_sales$X72, frequency =7)
p73=ts(pall_sales$X73, frequency =7)
p74=ts(pall_sales$X74, frequency =7)
p75=ts(pall_sales$X75, frequency =7)
p76=ts(pall_sales$X76, frequency =7)
p77=ts(pall_sales$X77, frequency =7)
p78=ts(pall_sales$X78, frequency =7)
p79=ts(pall_sales$X79, frequency =7)
p80=ts(pall_sales$X80, frequency =7)
p81=ts(pall_sales$X81, frequency =7)
p82=ts(pall_sales$X82, frequency =7)
p83=ts(pall_sales$X83, frequency =7)
p84=ts(pall_sales$X84, frequency =7)
p85=ts(pall_sales$X85, frequency =7)
p86=ts(pall_sales$X86, frequency =7)
p87=ts(pall_sales$X87, frequency =7)
p88=ts(pall_sales$X88, frequency =7)
p89=ts(pall_sales$X89, frequency =7)
p90=ts(pall_sales$X90, frequency =7)
p91=ts(pall_sales$X91, frequency =7)
p92=ts(pall_sales$X92, frequency =7)
p93=ts(pall_sales$X93, frequency =7)
p94=ts(pall_sales$X94, frequency =7)
p95=ts(pall_sales$X95, frequency =7)
p96=ts(pall_sales$X96, frequency =7)
p97=ts(pall_sales$X97, frequency =7)
p98=ts(pall_sales$X98, frequency =7)
p99=ts(pall_sales$X99, frequency =7)
p100=ts(pall_sales$X100, frequency =7)



############################################ Product-1 ######################################


### Vizualize the time series Data
plot(p1,type="l",lwd=3,col="red",xlab="week",ylab="Overall_sales",main="Time series plot for product1")



### Decomposed Time Series
### Decompose will provide us with the info on seasonality,trend and randomness
p1_decomposed = decompose(p1)
plot(p1_decomposed,col="Red")



## HoltWinters model  with trend  and Seasonality
p1_holtforecast1 <- HoltWinters(p1, seasonal="additive")
head(p1_holtforecast1$fitted)

### ("SELECTED")
p1_holtforecast2 <- HoltWinters(p1, seasonal="multiplicative")
head(p1_holtforecast2$fitted)


### Prediction on test data
p1_holtpriceforecast1<-  forecast(p1_holtforecast1,h = 14)
plot(p1_holtpriceforecast1,ylim = c(-200,500))

p1_holtpriceforecast2<- forecast(p1_holtforecast2,h=14)
plot(p1_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p1_hwTestMape <- regr.eval(Test$X1,p1_holtpriceforecast1$mean)
p1_hwTestMape

p1_hwTestMape2<-regr.eval(Test$X2,p1_holtpriceforecast2$mean)
p1_hwTestMape2

### ARIMA
###  Auto Arima
p1_MODEL_ARIMA <- auto.arima(p1, ic='aic')
summary(p1_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p1_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p1_pricearimaforecasts_autArima<- forecast(p1_MODEL_ARIMA,h=14)
plot(p1_pricearimaforecasts_autArima,flwd = 2)
p1_autoarimaTestMape <- regr.eval(Test$X1,p1_pricearimaforecasts_autArima$mean)
p1_autoarimaTestMape



############################################ Product-2 ######################################


### Vizualize the time series Data
plot(p2,type="l",lwd=3,col="red",xlab="week",ylab="Overall_sales",main="Time series plot for product2")



### Decomposed Time Series
### Decompose will provide us with the info on seasonality,trend and randomness
p2_decomposed = decompose(p2)
plot(p2_decomposed,col="Red")



## HoltWinters model  with trend  and Seasonality
p2_holtforecast1 <- HoltWinters(p2, seasonal="additive")
head(p2_holtforecast1$fitted)

### 
p2_holtforecast2 <- HoltWinters(p2, seasonal="multiplicative")
head(p2_holtforecast2$fitted)


### Prediction on test data
p2_holtpriceforecast1<-  forecast(p2_holtforecast1,h = 14)
plot(p2_holtpriceforecast1,ylim = c(-200,500))

p2_holtpriceforecast2<- forecast(p2_holtforecast2,h=14)
plot(p2_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p2_hwTestMape <- regr.eval(Test$X2,p2_holtpriceforecast1$mean)
p2_hwTestMape

p2_hwTestMape2<-regr.eval(Test$X2,p2_holtpriceforecast2$mean)
p2_hwTestMape2

### ARIMA
###  Auto Arima        (SELECTED)
p2_MODEL_ARIMA <- auto.arima(p2, ic='aic')
summary(p2_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p2_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p2_pricearimaforecasts_autArima<- forecast(p2_MODEL_ARIMA,h=14)
plot(p2_pricearimaforecasts_autArima,flwd = 2)
p2_autoarimaTestMape <- regr.eval(Test$X2,p2_pricearimaforecasts_autArima$mean)
p2_autoarimaTestMape


############################################ Product-3 ######################################


## HoltWinters model  with trend  and Seasonality
p3_holtforecast1 <- HoltWinters(p3, seasonal="additive")

### 
p3_holtforecast2 <- HoltWinters(p3, seasonal="multiplicative")


### Prediction on test data
p3_holtpriceforecast1<-  forecast(p3_holtforecast1,h = 14)
plot(p3_holtpriceforecast1,ylim = c(-200,500))

p3_holtpriceforecast2<- forecast(p3_holtforecast2,h=14)
plot(p3_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p3_hwTestMape <- regr.eval(Test$X3,p3_holtpriceforecast1$mean)
p3_hwTestMape

p3_hwTestMape2<-regr.eval(Test$X3,p3_holtpriceforecast2$mean)
p3_hwTestMape2

### ARIMA
###  Auto Arima (SELECTED)
p3_MODEL_ARIMA <- auto.arima(p3, ic='aic')
summary(p3_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p3_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p3_pricearimaforecasts_autArima<- forecast(p3_MODEL_ARIMA,h=14)
plot(p3_pricearimaforecasts_autArima,flwd = 2)
p3_autoarimaTestMape <- regr.eval(Test$X3,p3_pricearimaforecasts_autArima$mean)
p3_autoarimaTestMape


############################################ Product-4 ######################################


## HoltWinters model  with trend  and Seasonality(SELECTED)
p4_holtforecast1 <- HoltWinters(p4, seasonal="additive")

### 
p4_holtforecast2 <- HoltWinters(p4, seasonal="multiplicative")


### Prediction on test data
p4_holtpriceforecast1<-  forecast(p4_holtforecast1,h = 14)
plot(p4_holtpriceforecast1,ylim = c(-200,500))

p4_holtpriceforecast2<- forecast(p4_holtforecast2,h=14)
plot(p4_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p4_hwTestMape <- regr.eval(Test$X4,p4_holtpriceforecast1$mean)
p4_hwTestMape

p4_hwTestMape2<-regr.eval(Test$X4,p4_holtpriceforecast2$mean)
p4_hwTestMape2

### ARIMA
###  Auto Arima
p4_MODEL_ARIMA <- auto.arima(p4, ic='aic')
summary(p4_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p4_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p4_pricearimaforecasts_autArima<- forecast(p4_MODEL_ARIMA,h=14)
plot(p4_pricearimaforecasts_autArima,flwd = 2)
p4_autoarimaTestMape <- regr.eval(Test$X4,p4_pricearimaforecasts_autArima$mean)
p4_autoarimaTestMape


############################################ Product-5 ######################################


## HoltWinters model  with trend  and Seasonality
p5_holtforecast1 <- HoltWinters(p5, seasonal="additive")

### 
p5_holtforecast2 <- HoltWinters(p5, seasonal="multiplicative")


### Prediction on test data
p5_holtpriceforecast1<-  forecast(p5_holtforecast1,h = 14)
plot(p5_holtpriceforecast1,ylim = c(-200,500))

p5_holtpriceforecast2<- forecast(p5_holtforecast2,h=14)
plot(p5_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p5_hwTestMape <- regr.eval(Test$X5,p5_holtpriceforecast1$mean)
p5_hwTestMape

p5_hwTestMape2<-regr.eval(Test$X3,p5_holtpriceforecast2$mean)
p5_hwTestMape2

### ARIMA   (SELECTED)
###  Auto Arima
p5_MODEL_ARIMA <- auto.arima(p5, ic='aic')
summary(p5_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p5_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p5_pricearimaforecasts_autArima<- forecast(p5_MODEL_ARIMA,h=14)
plot(p5_pricearimaforecasts_autArima,flwd = 2)
p5_autoarimaTestMape <- regr.eval(Test$X5,p5_pricearimaforecasts_autArima$mean)
p5_autoarimaTestMape



############################################ Product-6 ######################################


## HoltWinters model  with trend  and Seasonality (SELECTED)
p6_holtforecast1 <- HoltWinters(p6, seasonal="additive")

### 
p6_holtforecast2 <- HoltWinters(p6, seasonal="multiplicative")


### Prediction on test data
p6_holtpriceforecast1<-  forecast(p6_holtforecast1,h = 14)
plot(p6_holtpriceforecast1,ylim = c(-200,500))

p6_holtpriceforecast2<- forecast(p6_holtforecast2,h=14)
plot(p6_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p6_hwTestMape <- regr.eval(Test$X6,p6_holtpriceforecast1$mean)
p6_hwTestMape

p6_hwTestMape2<-regr.eval(Test$X6,p6_holtpriceforecast2$mean)
p6_hwTestMape2

### ARIMA
###  Auto Arima (SELECTED)
p6_MODEL_ARIMA <- auto.arima(p6, ic='aic')
summary(p6_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p6_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p6_pricearimaforecasts_autArima<- forecast(p6_MODEL_ARIMA,h=14)
plot(p6_pricearimaforecasts_autArima,flwd = 2)
p6_autoarimaTestMape <- regr.eval(Test$X6,p6_pricearimaforecasts_autArima$mean)
p6_autoarimaTestMape



############################################ Product-7 ######################################


## HoltWinters model  with trend  and Seasonality
p7_holtforecast1 <- HoltWinters(p7, seasonal="additive")

### (SELECTED)
p7_holtforecast2 <- HoltWinters(p7, seasonal="multiplicative")


### Prediction on test data
p7_holtpriceforecast1<-  forecast(p7_holtforecast1,h = 14)
plot(p7_holtpriceforecast1,ylim = c(-200,500))

p7_holtpriceforecast2<- forecast(p7_holtforecast2,h=14)
plot(p7_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p7_hwTestMape <- regr.eval(Test$X7,p7_holtpriceforecast1$mean)
p7_hwTestMape

p7_hwTestMape2<-regr.eval(Test$X7,p7_holtpriceforecast2$mean)
p7_hwTestMape2

### ARIMA
###  Auto Arima
p7_MODEL_ARIMA <- auto.arima(p7, ic='aic')
summary(p7_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p7_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p7_pricearimaforecasts_autArima<- forecast(p7_MODEL_ARIMA,h=14)
plot(p7_pricearimaforecasts_autArima,flwd = 2)
p7_autoarimaTestMape <- regr.eval(Test$X7,p7_pricearimaforecasts_autArima$mean)
p7_autoarimaTestMape


############################################ Product-8 ######################################


## HoltWinters model  with trend  and Seasonality (SELECTED)
p8_holtforecast1 <- HoltWinters(p8, seasonal="additive")

### 
p8_holtforecast2 <- HoltWinters(p8, seasonal="multiplicative")


### Prediction on test data
p8_holtpriceforecast1<-  forecast(p8_holtforecast1,h = 14)
plot(p8_holtpriceforecast1,ylim = c(-200,500))

p8_holtpriceforecast2<- forecast(p8_holtforecast2,h=14)
plot(p8_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p8_hwTestMape <- regr.eval(Test$X8,p8_holtpriceforecast1$mean)
p8_hwTestMape

p8_hwTestMape2<-regr.eval(Test$X8,p8_holtpriceforecast2$mean)
p8_hwTestMape2

### ARIMA
###  Auto Arima
p8_MODEL_ARIMA <- auto.arima(p8, ic='aic')
summary(p8_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p8_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p8_pricearimaforecasts_autArima<- forecast(p8_MODEL_ARIMA,h=14)
plot(p8_pricearimaforecasts_autArima,flwd = 2)
p8_autoarimaTestMape <- regr.eval(Test$X8,p8_pricearimaforecasts_autArima$mean)
p8_autoarimaTestMape


############################################ Product-9 ######################################


## HoltWinters model  with trend  and Seasonality (SELECTED)
p9_holtforecast1 <- HoltWinters(p9, seasonal="additive")

### 
p9_holtforecast2 <- HoltWinters(p9, seasonal="multiplicative")


### Prediction on test data
p9_holtpriceforecast1<-  forecast(p9_holtforecast1,h = 14)
plot(p9_holtpriceforecast1,ylim = c(-200,500))

p9_holtpriceforecast2<- forecast(p9_holtforecast2,h=14)
plot(p9_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p9_hwTestMape <- regr.eval(Test$X9,p9_holtpriceforecast1$mean)
p9_hwTestMape

p9_hwTestMape2<-regr.eval(Test$X9,p9_holtpriceforecast2$mean)
p9_hwTestMape2

### ARIMA
###  Auto Arima
p9_MODEL_ARIMA <- auto.arima(p9, ic='aic')
summary(p9_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p9_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p9_pricearimaforecasts_autArima<- forecast(p9_MODEL_ARIMA,h=14)
plot(p9_pricearimaforecasts_autArima,flwd = 2)
p9_autoarimaTestMape <- regr.eval(Test$X9,p9_pricearimaforecasts_autArima$mean)
p9_autoarimaTestMape



############################################ Product-10 ######################################


## HoltWinters model  with trend  and Seasonality(SELECTED)
p10_holtforecast1 <- HoltWinters(p10, seasonal="additive")

### 
p10_holtforecast2 <- HoltWinters(p10, seasonal="multiplicative")


### Prediction on test data
p10_holtpriceforecast1<-  forecast(p10_holtforecast1,h = 14)
plot(p10_holtpriceforecast1,ylim = c(-200,500))

p10_holtpriceforecast2<- forecast(p10_holtforecast2,h=14)
plot(p10_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p10_hwTestMape <- regr.eval(Test$X10,p10_holtpriceforecast1$mean)
p10_hwTestMape

p10_hwTestMape2<-regr.eval(Test$X10,p10_holtpriceforecast2$mean)
p10_hwTestMape2

### ARIMA
###  Auto Arima
p10_MODEL_ARIMA <- auto.arima(p10, ic='aic')
summary(p10_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p10_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p10_pricearimaforecasts_autArima<- forecast(p10_MODEL_ARIMA,h=14)
plot(p10_pricearimaforecasts_autArima,flwd = 2)
p10_autoarimaTestMape <- regr.eval(Test$X10,p10_pricearimaforecasts_autArima$mean)
p10_autoarimaTestMape

############################################ Product-11 ######################################


## HoltWinters model  with trend  and Seasonality(SELECTED)
p11_holtforecast1 <- HoltWinters(p11, seasonal="additive")

### 
p11_holtforecast2 <- HoltWinters(p11, seasonal="multiplicative")


### Prediction on test data
p11_holtpriceforecast1<-  forecast(p11_holtforecast1,h = 14)
plot(p11_holtpriceforecast1,ylim = c(-200,500))

p11_holtpriceforecast2<- forecast(p11_holtforecast2,h=14)
plot(p11_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p11_hwTestMape <- regr.eval(Test$X11,p11_holtpriceforecast1$mean)
p11_hwTestMape

p11_hwTestMape2<-regr.eval(Test$X11,p11_holtpriceforecast2$mean)
p11_hwTestMape2

### ARIMA
###  Auto Arima
p11_MODEL_ARIMA <- auto.arima(p11, ic='aic')
summary(p11_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p11_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p11_pricearimaforecasts_autArima<- forecast(p11_MODEL_ARIMA,h=14)
plot(p11_pricearimaforecasts_autArima,flwd = 2)
p11_autoarimaTestMape <- regr.eval(Test$X11,p11_pricearimaforecasts_autArima$mean)
p11_autoarimaTestMape


############################################ Product-12 ######################################


plot(p12)
## HoltWinters model  with trend  and Seasonality
p12_holtforecast1 <- HoltWinters(p12, seasonal="additive")

### 
p12_holtforecast2 <- HoltWinters(p12, seasonal="multiplicative")


### Prediction on test data
p12_holtpriceforecast1<-  forecast(p12_holtforecast1,h = 14)
plot(p12_holtpriceforecast1,ylim = c(-200,500))

p12_holtpriceforecast2<- forecast(p12_holtforecast2,h=14)
plot(p12_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p12_hwTestMape <- regr.eval(Test$X12,p12_holtpriceforecast1$mean)
p12_hwTestMape

p12_hwTestMape2<-regr.eval(Test$X12,p12_holtpriceforecast2$mean)
p12_hwTestMape2

### ARIMA
###  Auto Arima (SELECTED)
p12_MODEL_ARIMA <- auto.arima(p12, ic='aic')
summary(p12_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p12_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p12_pricearimaforecasts_autArima<- forecast(p12_MODEL_ARIMA,h=14)
plot(p12_pricearimaforecasts_autArima,flwd = 2)
p12_autoarimaTestMape <- regr.eval(Test$X12,p12_pricearimaforecasts_autArima$mean)
p12_autoarimaTestMape

############################################ Product-13 ######################################


plot(p13)
## HoltWinters model  with trend  and Seasonality  (SELECTED)
p13_holtforecast1 <- HoltWinters(p13, seasonal="additive")

### 
p13_holtforecast2 <- HoltWinters(p13, seasonal="multiplicative")


### Prediction on test data
p13_holtpriceforecast1<-  forecast(p13_holtforecast1,h = 14)
plot(p13_holtpriceforecast1,ylim = c(-200,500))

p13_holtpriceforecast2<- forecast(p13_holtforecast2,h=14)
plot(p13_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p13_hwTestMape <- regr.eval(Test$X13,p13_holtpriceforecast1$mean)
p13_hwTestMape

p13_hwTestMape2<-regr.eval(Test$X13,p13_holtpriceforecast2$mean)
p13_hwTestMape2

### ARIMA
###  Auto Arima
p13_MODEL_ARIMA <- auto.arima(p13, ic='aic')
summary(p13_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p13_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p13_pricearimaforecasts_autArima<- forecast(p13_MODEL_ARIMA,h=14)
plot(p13_pricearimaforecasts_autArima,flwd = 2)
p13_autoarimaTestMape <- regr.eval(Test$X13,p13_pricearimaforecasts_autArima$mean)
p13_autoarimaTestMape


############################################ Product-14 ######################################


plot(p14)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p14_holtforecast1 <- HoltWinters(p14, seasonal="additive")

### 
p14_holtforecast2 <- HoltWinters(p14, seasonal="multiplicative")


### Prediction on test data
p14_holtpriceforecast1<-  forecast(p14_holtforecast1,h = 14)
plot(p14_holtpriceforecast1,ylim = c(-200,500))

p14_holtpriceforecast2<- forecast(p14_holtforecast2,h=14)
plot(p12_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p14_hwTestMape <- regr.eval(Test$X14,p14_holtpriceforecast1$mean)
p14_hwTestMape

p14_hwTestMape2<-regr.eval(Test$X14,p14_holtpriceforecast2$mean)
p14_hwTestMape2

### ARIMA
###  Auto Arima
p14_MODEL_ARIMA <- auto.arima(p14, ic='aic')
summary(p14_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p14_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p14_pricearimaforecasts_autArima<- forecast(p14_MODEL_ARIMA,h=14)
plot(p14_pricearimaforecasts_autArima,flwd = 2)
p14_autoarimaTestMape <- regr.eval(Test$X14,p14_pricearimaforecasts_autArima$mean)
p14_autoarimaTestMape


############################################ Product-15 ######################################


plot(p15)
## HoltWinters model  with trend  and Seasonality   (SELECTED)
p15_holtforecast1 <- HoltWinters(p15, seasonal="additive")

### 
p15_holtforecast2 <- HoltWinters(p15, seasonal="multiplicative")


### Prediction on test data
p15_holtpriceforecast1<-  forecast(p15_holtforecast1,h = 14)
plot(p15_holtpriceforecast1,ylim = c(-200,500))

p15_holtpriceforecast2<- forecast(p15_holtforecast2,h=14)
plot(p15_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p15_hwTestMape <- regr.eval(Test$X15,p15_holtpriceforecast1$mean)
p15_hwTestMape

p15_hwTestMape2<-regr.eval(Test$X15,p15_holtpriceforecast2$mean)
p15_hwTestMape2

### ARIMA
###  Auto Arima
p15_MODEL_ARIMA <- auto.arima(p15, ic='aic')
summary(p15_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p15_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p15_pricearimaforecasts_autArima<- forecast(p15_MODEL_ARIMA,h=14)
plot(p15_pricearimaforecasts_autArima,flwd = 2)
p15_autoarimaTestMape <- regr.eval(Test$X15,p15_pricearimaforecasts_autArima$mean)
p15_autoarimaTestMape


############################################ Product-16 ######################################


plot(p16)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p16_holtforecast1 <- HoltWinters(p16, seasonal="additive")

### 
p16_holtforecast2 <- HoltWinters(p16, seasonal="multiplicative")


### Prediction on test data
p16_holtpriceforecast1<-  forecast(p16_holtforecast1,h = 14)
plot(p16_holtpriceforecast1,ylim = c(-200,500))

p16_holtpriceforecast2<- forecast(p16_holtforecast2,h=14)
plot(p16_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p16_hwTestMape <- regr.eval(Test$X16,p16_holtpriceforecast1$mean)
p16_hwTestMape

p16_hwTestMape2<-regr.eval(Test$X16,p16_holtpriceforecast2$mean)
p16_hwTestMape2

### ARIMA   
###  Auto Arima
p16_MODEL_ARIMA <- auto.arima(p16, ic='aic')
summary(p16_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p16_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p16_pricearimaforecasts_autArima<- forecast(p16_MODEL_ARIMA,h=14)
plot(p16_pricearimaforecasts_autArima,flwd = 2)
p16_autoarimaTestMape <- regr.eval(Test$X16,p16_pricearimaforecasts_autArima$mean)
p16_autoarimaTestMape


############################################ Product-17 ######################################


plot(p17)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p17_holtforecast1 <- HoltWinters(p17, seasonal="additive")

### 
p17_holtforecast2 <- HoltWinters(p17, seasonal="multiplicative")


### Prediction on test data
p17_holtpriceforecast1<-  forecast(p17_holtforecast1,h = 14)
plot(p17_holtpriceforecast1,ylim = c(-200,500))

p17_holtpriceforecast2<- forecast(p17_holtforecast2,h=14)
plot(p17_holtpriceforecast2,ylim = c(-200,500))

### Define the metric hw 
p17_hwTestMape <- regr.eval(Test$X17,p17_holtpriceforecast1$mean)
p17_hwTestMape

p17_hwTestMape2<-regr.eval(Test$X17,p17_holtpriceforecast2$mean)
p17_hwTestMape2

### ARIMA
###  Auto Arima
p17_MODEL_ARIMA <- auto.arima(p17, ic='aic')
summary(p17_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p17_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p17_pricearimaforecasts_autArima<- forecast(p17_MODEL_ARIMA,h=14)
plot(p17_pricearimaforecasts_autArima,flwd = 2)
p17_autoarimaTestMape <- regr.eval(Test$X17,p17_pricearimaforecasts_autArima$mean)
p17_autoarimaTestMape


############################################ Product-18 ######################################


plot(p18)
## HoltWinters model  with trend  and Seasonality
p18_holtforecast1 <- HoltWinters(p18, seasonal="additive")

### 
p18_holtforecast2 <- HoltWinters(p18, seasonal="multiplicative")


### Prediction on test data
p18_holtpriceforecast1<-  forecast(p18_holtforecast1,h = 14)
plot(p18_holtpriceforecast1,ylim = c(-100,200))

p18_holtpriceforecast2<- forecast(p18_holtforecast2,h=14)
plot(p18_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p18_hwTestMape <- regr.eval(Test$X18,p18_holtpriceforecast1$mean)
p18_hwTestMape

p18_hwTestMape2<-regr.eval(Test$X18,p18_holtpriceforecast2$mean)
p18_hwTestMape2

### ARIMA
###  Auto Arima (SELECTED)
p18_MODEL_ARIMA <- auto.arima(p18, ic='aic')
summary(p18_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p18_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p18_pricearimaforecasts_autArima<- forecast(p18_MODEL_ARIMA,h=14)
plot(p18_pricearimaforecasts_autArima,flwd = 2)
p18_autoarimaTestMape <- regr.eval(Test$X18,p18_pricearimaforecasts_autArima$mean)
p18_autoarimaTestMape



############################################ Product-19 ######################################


plot(p19)
## HoltWinters model  with trend  and Seasonality
p19_holtforecast1 <- HoltWinters(p19, seasonal="additive")

### 
p19_holtforecast2 <- HoltWinters(p19, seasonal="multiplicative")


### Prediction on test data
p19_holtpriceforecast1<-  forecast(p19_holtforecast1,h = 14)
plot(p19_holtpriceforecast1,ylim = c(-100,200))

p19_holtpriceforecast2<- forecast(p19_holtforecast2,h=14)
plot(p19_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p19_hwTestMape <- regr.eval(Test$X19,p19_holtpriceforecast1$mean)
p19_hwTestMape

p19_hwTestMape2<-regr.eval(Test$X19,p19_holtpriceforecast2$mean)
p19_hwTestMape2

### ARIMA
###  Auto Arima(SELECTED)
p19_MODEL_ARIMA <- auto.arima(p19, ic='aic')
summary(p19_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p19_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p19_pricearimaforecasts_autArima<- forecast(p19_MODEL_ARIMA,h=14)
plot(p19_pricearimaforecasts_autArima,flwd = 2)
p19_autoarimaTestMape <- regr.eval(Test$X19,p19_pricearimaforecasts_autArima$mean)
p19_autoarimaTestMape


############################################ Product-20 ######################################


plot(p20)
## HoltWinters model  with trend  and Seasonality
p20_holtforecast1 <- HoltWinters(p20, seasonal="additive")

### (SELECTED)
p20_holtforecast2 <- HoltWinters(p20, seasonal="multiplicative")


### Prediction on test data
p20_holtpriceforecast1<-  forecast(p20_holtforecast1,h = 14)
plot(p20_holtpriceforecast1)

p20_holtpriceforecast2<- forecast(p20_holtforecast2,h=14)
plot(p20_holtpriceforecast2)

### Define the metric hw 
p20_hwTestMape <- regr.eval(Test$X20,p20_holtpriceforecast1$mean)
p20_hwTestMape

p20_hwTestMape2<-regr.eval(Test$X20,p20_holtpriceforecast2$mean)
p20_hwTestMape2

### ARIMA
###  Auto Arima
p20_MODEL_ARIMA <- auto.arima(p20, ic='aic')
summary(p20_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p20_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p20_pricearimaforecasts_autArima<- forecast(p20_MODEL_ARIMA,h=14)
plot(p20_pricearimaforecasts_autArima,flwd = 2)
p20_autoarimaTestMape <- regr.eval(Test$X20,p20_pricearimaforecasts_autArima$mean)
p20_autoarimaTestMape



############################################ Product-21 ######################################


plot(p21)
## HoltWinters model  with trend  and Seasonality(SELECTED)
p21_holtforecast1 <- HoltWinters(p21, seasonal="additive")

### 
p21_holtforecast2 <- HoltWinters(p21, seasonal="multiplicative")


### Prediction on test data
p21_holtpriceforecast1<-  forecast(p21_holtforecast1,h = 14)
plot(p21_holtpriceforecast1,ylim = c(-100,200))

p21_holtpriceforecast2<- forecast(p21_holtforecast2,h=14)
plot(p21_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p21_hwTestMape <- regr.eval(Test$X21,p21_holtpriceforecast1$mean)
p21_hwTestMape

p21_hwTestMape2<-regr.eval(Test$X21,p21_holtpriceforecast2$mean)
p21_hwTestMape2

### ARIMA
###  Auto Arima
p21_MODEL_ARIMA <- auto.arima(p21, ic='aic')
summary(p21_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p21_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p21_pricearimaforecasts_autArima<- forecast(p21_MODEL_ARIMA,h=14)
plot(p21_pricearimaforecasts_autArima,flwd = 2)
p21_autoarimaTestMape <- regr.eval(Test$X21,p21_pricearimaforecasts_autArima$mean)
p21_autoarimaTestMape



############################################ Product-22 ######################################


plot(p22)
## HoltWinters model  with trend  and Seasonality(SELECTED)
p22_holtforecast1 <- HoltWinters(p22, seasonal="additive")

### 
p22_holtforecast2 <- HoltWinters(p22, seasonal="multiplicative")


### Prediction on test data
p22_holtpriceforecast1<-  forecast(p22_holtforecast1,h = 14)
plot(p22_holtpriceforecast1,ylim = c(-100,200))

p22_holtpriceforecast2<- forecast(p22_holtforecast2,h=14)
plot(p22_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p22_hwTestMape <- regr.eval(Test$X22,p22_holtpriceforecast1$mean)
p22_hwTestMape

p22_hwTestMape2<-regr.eval(Test$X22,p22_holtpriceforecast2$mean)
p22_hwTestMape2

### ARIMA
###  Auto Arima
p22_MODEL_ARIMA <- auto.arima(p22, ic='aic')
summary(p22_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p22_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p22_pricearimaforecasts_autArima<- forecast(p22_MODEL_ARIMA,h=14)
plot(p22_pricearimaforecasts_autArima,flwd = 2)
p22_autoarimaTestMape <- regr.eval(Test$X22,p22_pricearimaforecasts_autArima$mean)
p22_autoarimaTestMape


############################################ Product-23 ######################################


plot(p23)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p23_holtforecast1 <- HoltWinters(p23, seasonal="additive")

### 
p23_holtforecast2 <- HoltWinters(p23, seasonal="multiplicative")


### Prediction on test data
p23_holtpriceforecast1<-  forecast(p23_holtforecast1,h = 14)
plot(p23_holtpriceforecast1,ylim = c(-100,200))

p23_holtpriceforecast2<- forecast(p23_holtforecast2,h=14)
plot(p23_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p23_hwTestMape <- regr.eval(Test$X23,p23_holtpriceforecast1$mean)
p23_hwTestMape

p23_hwTestMape2<-regr.eval(Test$X23,p23_holtpriceforecast2$mean)
p23_hwTestMape2

### ARIMA
###  Auto Arima
p23_MODEL_ARIMA <- auto.arima(p23, ic='aic')
summary(p23_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p23_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p23_pricearimaforecasts_autArima<- forecast(p23_MODEL_ARIMA,h=14)
plot(p23_pricearimaforecasts_autArima,flwd = 2)
p23_autoarimaTestMape <- regr.eval(Test$X23,p23_pricearimaforecasts_autArima$mean)
p23_autoarimaTestMape



############################################ Product-24 ######################################


plot(p24)
## HoltWinters model  with trend  and Seasonality
p24_holtforecast1 <- HoltWinters(p24, seasonal="additive")

### 
p24_holtforecast2 <- HoltWinters(p24, seasonal="multiplicative")


### Prediction on test data
p24_holtpriceforecast1<-  forecast(p24_holtforecast1,h = 14)
plot(p24_holtpriceforecast1,ylim = c(-100,200))

p24_holtpriceforecast2<- forecast(p24_holtforecast2,h=14)
plot(p24_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p24_hwTestMape <- regr.eval(Test$X24,p24_holtpriceforecast1$mean)
p24_hwTestMape

p24_hwTestMape2<-regr.eval(Test$X24,p24_holtpriceforecast2$mean)
p24_hwTestMape2

### ARIMA (SELECTED)
###  Auto Arima
p24_MODEL_ARIMA <- auto.arima(p24, ic='aic')
summary(p24_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p24_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p24_pricearimaforecasts_autArima<- forecast(p24_MODEL_ARIMA,h=14)
plot(p24_pricearimaforecasts_autArima,flwd = 2)
p24_autoarimaTestMape <- regr.eval(Test$X24,p24_pricearimaforecasts_autArima$mean)
p24_autoarimaTestMape



############################################ Product-25 ######################################


plot(p25)
## HoltWinters model  with trend  and Seasonality  (SELECTED)
p25_holtforecast1 <- HoltWinters(p25, seasonal="additive")

### 
p25_holtforecast2 <- HoltWinters(p25, seasonal="multiplicative")


### Prediction on test data
p25_holtpriceforecast1<-  forecast(p25_holtforecast1,h = 14)
plot(p25_holtpriceforecast1,ylim = c(-100,200))

p25_holtpriceforecast2<- forecast(p25_holtforecast2,h=14)
plot(p25_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p25_hwTestMape <- regr.eval(Test$X25,p25_holtpriceforecast1$mean)
p25_hwTestMape

p25_hwTestMape2<-regr.eval(Test$X25,p25_holtpriceforecast2$mean)
p25_hwTestMape2

### ARIMA
###  Auto Arima
p25_MODEL_ARIMA <- auto.arima(p25, ic='aic')
summary(p25_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p25_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p25_pricearimaforecasts_autArima<- forecast(p25_MODEL_ARIMA,h=14)
plot(p25_pricearimaforecasts_autArima,flwd = 2)
p25_autoarimaTestMape <- regr.eval(Test$X25,p25_pricearimaforecasts_autArima$mean)
p25_autoarimaTestMape


############################################ Product-26 ######################################


plot(p26)
## HoltWinters model  with trend  and Seasonality
p26_holtforecast1 <- HoltWinters(p26, seasonal="additive")

### 
p26_holtforecast2 <- HoltWinters(p26, seasonal="multiplicative")


### Prediction on test data
p26_holtpriceforecast1<-  forecast(p26_holtforecast1,h = 14)
plot(p26_holtpriceforecast1,ylim = c(-100,200))

p26_holtpriceforecast2<- forecast(p26_holtforecast2,h=14)
plot(p26_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p26_hwTestMape <- regr.eval(Test$X26,p26_holtpriceforecast1$mean)
p26_hwTestMape

p26_hwTestMape2<-regr.eval(Test$X26,p26_holtpriceforecast2$mean)
p26_hwTestMape2

### ARIMA(SELECTED)
###  Auto Arima
p26_MODEL_ARIMA <- auto.arima(p26, ic='aic')
summary(p26_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p26_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p26_pricearimaforecasts_autArima<- forecast(p26_MODEL_ARIMA,h=14)
plot(p26_pricearimaforecasts_autArima,flwd = 2)
p26_autoarimaTestMape <- regr.eval(Test$X26,p26_pricearimaforecasts_autArima$mean)
p26_autoarimaTestMape



############################################ Product-27 ######################################


plot(p27)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p27_holtforecast1 <- HoltWinters(p27, seasonal="additive")

### 
p27_holtforecast2 <- HoltWinters(p27, seasonal="multiplicative")


### Prediction on test data
p27_holtpriceforecast1<-  forecast(p27_holtforecast1,h = 14)
plot(p27_holtpriceforecast1,ylim = c(-100,200))

p27_holtpriceforecast2<- forecast(p27_holtforecast2,h=14)
plot(p27_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p27_hwTestMape <- regr.eval(Test$X27,p27_holtpriceforecast1$mean)
p27_hwTestMape

p27_hwTestMape2<-regr.eval(Test$X27,p27_holtpriceforecast2$mean)
p27_hwTestMape2

### ARIMA
###  Auto Arima
p27_MODEL_ARIMA <- auto.arima(p27, ic='aic')
summary(p27_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p27_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p27_pricearimaforecasts_autArima<- forecast(p27_MODEL_ARIMA,h=14)
plot(p27_pricearimaforecasts_autArima,flwd = 2)
p27_autoarimaTestMape <- regr.eval(Test$X27,p27_pricearimaforecasts_autArima$mean)
p27_autoarimaTestMape



############################################ Product-28 ######################################


plot(p28)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p28_holtforecast1 <- HoltWinters(p28, seasonal="additive")

### 
p28_holtforecast2 <- HoltWinters(p28, seasonal="multiplicative")


### Prediction on test data
p28_holtpriceforecast1<-  forecast(p28_holtforecast1,h = 14)
plot(p28_holtpriceforecast1,ylim = c(-100,200))

p28_holtpriceforecast2<- forecast(p28_holtforecast2,h=14)
plot(p28_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p28_hwTestMape <- regr.eval(Test$X28,p28_holtpriceforecast1$mean)
p28_hwTestMape

p28_hwTestMape2<-regr.eval(Test$X28,p28_holtpriceforecast2$mean)
p28_hwTestMape2

### ARIMA
###  Auto Arima
p28_MODEL_ARIMA <- auto.arima(p28, ic='aic')
summary(p28_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p28_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p28_pricearimaforecasts_autArima<- forecast(p28_MODEL_ARIMA,h=14)
plot(p28_pricearimaforecasts_autArima,flwd = 2)
p28_autoarimaTestMape <- regr.eval(Test$X28,p28_pricearimaforecasts_autArima$mean)
p18_autoarimaTestMape



############################################ Product-29 ######################################


plot(p29)
## HoltWinters model  with trend  and Seasonality  (SELECTED)
p29_holtforecast1 <- HoltWinters(p29, seasonal="additive")

### 
p29_holtforecast2 <- HoltWinters(p29, seasonal="multiplicative")


### Prediction on test data
p29_holtpriceforecast1<-  forecast(p29_holtforecast1,h = 14)
plot(p29_holtpriceforecast1)

p29_holtpriceforecast2<- forecast(p29_holtforecast2,h=14)
plot(p29_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p29_hwTestMape <- regr.eval(Test$X29,p29_holtpriceforecast1$mean)
p29_hwTestMape

p29_hwTestMape2<-regr.eval(Test$X29,p29_holtpriceforecast2$mean)
p29_hwTestMape2

### ARIMA
###  Auto Arima
p29_MODEL_ARIMA <- auto.arima(p29, ic='aic')
summary(p29_MODEL_ARIMA)
### Box Ljung Test
# Box test on our auto.arima model
Box.test(p29_MODEL_ARIMA$residuals, lag = 2, type = "Ljung-Box")
p29_pricearimaforecasts_autArima<- forecast(p29_MODEL_ARIMA,h=14)
plot(p29_pricearimaforecasts_autArima,flwd = 2)
p29_autoarimaTestMape <- regr.eval(Test$X29,p29_pricearimaforecasts_autArima$mean)
p29_autoarimaTestMape


############################################ Product-30 ######################################


plot(p30)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p30_holtforecast1 <- HoltWinters(p30, seasonal="additive")

### 
p30_holtforecast2 <- HoltWinters(p30, seasonal="multiplicative")


### Prediction on test data
p30_holtpriceforecast1<-  forecast(p30_holtforecast1,h = 14)
plot(p30_holtpriceforecast1,ylim = c(-100,200))

p30_holtpriceforecast2<- forecast(p30_holtforecast2,h=14)
plot(p30_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p30_hwTestMape <- regr.eval(Test$X30,p30_holtpriceforecast1$mean)
p30_hwTestMape

p30_hwTestMape2<-regr.eval(Test$X30,p18_holtpriceforecast2$mean)
p30_hwTestMape2

### ARIMA
###  Auto Arima
p30_MODEL_ARIMA <- auto.arima(p30, ic='aic')
p30_pricearimaforecasts_autArima<- forecast(p30_MODEL_ARIMA,h=14)
plot(p30_pricearimaforecasts_autArima,flwd = 2)
p30_autoarimaTestMape <- regr.eval(Test$X30,p30_pricearimaforecasts_autArima$mean)
p30_autoarimaTestMape



############################################ Product-31 ######################################

plot(p31)
## HoltWinters model  with trend  and Seasonality  (SELECTED)
p31_holtforecast1 <- HoltWinters(p31, seasonal="additive")

### 
p31_holtforecast2 <- HoltWinters(p31, seasonal="multiplicative")


### Prediction on test data
p31_holtpriceforecast1<-  forecast(p31_holtforecast1,h = 14)
plot(p31_holtpriceforecast1)

p31_holtpriceforecast2<- forecast(p31_holtforecast2,h=14)
plot(p31_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p31_hwTestMape <- regr.eval(Test$X31,p31_holtpriceforecast1$mean)
p31_hwTestMape

p31_hwTestMape2<-regr.eval(Test$X31,p31_holtpriceforecast2$mean)
p31_hwTestMape2

### ARIMA
###  Auto Arima
p31_MODEL_ARIMA <- auto.arima(p31, ic='aic')
p31_pricearimaforecasts_autArima<- forecast(p31_MODEL_ARIMA,h=14)
plot(p31_pricearimaforecasts_autArima,flwd = 2)
p31_autoarimaTestMape <- regr.eval(Test$X31,p31_pricearimaforecasts_autArima$mean)
p31_autoarimaTestMape



############################################ Product-32 ######################################

plot(p32)
## HoltWinters model  with trend  and Seasonality (SELECTED) 
p32_holtforecast1 <- HoltWinters(p32, seasonal="additive")

### 
p32_holtforecast2 <- HoltWinters(p32, seasonal="multiplicative")


### Prediction on test data
p32_holtpriceforecast1<-  forecast(p32_holtforecast1,h = 14)
plot(p32_holtpriceforecast1)

p32_holtpriceforecast2<- forecast(p32_holtforecast2,h=14)
plot(p32_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p32_hwTestMape <- regr.eval(Test$X32,p32_holtpriceforecast1$mean)
p32_hwTestMape

p32_hwTestMape2<-regr.eval(Test$X32,p32_holtpriceforecast2$mean)
p32_hwTestMape2

### ARIMA
###  Auto Arima
p32_MODEL_ARIMA <- auto.arima(p32, ic='aic')
p32_pricearimaforecasts_autArima<- forecast(p32_MODEL_ARIMA,h=14)
plot(p32_pricearimaforecasts_autArima,flwd = 2)
p32_autoarimaTestMape <- regr.eval(Test$X32,p32_pricearimaforecasts_autArima$mean)
p32_autoarimaTestMape

############################################ Product-33 ######################################

plot(p33)
## HoltWinters model  with trend  and Seasonality 
p33_holtforecast1 <- HoltWinters(p33, seasonal="additive")

### 
p33_holtforecast2 <- HoltWinters(p33, seasonal="multiplicative")


### Prediction on test data
p33_holtpriceforecast1<-  forecast(p33_holtforecast1,h = 14)
plot(p33_holtpriceforecast1)

p33_holtpriceforecast2<- forecast(p33_holtforecast2,h=14)
plot(p33_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p33_hwTestMape <- regr.eval(Test$X33,p33_holtpriceforecast1$mean)
p33_hwTestMape

p33_hwTestMape2<-regr.eval(Test$X33,p33_holtpriceforecast2$mean)
p33_hwTestMape2

### ARIMA  (SELECTED)
###  Auto Arima
p33_MODEL_ARIMA <- auto.arima(p33, ic='aic')
p33_pricearimaforecasts_autArima<- forecast(p33_MODEL_ARIMA,h=14)
plot(p33_pricearimaforecasts_autArima,flwd = 2)
p33_autoarimaTestMape <- regr.eval(Test$X33,p33_pricearimaforecasts_autArima$mean)
p33_autoarimaTestMape


############################################ Product-34 ######################################

plot(p34)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p34_holtforecast1 <- HoltWinters(p34, seasonal="additive")

### 
p34_holtforecast2 <- HoltWinters(p34, seasonal="multiplicative")


### Prediction on test data
p34_holtpriceforecast1<-  forecast(p34_holtforecast1,h = 14)
plot(p34_holtpriceforecast1)

p34_holtpriceforecast2<- forecast(p34_holtforecast2,h=14)
plot(p34_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p34_hwTestMape <- regr.eval(Test$X34,p34_holtpriceforecast1$mean)
p34_hwTestMape

p34_hwTestMape2<-regr.eval(Test$X34,p34_holtpriceforecast2$mean)
p34_hwTestMape2

### ARIMA
###  Auto Arima
p34_MODEL_ARIMA <- auto.arima(p34, ic='aic')
p34_pricearimaforecasts_autArima<- forecast(p34_MODEL_ARIMA,h=14)
plot(p34_pricearimaforecasts_autArima,flwd = 2)
p34_autoarimaTestMape <- regr.eval(Test$X34,p34_pricearimaforecasts_autArima$mean)
p34_autoarimaTestMape

############################################ Product-35 ######################################

plot(p35)
## HoltWinters model  with trend  and Seasonality
p35_holtforecast1 <- HoltWinters(p35, seasonal="additive")

### 
p35_holtforecast2 <- HoltWinters(p35, seasonal="multiplicative")


### Prediction on test data
p35_holtpriceforecast1<-  forecast(p35_holtforecast1,h = 14)
plot(p35_holtpriceforecast1)

p35_holtpriceforecast2<- forecast(p35_holtforecast2,h=14)
plot(p35_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p35_hwTestMape <- regr.eval(Test$X35,p35_holtpriceforecast1$mean)
p35_hwTestMape

p35_hwTestMape2<-regr.eval(Test$X35,p35_holtpriceforecast2$mean)
p35_hwTestMape2

### ARIMA   (SELECTED)
###  Auto Arima
p35_MODEL_ARIMA <- auto.arima(p35, ic='aic')
p35_pricearimaforecasts_autArima<- forecast(p35_MODEL_ARIMA,h=14)
plot(p35_pricearimaforecasts_autArima,flwd = 2)
p35_autoarimaTestMape <- regr.eval(Test$X35,p35_pricearimaforecasts_autArima$mean)
p35_autoarimaTestMape


############################################ Product-36 ######################################

plot(p36)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p36_holtforecast1 <- HoltWinters(p36, seasonal="additive")

### 
p36_holtforecast2 <- HoltWinters(p36, seasonal="multiplicative")


### Prediction on test data
p36_holtpriceforecast1<-  forecast(p36_holtforecast1,h = 14)
plot(p36_holtpriceforecast1)

p36_holtpriceforecast2<- forecast(p36_holtforecast2,h=14)
plot(p36_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p36_hwTestMape <- regr.eval(Test$X36,p36_holtpriceforecast1$mean)
p36_hwTestMape

p36_hwTestMape2<-regr.eval(Test$X36,p36_holtpriceforecast2$mean)
p36_hwTestMape2

### ARIMA
###  Auto Arima
p36_MODEL_ARIMA <- auto.arima(p36, ic='aic')
p36_pricearimaforecasts_autArima<- forecast(p36_MODEL_ARIMA,h=14)
plot(p36_pricearimaforecasts_autArima,flwd = 2)
p36_autoarimaTestMape <- regr.eval(Test$X36,p36_pricearimaforecasts_autArima$mean)
p36_autoarimaTestMape

############################################ Product-37 ######################################

plot(p37)
## HoltWinters model  with trend  and Seasonality
p37_holtforecast1 <- HoltWinters(p37, seasonal="additive")

### 
p37_holtforecast2 <- HoltWinters(p37, seasonal="multiplicative")


### Prediction on test data
p37_holtpriceforecast1<-  forecast(p37_holtforecast1,h = 14)
plot(p37_holtpriceforecast1)

p37_holtpriceforecast2<- forecast(p37_holtforecast2,h=14)
plot(p37_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p37_hwTestMape <- regr.eval(Test$X37,p37_holtpriceforecast1$mean)
p37_hwTestMape

p37_hwTestMape2<-regr.eval(Test$X37,p37_holtpriceforecast2$mean)
p37_hwTestMape2

### ARIMA
###  Auto Arima (SELECTED)
p37_MODEL_ARIMA <- auto.arima(p37, ic='aic')
p37_pricearimaforecasts_autArima<- forecast(p37_MODEL_ARIMA,h=14)
plot(p37_pricearimaforecasts_autArima,flwd = 2)
p37_autoarimaTestMape <- regr.eval(Test$X37,p37_pricearimaforecasts_autArima$mean)
p37_autoarimaTestMape


############################################ Product-38 ######################################

plot(p38)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p38_holtforecast1 <- HoltWinters(p38, seasonal="additive")

### 
p38_holtforecast2 <- HoltWinters(p38, seasonal="multiplicative")


### Prediction on test data
p38_holtpriceforecast1<-  forecast(p38_holtforecast1,h = 14)
plot(p38_holtpriceforecast1)

p38_holtpriceforecast2<- forecast(p38_holtforecast2,h=14)
plot(p38_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p38_hwTestMape <- regr.eval(Test$X38,p38_holtpriceforecast1$mean)
p38_hwTestMape

p38_hwTestMape2<-regr.eval(Test$X38,p38_holtpriceforecast2$mean)
p38_hwTestMape2

### ARIMA
###  Auto Arima
p38_MODEL_ARIMA <- auto.arima(p38, ic='aic')
p38_pricearimaforecasts_autArima<- forecast(p38_MODEL_ARIMA,h=14)
plot(p38_pricearimaforecasts_autArima,flwd = 2)
p38_autoarimaTestMape <- regr.eval(Test$X38,p38_pricearimaforecasts_autArima$mean)
p38_autoarimaTestMape


############################################ Product-39 ######################################

plot(p39)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p39_holtforecast1 <- HoltWinters(p39, seasonal="additive")

### 
p39_holtforecast2 <- HoltWinters(p39, seasonal="multiplicative")


### Prediction on test data
p39_holtpriceforecast1<-  forecast(p39_holtforecast1,h = 14)
plot(p39_holtpriceforecast1)

p39_holtpriceforecast2<- forecast(p39_holtforecast2,h=14)
plot(p39_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p39_hwTestMape <- regr.eval(Test$X39,p39_holtpriceforecast1$mean)
p39_hwTestMape

p39_hwTestMape2<-regr.eval(Test$X39,p39_holtpriceforecast2$mean)
p39_hwTestMape2

### ARIMA (SELECTED)
###  Auto Arima
p39_MODEL_ARIMA <- auto.arima(p39, ic='aic')
p39_pricearimaforecasts_autArima<- forecast(p39_MODEL_ARIMA,h=14)
plot(p39_pricearimaforecasts_autArima,flwd = 2)
p39_autoarimaTestMape <- regr.eval(Test$X39,p39_pricearimaforecasts_autArima$mean)
p39_autoarimaTestMape

############################################ Product-40 ######################################

plot(p40)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p40_holtforecast1 <- HoltWinters(p40, seasonal="additive")

### 
p40_holtforecast2 <- HoltWinters(p40, seasonal="multiplicative")


### Prediction on test data
p40_holtpriceforecast1<-  forecast(p40_holtforecast1,h = 14)
plot(p40_holtpriceforecast1)

p40_holtpriceforecast2<- forecast(p40_holtforecast2,h=14)
plot(p40_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p40_hwTestMape <- regr.eval(Test$X40,p40_holtpriceforecast1$mean)
p40_hwTestMape

p40_hwTestMape2<-regr.eval(Test$X40,p34_holtpriceforecast2$mean)
p40_hwTestMape2

### ARIMA
###  Auto Arima
p40_MODEL_ARIMA <- auto.arima(p40, ic='aic')
p40_pricearimaforecasts_autArima<- forecast(p40_MODEL_ARIMA,h=14)
plot(p40_pricearimaforecasts_autArima,flwd = 2)
p40_autoarimaTestMape <- regr.eval(Test$X40,p40_pricearimaforecasts_autArima$mean)
p40_autoarimaTestMape


############################################ Product-41 ######################################

plot(p41)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p41_holtforecast1 <- HoltWinters(p41, seasonal="additive")

### 
p41_holtforecast2 <- HoltWinters(p41, seasonal="multiplicative")


### Prediction on test data
p41_holtpriceforecast1<-  forecast(p41_holtforecast1,h = 14)
plot(p41_holtpriceforecast1)

p41_holtpriceforecast2<- forecast(p41_holtforecast2,h=14)
plot(p41_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p41_hwTestMape <- regr.eval(Test$X41,p41_holtpriceforecast1$mean)
p41_hwTestMape

p41_hwTestMape2<-regr.eval(Test$X41,p41_holtpriceforecast2$mean)
p41_hwTestMape2

### ARIMA
###  Auto Arima
p41_MODEL_ARIMA <- auto.arima(p41, ic='aic')
p41_pricearimaforecasts_autArima<- forecast(p41_MODEL_ARIMA,h=14)
plot(p41_pricearimaforecasts_autArima,flwd = 2)
p41_autoarimaTestMape <- regr.eval(Test$X41,p41_pricearimaforecasts_autArima$mean)
p41_autoarimaTestMape


############################################ Product-42 ######################################

plot(p42)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p42_holtforecast1 <- HoltWinters(p42, seasonal="additive")

### 
p42_holtforecast2 <- HoltWinters(p42, seasonal="multiplicative")


### Prediction on test data
p42_holtpriceforecast1<-  forecast(p42_holtforecast1,h = 14)
plot(p42_holtpriceforecast1)

p42_holtpriceforecast2<- forecast(p42_holtforecast2,h=14)
plot(p42_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p42_hwTestMape <- regr.eval(Test$X42,p42_holtpriceforecast1$mean)
p42_hwTestMape

p42_hwTestMape2<-regr.eval(Test$X42,p42_holtpriceforecast2$mean)
p42_hwTestMape2

### ARIMA
###  Auto Arima
p42_MODEL_ARIMA <- auto.arima(p42, ic='aic')
p42_pricearimaforecasts_autArima<- forecast(p42_MODEL_ARIMA,h=14)
plot(p42_pricearimaforecasts_autArima,flwd = 2)
p42_autoarimaTestMape <- regr.eval(Test$X42,p42_pricearimaforecasts_autArima$mean)
p42_autoarimaTestMape



############################################ Product-43 ######################################

plot(p43)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p43_holtforecast1 <- HoltWinters(p43, seasonal="additive")

### 
p43_holtforecast2 <- HoltWinters(p43, seasonal="multiplicative")


### Prediction on test data
p43_holtpriceforecast1<-  forecast(p43_holtforecast1,h = 14)
plot(p43_holtpriceforecast1)

p43_holtpriceforecast2<- forecast(p43_holtforecast2,h=14)
plot(p43_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p43_hwTestMape <- regr.eval(Test$X43,p43_holtpriceforecast1$mean)
p43_hwTestMape

p43_hwTestMape2<-regr.eval(Test$X43,p43_holtpriceforecast2$mean)
p43_hwTestMape2

### ARIMA
###  Auto Arima
p43_MODEL_ARIMA <- auto.arima(p43, ic='aic')
p43_pricearimaforecasts_autArima<- forecast(p43_MODEL_ARIMA,h=14)
plot(p43_pricearimaforecasts_autArima,flwd = 2)
p43_autoarimaTestMape <- regr.eval(Test$X43,p43_pricearimaforecasts_autArima$mean)
p43_autoarimaTestMape


############################################ Product-44 ######################################

plot(p44)
## HoltWinters model  with trend  and Seasonality 
p44_holtforecast1 <- HoltWinters(p44, seasonal="additive")

### 
p44_holtforecast2 <- HoltWinters(p44, seasonal="multiplicative")


### Prediction on test data
p44_holtpriceforecast1<-  forecast(p44_holtforecast1,h = 14)
plot(p44_holtpriceforecast1)

p44_holtpriceforecast2<- forecast(p44_holtforecast2,h=14)
plot(p44_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p44_hwTestMape <- regr.eval(Test$X44,p44_holtpriceforecast1$mean)
p44_hwTestMape

p44_hwTestMape2<-regr.eval(Test$X44,p44_holtpriceforecast2$mean)
p44_hwTestMape2

### ARIMA(SELECTED)
###  Auto Arima
p44_MODEL_ARIMA <- auto.arima(p44, ic='aic')
p44_pricearimaforecasts_autArima<- forecast(p44_MODEL_ARIMA,h=14)
plot(p44_pricearimaforecasts_autArima,flwd = 2)
p44_autoarimaTestMape <- regr.eval(Test$X44,p44_pricearimaforecasts_autArima$mean)
p44_autoarimaTestMape



############################################ Product-45 ######################################

plot(p45)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p45_holtforecast1 <- HoltWinters(p45, seasonal="additive")

### 
p45_holtforecast2 <- HoltWinters(p45, seasonal="multiplicative")


### Prediction on test data
p45_holtpriceforecast1<-  forecast(p45_holtforecast1,h = 14)
plot(p45_holtpriceforecast1)

p45_holtpriceforecast2<- forecast(p45_holtforecast2,h=14)
plot(p45_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p45_hwTestMape <- regr.eval(Test$X45,p45_holtpriceforecast1$mean)
p45_hwTestMape

p45_hwTestMape2<-regr.eval(Test$X45,p45_holtpriceforecast2$mean)
p45_hwTestMape2

### ARIMA
###  Auto Arima
p45_MODEL_ARIMA <- auto.arima(p45, ic='aic')
p45_pricearimaforecasts_autArima<- forecast(p45_MODEL_ARIMA,h=14)
plot(p45_pricearimaforecasts_autArima,flwd = 2)
p45_autoarimaTestMape <- regr.eval(Test$X45,p45_pricearimaforecasts_autArima$mean)
p45_autoarimaTestMape



############################################ Product-46 ######################################

plot(p46)
## HoltWinters model  with trend  and Seasonality
p46_holtforecast1 <- HoltWinters(p46, seasonal="additive")

### 
p46_holtforecast2 <- HoltWinters(p46, seasonal="multiplicative")


### Prediction on test data
p46_holtpriceforecast1<-  forecast(p46_holtforecast1,h = 14)
plot(p46_holtpriceforecast1)

p46_holtpriceforecast2<- forecast(p46_holtforecast2,h=14)
plot(p46_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p46_hwTestMape <- regr.eval(Test$X46,p46_holtpriceforecast1$mean)
p46_hwTestMape

p46_hwTestMape2<-regr.eval(Test$X46,p46_holtpriceforecast2$mean)
p46_hwTestMape2

### ARIMA
###  Auto Arima(sELECTED)
p46_MODEL_ARIMA <- auto.arima(p46, ic='aic')
p46_pricearimaforecasts_autArima<- forecast(p46_MODEL_ARIMA,h=14)
plot(p46_pricearimaforecasts_autArima,flwd = 2)
p46_autoarimaTestMape <- regr.eval(Test$X46,p46_pricearimaforecasts_autArima$mean)
p46_autoarimaTestMape



############################################ Product-47 ######################################

plot(p47)
## HoltWinters model  with trend  and Seasonality
p47_holtforecast1 <- HoltWinters(p47, seasonal="additive")

### 
p47_holtforecast2 <- HoltWinters(p47, seasonal="multiplicative")


### Prediction on test data (SELECTED)
p47_holtpriceforecast1<-  forecast(p47_holtforecast1,h = 14)
plot(p47_holtpriceforecast1)

p47_holtpriceforecast2<- forecast(p47_holtforecast2,h=14)
plot(p47_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p47_hwTestMape <- regr.eval(Test$X47,p47_holtpriceforecast1$mean)
p47_hwTestMape

p47_hwTestMape2<-regr.eval(Test$X47,p47_holtpriceforecast2$mean)
p47_hwTestMape2

### ARIMA
###  Auto Arima
p47_MODEL_ARIMA <- auto.arima(p47, ic='aic')
p47_pricearimaforecasts_autArima<- forecast(p47_MODEL_ARIMA,h=14)
plot(p47_pricearimaforecasts_autArima,flwd = 2)
p47_autoarimaTestMape <- regr.eval(Test$X47,p47_pricearimaforecasts_autArima$mean)
p47_autoarimaTestMape



############################################ Product-48 ######################################

plot(p48)
## HoltWinters model  with trend  and Seasonality
p48_holtforecast1 <- HoltWinters(p48, seasonal="additive")

### 
p48_holtforecast2 <- HoltWinters(p48, seasonal="multiplicative")


### Prediction on test data (SELECTED)
p48_holtpriceforecast1<-  forecast(p48_holtforecast1,h = 14)
plot(p48_holtpriceforecast1)

p48_holtpriceforecast2<- forecast(p48_holtforecast2,h=14)
plot(p48_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p48_hwTestMape <- regr.eval(Test$X48,p48_holtpriceforecast1$mean)
p48_hwTestMape

p48_hwTestMape2<-regr.eval(Test$X48,p48_holtpriceforecast2$mean)
p48_hwTestMape2

### ARIMA
###  Auto Arima
p48_MODEL_ARIMA <- auto.arima(p48, ic='aic')
p48_pricearimaforecasts_autArima<- forecast(p48_MODEL_ARIMA,h=14)
plot(p48_pricearimaforecasts_autArima,flwd = 2)
p48_autoarimaTestMape <- regr.eval(Test$X48,p48_pricearimaforecasts_autArima$mean)
p48_autoarimaTestMape



############################################ Product-49 ######################################

plot(p49)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p49_holtforecast1 <- HoltWinters(p49, seasonal="additive")

### 
p49_holtforecast2 <- HoltWinters(p49, seasonal="multiplicative")


### Prediction on test data
p49_holtpriceforecast1<-  forecast(p49_holtforecast1,h = 14)
plot(p49_holtpriceforecast1)

p49_holtpriceforecast2<- forecast(p49_holtforecast2,h=14)
plot(p49_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p49_hwTestMape <- regr.eval(Test$X49,p49_holtpriceforecast1$mean)
p49_hwTestMape

p49_hwTestMape2<-regr.eval(Test$X49,p49_holtpriceforecast2$mean)
p49_hwTestMape2

### ARIMA
###  Auto Arima
p49_MODEL_ARIMA <- auto.arima(p49, ic='aic')
p49_pricearimaforecasts_autArima<- forecast(p49_MODEL_ARIMA,h=14)
plot(p49_pricearimaforecasts_autArima,flwd = 2)
p49_autoarimaTestMape <- regr.eval(Test$X49,p49_pricearimaforecasts_autArima$mean)
p49_autoarimaTestMape


############################################ Product-50 ######################################

plot(p50)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p50_holtforecast1 <- HoltWinters(p50, seasonal="additive")

### 
p50_holtforecast2 <- HoltWinters(p50, seasonal="multiplicative")


### Prediction on test data
p50_holtpriceforecast1<-  forecast(p50_holtforecast1,h = 14)
plot(p50_holtpriceforecast1)

p50_holtpriceforecast2<- forecast(p50_holtforecast2,h=14)
plot(p50_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p50_hwTestMape <- regr.eval(Test$X50,p50_holtpriceforecast1$mean)
p50_hwTestMape

p50_hwTestMape2<-regr.eval(Test$X50,p50_holtpriceforecast2$mean)
p50_hwTestMape2

### ARIMA
###  Auto Arima
p50_MODEL_ARIMA <- auto.arima(p50, ic='aic')
p50_pricearimaforecasts_autArima<- forecast(p50_MODEL_ARIMA,h=14)
plot(p50_pricearimaforecasts_autArima,flwd = 2)
p50_autoarimaTestMape <- regr.eval(Test$X50,p50_pricearimaforecasts_autArima$mean)
p50_autoarimaTestMape

############################################ Product-51 ######################################

plot(p51)
## HoltWinters model  with trend  and Seasonality 
p51_holtforecast1 <- HoltWinters(p51, seasonal="additive")

### 
p51_holtforecast2 <- HoltWinters(p51, seasonal="multiplicative")


### Prediction on test data
p51_holtpriceforecast1<-  forecast(p51_holtforecast1,h = 14)
plot(p51_holtpriceforecast1)

p51_holtpriceforecast2<- forecast(p51_holtforecast2,h=14)
plot(p51_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p51_hwTestMape <- regr.eval(Test$X51,p51_holtpriceforecast1$mean)
p51_hwTestMape

p51_hwTestMape2<-regr.eval(Test$X51,p51_holtpriceforecast2$mean)
p51_hwTestMape2

### ARIMA   (SELECTED)
###  Auto Arima
p51_MODEL_ARIMA <- auto.arima(p51, ic='aic')
p51_pricearimaforecasts_autArima<- forecast(p51_MODEL_ARIMA,h=14)
plot(p51_pricearimaforecasts_autArima,flwd = 2)
p51_autoarimaTestMape <- regr.eval(Test$X51,p51_pricearimaforecasts_autArima$mean)
p51_autoarimaTestMape

############################################ Product-52 ######################################

plot(p52)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p52_holtforecast1 <- HoltWinters(p52, seasonal="additive")

### 
p52_holtforecast2 <- HoltWinters(p52, seasonal="multiplicative")


### Prediction on test data
p52_holtpriceforecast1<-  forecast(p52_holtforecast1,h = 14)
plot(p52_holtpriceforecast1)

p52_holtpriceforecast2<- forecast(p52_holtforecast2,h=14)
plot(p52_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p52_hwTestMape <- regr.eval(Test$X52,p52_holtpriceforecast1$mean)
p52_hwTestMape

p52_hwTestMape2<-regr.eval(Test$X52,p52_holtpriceforecast2$mean)
p52_hwTestMape2

### ARIMA
###  Auto Arima
p52_MODEL_ARIMA <- auto.arima(p52, ic='aic')
p52_pricearimaforecasts_autArima<- forecast(p52_MODEL_ARIMA,h=14)
plot(p52_pricearimaforecasts_autArima,flwd = 2)
p52_autoarimaTestMape <- regr.eval(Test$X52,p52_pricearimaforecasts_autArima$mean)
p52_autoarimaTestMape

############################################ Product-53 ######################################

plot(p53)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p53_holtforecast1 <- HoltWinters(p53, seasonal="additive")

### 
p53_holtforecast2 <- HoltWinters(p53, seasonal="multiplicative")


### Prediction on test data
p53_holtpriceforecast1<-  forecast(p53_holtforecast1,h = 14)
plot(p53_holtpriceforecast1)

p53_holtpriceforecast2<- forecast(p53_holtforecast2,h=14)
plot(p53_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p53_hwTestMape <- regr.eval(Test$X53,p53_holtpriceforecast1$mean)
p53_hwTestMape

p53_hwTestMape2<-regr.eval(Test$X53,p53_holtpriceforecast2$mean)
p53_hwTestMape2

### ARIMA
###  Auto Arima
p53_MODEL_ARIMA <- auto.arima(p53, ic='aic')
p53_pricearimaforecasts_autArima<- forecast(p53_MODEL_ARIMA,h=14)
plot(p53_pricearimaforecasts_autArima,flwd = 2)
p53_autoarimaTestMape <- regr.eval(Test$X53,p53_pricearimaforecasts_autArima$mean)
p53_autoarimaTestMape

############################################ Product-54 ######################################

plot(p54)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p54_holtforecast1 <- HoltWinters(p54, seasonal="additive")

### 
p54_holtforecast2 <- HoltWinters(p54, seasonal="multiplicative")


### Prediction on test data
p54_holtpriceforecast1<-  forecast(p54_holtforecast1,h = 14)
plot(p54_holtpriceforecast1)

p54_holtpriceforecast2<- forecast(p54_holtforecast2,h=14)
plot(p54_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p54_hwTestMape <- regr.eval(Test$X54,p54_holtpriceforecast1$mean)
p54_hwTestMape

p54_hwTestMape2<-regr.eval(Test$X54,p54_holtpriceforecast2$mean)
p54_hwTestMape2

### ARIMA
###  Auto Arima
p54_MODEL_ARIMA <- auto.arima(p54, ic='aic')
p54_pricearimaforecasts_autArima<- forecast(p54_MODEL_ARIMA,h=14)
plot(p54_pricearimaforecasts_autArima,flwd = 2)
p54_autoarimaTestMape <- regr.eval(Test$X54,p54_pricearimaforecasts_autArima$mean)
p54_autoarimaTestMape

############################################ Product-55 ######################################

plot(p55)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p55_holtforecast1 <- HoltWinters(p55, seasonal="additive")

### 
p55_holtforecast2 <- HoltWinters(p55, seasonal="multiplicative")


### Prediction on test data
p55_holtpriceforecast1<-  forecast(p55_holtforecast1,h = 14)
plot(p55_holtpriceforecast1)

p55_holtpriceforecast2<- forecast(p55_holtforecast2,h=14)
plot(p55_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p55_hwTestMape <- regr.eval(Test$X55,p55_holtpriceforecast1$mean)
p55_hwTestMape

p55_hwTestMape2<-regr.eval(Test$X55,p55_holtpriceforecast2$mean)
p55_hwTestMape2

### ARIMA
###  Auto Arima
p55_MODEL_ARIMA <- auto.arima(p55, ic='aic')
p55_pricearimaforecasts_autArima<- forecast(p55_MODEL_ARIMA,h=14)
plot(p55_pricearimaforecasts_autArima,flwd = 2)
p55_autoarimaTestMape <- regr.eval(Test$X55,p55_pricearimaforecasts_autArima$mean)
p55_autoarimaTestMape

############################################ Product-56 ######################################

plot(p56)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p56_holtforecast1 <- HoltWinters(p56, seasonal="additive")

### 
p56_holtforecast2 <- HoltWinters(p56, seasonal="multiplicative")


### Prediction on test data
p56_holtpriceforecast1<-  forecast(p56_holtforecast1,h = 14)
plot(p56_holtpriceforecast1)

p56_holtpriceforecast2<- forecast(p56_holtforecast2,h=14)
plot(p56_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p56_hwTestMape <- regr.eval(Test$X56,p56_holtpriceforecast1$mean)
p56_hwTestMape

p56_hwTestMape2<-regr.eval(Test$X56,p56_holtpriceforecast2$mean)
p56_hwTestMape2

### ARIMA
###  Auto Arima
p56_MODEL_ARIMA <- auto.arima(p56, ic='aic')
p56_pricearimaforecasts_autArima<- forecast(p56_MODEL_ARIMA,h=14)
plot(p56_pricearimaforecasts_autArima,flwd = 2)
p56_autoarimaTestMape <- regr.eval(Test$X56,p56_pricearimaforecasts_autArima$mean)
p56_autoarimaTestMape

############################################ Product-57 ######################################

plot(p57)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p57_holtforecast1 <- HoltWinters(p57, seasonal="additive")

### 
p57_holtforecast2 <- HoltWinters(p57, seasonal="multiplicative")


### Prediction on test data
p57_holtpriceforecast1<-  forecast(p57_holtforecast1,h = 14)
plot(p57_holtpriceforecast1)

p57_holtpriceforecast2<- forecast(p57_holtforecast2,h=14)
plot(p57_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p57_hwTestMape <- regr.eval(Test$X57,p57_holtpriceforecast1$mean)
p57_hwTestMape

p57_hwTestMape2<-regr.eval(Test$X57,p57_holtpriceforecast2$mean)
p57_hwTestMape2

### ARIMA
###  Auto Arima
p57_MODEL_ARIMA <- auto.arima(p57, ic='aic')
p57_pricearimaforecasts_autArima<- forecast(p57_MODEL_ARIMA,h=14)
plot(p57_pricearimaforecasts_autArima,flwd = 2)
p57_autoarimaTestMape <- regr.eval(Test$X57,p57_pricearimaforecasts_autArima$mean)
p57_autoarimaTestMape

############################################ Product-58 ######################################

plot(p58)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p58_holtforecast1 <- HoltWinters(p58, seasonal="additive")

### 
p58_holtforecast2 <- HoltWinters(p58, seasonal="multiplicative")


### Prediction on test data
p58_holtpriceforecast1<-  forecast(p58_holtforecast1,h = 14)
plot(p58_holtpriceforecast1)

p58_holtpriceforecast2<- forecast(p58_holtforecast2,h=14)
plot(p58_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p58_hwTestMape <- regr.eval(Test$X58,p58_holtpriceforecast1$mean)
p58_hwTestMape

p58_hwTestMape2<-regr.eval(Test$X58,p58_holtpriceforecast2$mean)
p58_hwTestMape2

### ARIMA
###  Auto Arima
p58_MODEL_ARIMA <- auto.arima(p58, ic='aic')
p58_pricearimaforecasts_autArima<- forecast(p58_MODEL_ARIMA,h=14)
plot(p58_pricearimaforecasts_autArima,flwd = 2)
p58_autoarimaTestMape <- regr.eval(Test$X58,p58_pricearimaforecasts_autArima$mean)
p58_autoarimaTestMape

############################################ Product-59 ######################################

plot(p59)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p59_holtforecast1 <- HoltWinters(p59, seasonal="additive")

### 
p59_holtforecast2 <- HoltWinters(p59, seasonal="multiplicative")


### Prediction on test data
p59_holtpriceforecast1<-  forecast(p59_holtforecast1,h = 14)
plot(p59_holtpriceforecast1)

p59_holtpriceforecast2<- forecast(p59_holtforecast2,h=14)
plot(p59_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p59_hwTestMape <- regr.eval(Test$X59,p59_holtpriceforecast1$mean)
p59_hwTestMape

p59_hwTestMape2<-regr.eval(Test$X59,p59_holtpriceforecast2$mean)
p59_hwTestMape2

### ARIMA
###  Auto Arima
p59_MODEL_ARIMA <- auto.arima(p59, ic='aic')
p59_pricearimaforecasts_autArima<- forecast(p59_MODEL_ARIMA,h=14)
plot(p59_pricearimaforecasts_autArima,flwd = 2)
p59_autoarimaTestMape <- regr.eval(Test$X59,p59_pricearimaforecasts_autArima$mean)
p59_autoarimaTestMape

############################################ Product-60 ######################################

plot(p60)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p60_holtforecast1 <- HoltWinters(p60, seasonal="additive")

### 
p60_holtforecast2 <- HoltWinters(p60, seasonal="multiplicative")


### Prediction on test data
p60_holtpriceforecast1<-  forecast(p60_holtforecast1,h = 14)
plot(p60_holtpriceforecast1)

p60_holtpriceforecast2<- forecast(p60_holtforecast2,h=14)
plot(p60_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p60_hwTestMape <- regr.eval(Test$X60,p60_holtpriceforecast1$mean)
p60_hwTestMape

p60_hwTestMape2<-regr.eval(Test$X60,p60_holtpriceforecast2$mean)
p60_hwTestMape2

### ARIMA
###  Auto Arima
p60_MODEL_ARIMA <- auto.arima(p60, ic='aic')
p60_pricearimaforecasts_autArima<- forecast(p60_MODEL_ARIMA,h=14)
plot(p60_pricearimaforecasts_autArima,flwd = 2)
p60_autoarimaTestMape <- regr.eval(Test$X60,p60_pricearimaforecasts_autArima$mean)
p60_autoarimaTestMape

############################################ Product-61 ######################################

plot(p61)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p61_holtforecast1 <- HoltWinters(p61, seasonal="additive")

### 
p61_holtforecast2 <- HoltWinters(p61, seasonal="multiplicative")


### Prediction on test data
p61_holtpriceforecast1<-  forecast(p61_holtforecast1,h = 14)
plot(p61_holtpriceforecast1)

p61_holtpriceforecast2<- forecast(p61_holtforecast2,h=14)
plot(p61_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p61_hwTestMape <- regr.eval(Test$X61,p61_holtpriceforecast1$mean)
p61_hwTestMape

p61_hwTestMape2<-regr.eval(Test$X61,p61_holtpriceforecast2$mean)
p61_hwTestMape2

### ARIMA
###  Auto Arima
p61_MODEL_ARIMA <- auto.arima(p61, ic='aic')
p61_pricearimaforecasts_autArima<- forecast(p61_MODEL_ARIMA,h=14)
plot(p61_pricearimaforecasts_autArima,flwd = 2)
p61_autoarimaTestMape <- regr.eval(Test$X61,p61_pricearimaforecasts_autArima$mean)
p61_autoarimaTestMape


############################################ Product-62 ######################################

plot(p62)
## HoltWinters model  with trend  and Seasonality 
p62_holtforecast1 <- HoltWinters(p62, seasonal="additive")

### 
p62_holtforecast2 <- HoltWinters(p62, seasonal="multiplicative")


### Prediction on test data
p62_holtpriceforecast1<-  forecast(p62_holtforecast1,h = 14)
plot(p62_holtpriceforecast1)

p62_holtpriceforecast2<- forecast(p62_holtforecast2,h=14)
plot(p62_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p62_hwTestMape <- regr.eval(Test$X62,p62_holtpriceforecast1$mean)
p62_hwTestMape

p62_hwTestMape2<-regr.eval(Test$X62,p62_holtpriceforecast2$mean)
p62_hwTestMape2

### ARIMA   (SELECTED)
###  Auto Arima
p62_MODEL_ARIMA <- auto.arima(p62, ic='aic')
p62_pricearimaforecasts_autArima<- forecast(p62_MODEL_ARIMA,h=14)
plot(p62_pricearimaforecasts_autArima,flwd = 2)
p62_autoarimaTestMape <- regr.eval(Test$X62,p62_pricearimaforecasts_autArima$mean)
p62_autoarimaTestMape


############################################ Product-63 ######################################

plot(p63)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p63_holtforecast1 <- HoltWinters(p63, seasonal="additive")

### 
p63_holtforecast2 <- HoltWinters(p63, seasonal="multiplicative")


### Prediction on test data
p63_holtpriceforecast1<-  forecast(p63_holtforecast1,h = 14)
plot(p63_holtpriceforecast1)

p63_holtpriceforecast2<- forecast(p63_holtforecast2,h=14)
plot(p63_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p63_hwTestMape <- regr.eval(Test$X63,p63_holtpriceforecast1$mean)
p63_hwTestMape

p63_hwTestMape2<-regr.eval(Test$X63,p63_holtpriceforecast2$mean)
p63_hwTestMape2

### ARIMA
###  Auto Arima
p63_MODEL_ARIMA <- auto.arima(p63, ic='aic')
p63_pricearimaforecasts_autArima<- forecast(p63_MODEL_ARIMA,h=14)
plot(p63_pricearimaforecasts_autArima,flwd = 2)
p63_autoarimaTestMape <- regr.eval(Test$X63,p63_pricearimaforecasts_autArima$mean)
p63_autoarimaTestMape


############################################ Product-64 ######################################

plot(p64)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p64_holtforecast1 <- HoltWinters(p64, seasonal="additive")

### 
p64_holtforecast2 <- HoltWinters(p64, seasonal="multiplicative")


### Prediction on test data
p64_holtpriceforecast1<-  forecast(p64_holtforecast1,h = 14)
plot(p64_holtpriceforecast1)

p64_holtpriceforecast2<- forecast(p64_holtforecast2,h=14)
plot(p64_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p64_hwTestMape <- regr.eval(Test$X64,p64_holtpriceforecast1$mean)
p64_hwTestMape

p64_hwTestMape2<-regr.eval(Test$X64,p64_holtpriceforecast2$mean)
p64_hwTestMape2

### ARIMA
###  Auto Arima
p64_MODEL_ARIMA <- auto.arima(p64, ic='aic')
p64_pricearimaforecasts_autArima<- forecast(p64_MODEL_ARIMA,h=14)
plot(p64_pricearimaforecasts_autArima,flwd = 2)
p64_autoarimaTestMape <- regr.eval(Test$X64,p64_pricearimaforecasts_autArima$mean)
p64_autoarimaTestMape


############################################ Product-65 ######################################

plot(p65)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p65_holtforecast1 <- HoltWinters(p65, seasonal="additive")

### 
p65_holtforecast2 <- HoltWinters(p65, seasonal="multiplicative")


### Prediction on test data
p65_holtpriceforecast1<-  forecast(p65_holtforecast1,h = 14)
plot(p65_holtpriceforecast1)

p65_holtpriceforecast2<- forecast(p65_holtforecast2,h=14)
plot(p65_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p65_hwTestMape <- regr.eval(Test$X65,p65_holtpriceforecast1$mean)
p65_hwTestMape

p65_hwTestMape2<-regr.eval(Test$X65,p65_holtpriceforecast2$mean)
p65_hwTestMape2

### ARIMA
###  Auto Arima
p65_MODEL_ARIMA <- auto.arima(p65, ic='aic')
p65_pricearimaforecasts_autArima<- forecast(p65_MODEL_ARIMA,h=14)
plot(p65_pricearimaforecasts_autArima,flwd = 2)
p65_autoarimaTestMape <- regr.eval(Test$X65,p65_pricearimaforecasts_autArima$mean)
p65_autoarimaTestMape


############################################ Product-66 ######################################

plot(p66)
## HoltWinters model  with trend  and Seasonality
p66_holtforecast1 <- HoltWinters(p66, seasonal="additive")

### 
p66_holtforecast2 <- HoltWinters(p66, seasonal="multiplicative")


### Prediction on test data
p66_holtpriceforecast1<-  forecast(p66_holtforecast1,h = 14)
plot(p66_holtpriceforecast1)

p66_holtpriceforecast2<- forecast(p66_holtforecast2,h=14)
plot(p66_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p66_hwTestMape <- regr.eval(Test$X66,p66_holtpriceforecast1$mean)
p66_hwTestMape

p66_hwTestMape2<-regr.eval(Test$X66,p66_holtpriceforecast2$mean)
p66_hwTestMape2

### ARIMA
###  Auto Arima (SELECTED)
p66_MODEL_ARIMA <- auto.arima(p66, ic='aic')
p66_pricearimaforecasts_autArima<- forecast(p66_MODEL_ARIMA,h=14)
plot(p66_pricearimaforecasts_autArima,flwd = 2)
p66_autoarimaTestMape <- regr.eval(Test$X66,p60_pricearimaforecasts_autArima$mean)
p66_autoarimaTestMape


############################################ Product-67 ######################################

plot(p67)
## HoltWinters model  with trend  and Seasonality
p67_holtforecast1 <- HoltWinters(p67, seasonal="additive")

### 
p67_holtforecast2 <- HoltWinters(p67, seasonal="multiplicative")


### Prediction on test data
p67_holtpriceforecast1<-  forecast(p67_holtforecast1,h = 14)
plot(p67_holtpriceforecast1)

p67_holtpriceforecast2<- forecast(p67_holtforecast2,h=14)
plot(p67_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p67_hwTestMape <- regr.eval(Test$X67,p67_holtpriceforecast1$mean)
p67_hwTestMape

p67_hwTestMape2<-regr.eval(Test$X67,p67_holtpriceforecast2$mean)
p67_hwTestMape2

### ARIMA  (SELECTED)
###  Auto Arima
p67_MODEL_ARIMA <- auto.arima(p67, ic='aic')
p67_pricearimaforecasts_autArima<- forecast(p67_MODEL_ARIMA,h=14)
plot(p67_pricearimaforecasts_autArima,flwd = 2)
p67_autoarimaTestMape <- regr.eval(Test$X67,p67_pricearimaforecasts_autArima$mean)
p67_autoarimaTestMape


############################################ Product-68 ######################################

plot(p68)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p68_holtforecast1 <- HoltWinters(p68, seasonal="additive")

### 
p68_holtforecast2 <- HoltWinters(p68, seasonal="multiplicative")


### Prediction on test data
p68_holtpriceforecast1<-  forecast(p68_holtforecast1,h = 14)
plot(p68_holtpriceforecast1)

p68_holtpriceforecast2<- forecast(p68_holtforecast2,h=14)
plot(p68_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p68_hwTestMape <- regr.eval(Test$X68,p68_holtpriceforecast1$mean)
p68_hwTestMape

p68_hwTestMape2<-regr.eval(Test$X68,p68_holtpriceforecast2$mean)
p68_hwTestMape2

### ARIMA
###  Auto Arima
p68_MODEL_ARIMA <- auto.arima(p68, ic='aic')
p68_pricearimaforecasts_autArima<- forecast(p68_MODEL_ARIMA,h=14)
plot(p68_pricearimaforecasts_autArima,flwd = 2)
p68_autoarimaTestMape <- regr.eval(Test$X68,p68_pricearimaforecasts_autArima$mean)
p68_autoarimaTestMape


############################################ Product-69 ######################################

plot(p69)
## HoltWinters model  with trend  and Seasonality (SELECTED)
p69_holtforecast1 <- HoltWinters(p69, seasonal="additive")

### 
p69_holtforecast2 <- HoltWinters(p69, seasonal="multiplicative")


### Prediction on test data
p69_holtpriceforecast1<-  forecast(p69_holtforecast1,h = 14)
plot(p69_holtpriceforecast1)

p69_holtpriceforecast2<- forecast(p69_holtforecast2,h=14)
plot(p69_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p69_hwTestMape <- regr.eval(Test$X69,p69_holtpriceforecast1$mean)
p69_hwTestMape

p69_hwTestMape2<-regr.eval(Test$X69,p69_holtpriceforecast2$mean)
p69_hwTestMape2

### ARIMA
###  Auto Arima
p69_MODEL_ARIMA <- auto.arima(p69, ic='aic')
p69_pricearimaforecasts_autArima<- forecast(p69_MODEL_ARIMA,h=14)
plot(p69_pricearimaforecasts_autArima,flwd = 2)
p69_autoarimaTestMape <- regr.eval(Test$X69,p69_pricearimaforecasts_autArima$mean)
p69_autoarimaTestMape


############################################ Product-70 ######################################

plot(p70)
## HoltWinters model  with trend  and Seasonality
p70_holtforecast1 <- HoltWinters(p70, seasonal="additive")

### 
p70_holtforecast2 <- HoltWinters(p70, seasonal="multiplicative")


### Prediction on test data
p70_holtpriceforecast1<-  forecast(p70_holtforecast1,h = 14)
plot(p70_holtpriceforecast1)

p70_holtpriceforecast2<- forecast(p70_holtforecast2,h=14)
plot(p70_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p70_hwTestMape <- regr.eval(Test$X70,p70_holtpriceforecast1$mean)
p70_hwTestMape

p70_hwTestMape2<-regr.eval(Test$X70,p70_holtpriceforecast2$mean)
p70_hwTestMape2

### ARIMA   (SELECTED)
###  Auto Arima
p70_MODEL_ARIMA <- auto.arima(p70, ic='aic')
p70_pricearimaforecasts_autArima<- forecast(p70_MODEL_ARIMA,h=14)
plot(p70_pricearimaforecasts_autArima,flwd = 2)
p70_autoarimaTestMape <- regr.eval(Test$X70,p70_pricearimaforecasts_autArima$mean)
p70_autoarimaTestMape

############################################ Product-71 ######################################

plot(p71)
## HoltWinters model  with trend  and Seasonality
p71_holtforecast1 <- HoltWinters(p71, seasonal="additive")

### 
p71_holtforecast2 <- HoltWinters(p71, seasonal="multiplicative")


### Prediction on test data
p71_holtpriceforecast1<-  forecast(p71_holtforecast1,h = 14)
plot(p71_holtpriceforecast1)

p71_holtpriceforecast2<- forecast(p71_holtforecast2,h=14)
plot(p71_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p71_hwTestMape <- regr.eval(Test$X71,p71_holtpriceforecast1$mean)
p71_hwTestMape

p71_hwTestMape2<-regr.eval(Test$X71,p71_holtpriceforecast2$mean)
p71_hwTestMape2

### ARIMA 
###  Auto Arima
p71_MODEL_ARIMA <- auto.arima(p71, ic='aic')
p71_pricearimaforecasts_autArima<- forecast(p71_MODEL_ARIMA,h=14)
plot(p71_pricearimaforecasts_autArima,flwd = 2)
p71_autoarimaTestMape <- regr.eval(Test$X71,p71_pricearimaforecasts_autArima$mean)
p71_autoarimaTestMape

############################################ Product-72 ######################################

plot(p72)
## HoltWinters model  with trend  and Seasonality
p72_holtforecast1 <- HoltWinters(p72, seasonal="additive")

### 
p72_holtforecast2 <- HoltWinters(p72, seasonal="multiplicative")


### Prediction on test data
p72_holtpriceforecast1<-  forecast(p72_holtforecast1,h = 14)
plot(p72_holtpriceforecast1)

p72_holtpriceforecast2<- forecast(p72_holtforecast2,h=14)
plot(p72_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p72_hwTestMape <- regr.eval(Test$X72,p72_holtpriceforecast1$mean)
p72_hwTestMape

p72_hwTestMape2<-regr.eval(Test$X72,p72_holtpriceforecast2$mean)
p72_hwTestMape2

### ARIMA 
###  Auto Arima
p72_MODEL_ARIMA <- auto.arima(p72, ic='aic')
p72_pricearimaforecasts_autArima<- forecast(p72_MODEL_ARIMA,h=14)
plot(p72_pricearimaforecasts_autArima,flwd = 2)
p72_autoarimaTestMape <- regr.eval(Test$X72,p72_pricearimaforecasts_autArima$mean)
p72_autoarimaTestMape

############################################ Product-73 ######################################

plot(p73)
## HoltWinters model  with trend  and Seasonality
p73_holtforecast1 <- HoltWinters(p73, seasonal="additive")

### 
p73_holtforecast2 <- HoltWinters(p73, seasonal="multiplicative")


### Prediction on test data
p73_holtpriceforecast1<-  forecast(p73_holtforecast1,h = 14)
plot(p73_holtpriceforecast1)

p73_holtpriceforecast2<- forecast(p73_holtforecast2,h=14)
plot(p73_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p73_hwTestMape <- regr.eval(Test$X73,p73_holtpriceforecast1$mean)
p73_hwTestMape

p73_hwTestMape2<-regr.eval(Test$X73,p73_holtpriceforecast2$mean)
p73_hwTestMape2

### ARIMA
###  Auto Arima
p73_MODEL_ARIMA <- auto.arima(p73, ic='aic')
p73_pricearimaforecasts_autArima<- forecast(p73_MODEL_ARIMA,h=14)
plot(p73_pricearimaforecasts_autArima,flwd = 2)
p73_autoarimaTestMape <- regr.eval(Test$X73,p73_pricearimaforecasts_autArima$mean)
p73_autoarimaTestMape

############################################ Product-74 ######################################

plot(p74)
## HoltWinters model  with trend  and Seasonality
p74_holtforecast1 <- HoltWinters(p74, seasonal="additive")

### 
p74_holtforecast2 <- HoltWinters(p74, seasonal="multiplicative")


### Prediction on test data
p74_holtpriceforecast1<-  forecast(p74_holtforecast1,h = 14)
plot(p74_holtpriceforecast1)

p74_holtpriceforecast2<- forecast(p74_holtforecast2,h=14)
plot(p74_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p74_hwTestMape <- regr.eval(Test$X74,p74_holtpriceforecast1$mean)
p74_hwTestMape

p74_hwTestMape2<-regr.eval(Test$X74,p74_holtpriceforecast2$mean)
p74_hwTestMape2

### ARIMA
###  Auto Arima
p74_MODEL_ARIMA <- auto.arima(p74, ic='aic')
p74_pricearimaforecasts_autArima<- forecast(p74_MODEL_ARIMA,h=14)
plot(p74_pricearimaforecasts_autArima,flwd = 2)
p74_autoarimaTestMape <- regr.eval(Test$X74,p74_pricearimaforecasts_autArima$mean)
p74_autoarimaTestMape

############################################ Product-75 ######################################

plot(p75)
## HoltWinters model  with trend  and Seasonality
p75_holtforecast1 <- HoltWinters(p75, seasonal="additive")

### 
p75_holtforecast2 <- HoltWinters(p75, seasonal="multiplicative")


### Prediction on test data
p75_holtpriceforecast1<-  forecast(p75_holtforecast1,h = 14)
plot(p75_holtpriceforecast1)

p75_holtpriceforecast2<- forecast(p75_holtforecast2,h=14)
plot(p75_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p75_hwTestMape <- regr.eval(Test$X75,p75_holtpriceforecast1$mean)
p75_hwTestMape

p75_hwTestMape2<-regr.eval(Test$X75,p75_holtpriceforecast2$mean)
p75_hwTestMape2

### ARIMA 
###  Auto Arima
p75_MODEL_ARIMA <- auto.arima(p75, ic='aic')
p75_pricearimaforecasts_autArima<- forecast(p75_MODEL_ARIMA,h=14)
plot(p75_pricearimaforecasts_autArima,flwd = 2)
p75_autoarimaTestMape <- regr.eval(Test$X75,p75_pricearimaforecasts_autArima$mean)
p75_autoarimaTestMape

############################################ Product-76 ######################################

plot(p76)
## HoltWinters model  with trend  and Seasonality
p76_holtforecast1 <- HoltWinters(p76, seasonal="additive")

### 
p76_holtforecast2 <- HoltWinters(p76, seasonal="multiplicative")


### Prediction on test data
p76_holtpriceforecast1<-  forecast(p76_holtforecast1,h = 14)
plot(p76_holtpriceforecast1)

p76_holtpriceforecast2<- forecast(p76_holtforecast2,h=14)
plot(p76_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p76_hwTestMape <- regr.eval(Test$X76,p76_holtpriceforecast1$mean)
p76_hwTestMape

p76_hwTestMape2<-regr.eval(Test$X76,p76_holtpriceforecast2$mean)
p76_hwTestMape2

### ARIMA
###  Auto Arima
p76_MODEL_ARIMA <- auto.arima(p76, ic='aic')
p76_pricearimaforecasts_autArima<- forecast(p76_MODEL_ARIMA,h=14)
plot(p76_pricearimaforecasts_autArima,flwd = 2)
p76_autoarimaTestMape <- regr.eval(Test$X76,p76_pricearimaforecasts_autArima$mean)
p76_autoarimaTestMape

############################################ Product-77 ######################################

plot(p77)
## HoltWinters model  with trend  and Seasonality
p77_holtforecast1 <- HoltWinters(p77, seasonal="additive")

### 
p77_holtforecast2 <- HoltWinters(p77, seasonal="multiplicative")


### Prediction on test data
p77_holtpriceforecast1<-  forecast(p77_holtforecast1,h = 14)
plot(p77_holtpriceforecast1)

p77_holtpriceforecast2<- forecast(p77_holtforecast2,h=14)
plot(p77_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p77_hwTestMape <- regr.eval(Test$X77,p77_holtpriceforecast1$mean)
p77_hwTestMape

p77_hwTestMape2<-regr.eval(Test$X77,p77_holtpriceforecast2$mean)
p77_hwTestMape2

### ARIMA
###  Auto Arima
p77_MODEL_ARIMA <- auto.arima(p77, ic='aic')
p77_pricearimaforecasts_autArima<- forecast(p77_MODEL_ARIMA,h=14)
plot(p77_pricearimaforecasts_autArima,flwd = 2)
p77_autoarimaTestMape <- regr.eval(Test$X77,p77_pricearimaforecasts_autArima$mean)
p77_autoarimaTestMape

############################################ Product-78 ######################################

plot(p78)
## HoltWinters model  with trend  and Seasonality
p78_holtforecast1 <- HoltWinters(p78, seasonal="additive")

### 
p78_holtforecast2 <- HoltWinters(p78, seasonal="multiplicative")


### Prediction on test data
p78_holtpriceforecast1<-  forecast(p78_holtforecast1,h = 14)
plot(p78_holtpriceforecast1)

p78_holtpriceforecast2<- forecast(p78_holtforecast2,h=14)
plot(p78_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p78_hwTestMape <- regr.eval(Test$X78,p78_holtpriceforecast1$mean)
p78_hwTestMape

p78_hwTestMape2<-regr.eval(Test$X78,p78_holtpriceforecast2$mean)
p78_hwTestMape2

### ARIMA (SELECTED)
###  Auto Arima
p78_MODEL_ARIMA <- auto.arima(p78, ic='aic')
p78_pricearimaforecasts_autArima<- forecast(p78_MODEL_ARIMA,h=14)
plot(p78_pricearimaforecasts_autArima,flwd = 2)
p78_autoarimaTestMape <- regr.eval(Test$X78,p78_pricearimaforecasts_autArima$mean)
p78_autoarimaTestMape

############################################ Product-79 ######################################

plot(p79)
## HoltWinters model  with trend  and Seasonality
p79_holtforecast1 <- HoltWinters(p79, seasonal="additive")

### 
p79_holtforecast2 <- HoltWinters(p79, seasonal="multiplicative")


### Prediction on test data
p79_holtpriceforecast1<-  forecast(p79_holtforecast1,h = 14)
plot(p79_holtpriceforecast1)

p79_holtpriceforecast2<- forecast(p79_holtforecast2,h=14)
plot(p79_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p79_hwTestMape <- regr.eval(Test$X79,p79_holtpriceforecast1$mean)
p79_hwTestMape

p79_hwTestMape2<-regr.eval(Test$X79,p79_holtpriceforecast2$mean)
p79_hwTestMape2

### ARIMA 
###  Auto Arima
p79_MODEL_ARIMA <- auto.arima(p79, ic='aic')
p79_pricearimaforecasts_autArima<- forecast(p79_MODEL_ARIMA,h=14)
plot(p79_pricearimaforecasts_autArima,flwd = 2)
p79_autoarimaTestMape <- regr.eval(Test$X79,p79_pricearimaforecasts_autArima$mean)
p79_autoarimaTestMape

############################################ Product-80 ######################################

plot(p80)
## HoltWinters model  with trend  and Seasonality
p80_holtforecast1 <- HoltWinters(p80, seasonal="additive")

### 
p80_holtforecast2 <- HoltWinters(p80, seasonal="multiplicative")


### Prediction on test data
p80_holtpriceforecast1<-  forecast(p80_holtforecast1,h = 14)
plot(p80_holtpriceforecast1)

p80_holtpriceforecast2<- forecast(p80_holtforecast2,h=14)
plot(p80_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p80_hwTestMape <- regr.eval(Test$X80,p80_holtpriceforecast1$mean)
p80_hwTestMape

p80_hwTestMape2<-regr.eval(Test$X80,p80_holtpriceforecast2$mean)
p80_hwTestMape2

### ARIMA 
###  Auto Arima
p80_MODEL_ARIMA <- auto.arima(p80, ic='aic')
p80_pricearimaforecasts_autArima<- forecast(p80_MODEL_ARIMA,h=14)
plot(p80_pricearimaforecasts_autArima,flwd = 2)
p80_autoarimaTestMape <- regr.eval(Test$X80,p80_pricearimaforecasts_autArima$mean)
p80_autoarimaTestMape

############################################ Product-81 ######################################

plot(p81)
## HoltWinters model  with trend  and Seasonality
p81_holtforecast1 <- HoltWinters(p81, seasonal="additive")

### 
p81_holtforecast2 <- HoltWinters(p81, seasonal="multiplicative")


### Prediction on test data
p81_holtpriceforecast1<-  forecast(p81_holtforecast1,h = 14)
plot(p81_holtpriceforecast1)

p81_holtpriceforecast2<- forecast(p81_holtforecast2,h=14)
plot(p81_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p81_hwTestMape <- regr.eval(Test$X81,p81_holtpriceforecast1$mean)
p81_hwTestMape

p81_hwTestMape2<-regr.eval(Test$X81,p81_holtpriceforecast2$mean)
p81_hwTestMape2

### ARIMA 
###  Auto Arima
p81_MODEL_ARIMA <- auto.arima(p81, ic='aic')
p81_pricearimaforecasts_autArima<- forecast(p81_MODEL_ARIMA,h=14)
plot(p81_pricearimaforecasts_autArima,flwd = 2)
p81_autoarimaTestMape <- regr.eval(Test$X81,p81_pricearimaforecasts_autArima$mean)
p81_autoarimaTestMape

############################################ Product-82 ######################################

plot(p82)
## HoltWinters model  with trend  and Seasonality
p82_holtforecast1 <- HoltWinters(p82, seasonal="additive")

### 
p82_holtforecast2 <- HoltWinters(p82, seasonal="multiplicative")


### Prediction on test data
p82_holtpriceforecast1<-  forecast(p82_holtforecast1,h = 14)
plot(p82_holtpriceforecast1)

p82_holtpriceforecast2<- forecast(p82_holtforecast2,h=14)
plot(p82_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p82_hwTestMape <- regr.eval(Test$X82,p82_holtpriceforecast1$mean)
p82_hwTestMape

p82_hwTestMape2<-regr.eval(Test$X82,p82_holtpriceforecast2$mean)
p82_hwTestMape2

### ARIMA 
###  Auto Arima
p82_MODEL_ARIMA <- auto.arima(p82, ic='aic')
p82_pricearimaforecasts_autArima<- forecast(p82_MODEL_ARIMA,h=14)
plot(p82_pricearimaforecasts_autArima,flwd = 2)
p82_autoarimaTestMape <- regr.eval(Test$X82,p82_pricearimaforecasts_autArima$mean)
p82_autoarimaTestMape

############################################ Product-83 ######################################

plot(p83)
## HoltWinters model  with trend  and Seasonality
p83_holtforecast1 <- HoltWinters(p83, seasonal="additive")

### 
p83_holtforecast2 <- HoltWinters(p83, seasonal="multiplicative")


### Prediction on test data
p83_holtpriceforecast1<-  forecast(p83_holtforecast1,h = 14)
plot(p83_holtpriceforecast1)

p83_holtpriceforecast2<- forecast(p83_holtforecast2,h=14)
plot(p83_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p83_hwTestMape <- regr.eval(Test$X83,p83_holtpriceforecast1$mean)
p83_hwTestMape

p83_hwTestMape2<-regr.eval(Test$X83,p83_holtpriceforecast2$mean)
p83_hwTestMape2

### ARIMA 
###  Auto Arima
p83_MODEL_ARIMA <- auto.arima(p83, ic='aic')
p83_pricearimaforecasts_autArima<- forecast(p83_MODEL_ARIMA,h=14)
plot(p83_pricearimaforecasts_autArima,flwd = 2)
p83_autoarimaTestMape <- regr.eval(Test$X83,p83_pricearimaforecasts_autArima$mean)
p83_autoarimaTestMape

############################################ Product-84 ######################################

plot(p84)
## HoltWinters model  with trend  and Seasonality
p84_holtforecast1 <- HoltWinters(p84, seasonal="additive")

### 
p84_holtforecast2 <- HoltWinters(p84, seasonal="multiplicative")


### Prediction on test data
p84_holtpriceforecast1<-  forecast(p84_holtforecast1,h = 14)
plot(p84_holtpriceforecast1)

p84_holtpriceforecast2<- forecast(p84_holtforecast2,h=14)
plot(p84_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p84_hwTestMape <- regr.eval(Test$X84,p84_holtpriceforecast1$mean)
p84_hwTestMape

p84_hwTestMape2<-regr.eval(Test$X84,p84_holtpriceforecast2$mean)
p84_hwTestMape2

### ARIMA 
###  Auto Arima
p84_MODEL_ARIMA <- auto.arima(p84, ic='aic')
p84_pricearimaforecasts_autArima<- forecast(p84_MODEL_ARIMA,h=14)
plot(p84_pricearimaforecasts_autArima,flwd = 2)
p84_autoarimaTestMape <- regr.eval(Test$X84,p84_pricearimaforecasts_autArima$mean)
p84_autoarimaTestMape

############################################ Product-85 ######################################

plot(p85)
## HoltWinters model  with trend  and Seasonality
p85_holtforecast1 <- HoltWinters(p85, seasonal="additive")

### 
p85_holtforecast2 <- HoltWinters(p85, seasonal="multiplicative")


### Prediction on test data
p85_holtpriceforecast1<-  forecast(p85_holtforecast1,h = 14)
plot(p85_holtpriceforecast1)

p85_holtpriceforecast2<- forecast(p85_holtforecast2,h=14)
plot(p85_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p85_hwTestMape <- regr.eval(Test$X85,p85_holtpriceforecast1$mean)
p85_hwTestMape

p85_hwTestMape2<-regr.eval(Test$X85,p85_holtpriceforecast2$mean)
p85_hwTestMape2

### ARIMA 
###  Auto Arima (SELECTED)
p85_MODEL_ARIMA <- auto.arima(p85, ic='aic')
p85_pricearimaforecasts_autArima<- forecast(p85_MODEL_ARIMA,h=14)
plot(p85_pricearimaforecasts_autArima,flwd = 2)
p85_autoarimaTestMape <- regr.eval(Test$X85,p85_pricearimaforecasts_autArima$mean)
p85_autoarimaTestMape

############################################ Product-86 ######################################

plot(p86)
## HoltWinters model  with trend  and Seasonality
p86_holtforecast1 <- HoltWinters(p86, seasonal="additive")

### 
p86_holtforecast2 <- HoltWinters(p86, seasonal="multiplicative")


### Prediction on test data
p86_holtpriceforecast1<-  forecast(p86_holtforecast1,h = 14)
plot(p86_holtpriceforecast1)

p86_holtpriceforecast2<- forecast(p86_holtforecast2,h=14)
plot(p86_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p86_hwTestMape <- regr.eval(Test$X86,p86_holtpriceforecast1$mean)
p86_hwTestMape

p86_hwTestMape2<-regr.eval(Test$X86,p86_holtpriceforecast2$mean)
p86_hwTestMape2

### ARIMA 
###  Auto Arima
p86_MODEL_ARIMA <- auto.arima(p86, ic='aic')
p86_pricearimaforecasts_autArima<- forecast(p86_MODEL_ARIMA,h=14)
plot(p86_pricearimaforecasts_autArima,flwd = 2)
p86_autoarimaTestMape <- regr.eval(Test$X86,p86_pricearimaforecasts_autArima$mean)
p86_autoarimaTestMape

############################################ Product-87 ######################################

plot(p87)
## HoltWinters model  with trend  and Seasonality
p87_holtforecast1 <- HoltWinters(p87, seasonal="additive")

### 
p87_holtforecast2 <- HoltWinters(p87, seasonal="multiplicative")


### Prediction on test data
p87_holtpriceforecast1<-  forecast(p87_holtforecast1,h = 14)
plot(p87_holtpriceforecast1)

p87_holtpriceforecast2<- forecast(p87_holtforecast2,h=14)
plot(p87_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p87_hwTestMape <- regr.eval(Test$X87,p87_holtpriceforecast1$mean)
p87_hwTestMape

p87_hwTestMape2<-regr.eval(Test$X87,p87_holtpriceforecast2$mean)
p87_hwTestMape2

### ARIMA (SELECTED)
###  Auto Arima
p87_MODEL_ARIMA <- auto.arima(p87, ic='aic')
p87_pricearimaforecasts_autArima<- forecast(p87_MODEL_ARIMA,h=14)
plot(p87_pricearimaforecasts_autArima,flwd = 2)
p87_autoarimaTestMape <- regr.eval(Test$X87,p87_pricearimaforecasts_autArima$mean)
p87_autoarimaTestMape

############################################ Product-88 ######################################

plot(p88)
## HoltWinters model  with trend  and Seasonality
p88_holtforecast1 <- HoltWinters(p88, seasonal="additive")

### 
p88_holtforecast2 <- HoltWinters(p88, seasonal="multiplicative")


### Prediction on test data
p88_holtpriceforecast1<-  forecast(p88_holtforecast1,h = 14)
plot(p88_holtpriceforecast1)

p88_holtpriceforecast2<- forecast(p88_holtforecast2,h=14)
plot(p88_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p88_hwTestMape <- regr.eval(Test$X88,p88_holtpriceforecast1$mean)
p88_hwTestMape

p88_hwTestMape2<-regr.eval(Test$X88,p88_holtpriceforecast2$mean)
p88_hwTestMape2

### ARIMA (SELECTED)
###  Auto Arima
p88_MODEL_ARIMA <- auto.arima(p88, ic='aic')
p88_pricearimaforecasts_autArima<- forecast(p88_MODEL_ARIMA,h=14)
plot(p88_pricearimaforecasts_autArima,flwd = 2)
p88_autoarimaTestMape <- regr.eval(Test$X88,p88_pricearimaforecasts_autArima$mean)
p88_autoarimaTestMape

############################################ Product-89 ######################################

plot(p89)
## HoltWinters model  with trend  and Seasonality
p89_holtforecast1 <- HoltWinters(p89, seasonal="additive")

### 
p89_holtforecast2 <- HoltWinters(p89, seasonal="multiplicative")


### Prediction on test data
p89_holtpriceforecast1<-  forecast(p89_holtforecast1,h = 14)
plot(p89_holtpriceforecast1)

p89_holtpriceforecast2<- forecast(p89_holtforecast2,h=14)
plot(p89_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p89_hwTestMape <- regr.eval(Test$X89,p89_holtpriceforecast1$mean)
p89_hwTestMape

p89_hwTestMape2<-regr.eval(Test$X89,p89_holtpriceforecast2$mean)
p89_hwTestMape2

### ARIMA 
###  Auto Arima
p89_MODEL_ARIMA <- auto.arima(p89, ic='aic')
p89_pricearimaforecasts_autArima<- forecast(p89_MODEL_ARIMA,h=14)
plot(p89_pricearimaforecasts_autArima,flwd = 2)
p89_autoarimaTestMape <- regr.eval(Test$X89,p89_pricearimaforecasts_autArima$mean)
p89_autoarimaTestMape

############################################ Product-90 ######################################

plot(p90)
## HoltWinters model  with trend  and Seasonality
p90_holtforecast1 <- HoltWinters(p90, seasonal="additive")

### 
p90_holtforecast2 <- HoltWinters(p90, seasonal="multiplicative")


### Prediction on test data
p90_holtpriceforecast1<-  forecast(p90_holtforecast1,h = 14)
plot(p90_holtpriceforecast1)

p90_holtpriceforecast2<- forecast(p90_holtforecast2,h=14)
plot(p90_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p90_hwTestMape <- regr.eval(Test$X90,p90_holtpriceforecast1$mean)
p90_hwTestMape

p90_hwTestMape2<-regr.eval(Test$X90,p90_holtpriceforecast2$mean)
p90_hwTestMape2

### ARIMA 
###  Auto Arima
p90_MODEL_ARIMA <- auto.arima(p90, ic='aic')
p90_pricearimaforecasts_autArima<- forecast(p90_MODEL_ARIMA,h=14)
plot(p90_pricearimaforecasts_autArima,flwd = 2)
p90_autoarimaTestMape <- regr.eval(Test$X90,p90_pricearimaforecasts_autArima$mean)
p90_autoarimaTestMape


############################################ Product-91 ######################################

plot(p91)
## HoltWinters model  with trend  and Seasonality
p91_holtforecast1 <- HoltWinters(p91, seasonal="additive")

### 
p91_holtforecast2 <- HoltWinters(p91, seasonal="multiplicative")


### Prediction on test data
p91_holtpriceforecast1<-  forecast(p91_holtforecast1,h = 14)
plot(p91_holtpriceforecast1)

p91_holtpriceforecast2<- forecast(p91_holtforecast2,h=14)
plot(p91_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p91_hwTestMape <- regr.eval(Test$X91,p91_holtpriceforecast1$mean)
p91_hwTestMape

p91_hwTestMape2<-regr.eval(Test$X91,p91_holtpriceforecast2$mean)
p91_hwTestMape2

### ARIMA 
###  Auto Arima
p91_MODEL_ARIMA <- auto.arima(p90, ic='aic')
p91_pricearimaforecasts_autArima<- forecast(p91_MODEL_ARIMA,h=14)
plot(p91_pricearimaforecasts_autArima,flwd = 2)
p91_autoarimaTestMape <- regr.eval(Test$X91,p91_pricearimaforecasts_autArima$mean)
p91_autoarimaTestMape

############################################ Product-92 ######################################

plot(p92)
## HoltWinters model  with trend  and Seasonality
p92_holtforecast1 <- HoltWinters(p92, seasonal="additive")

### 
p92_holtforecast2 <- HoltWinters(p92, seasonal="multiplicative")


### Prediction on test data
p92_holtpriceforecast1<-  forecast(p92_holtforecast1,h = 14)
plot(p92_holtpriceforecast1)

p92_holtpriceforecast2<- forecast(p92_holtforecast2,h=14)
plot(p92_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p92_hwTestMape <- regr.eval(Test$X92,p92_holtpriceforecast1$mean)
p92_hwTestMape

p92_hwTestMape2<-regr.eval(Test$X92,p92_holtpriceforecast2$mean)
p92_hwTestMape2

### ARIMA 
###  Auto Arima
p92_MODEL_ARIMA <- auto.arima(p92, ic='aic')
p92_pricearimaforecasts_autArima<- forecast(p92_MODEL_ARIMA,h=14)
plot(p92_pricearimaforecasts_autArima,flwd = 2)
p92_autoarimaTestMape <- regr.eval(Test$X92,p92_pricearimaforecasts_autArima$mean)
p92_autoarimaTestMape

############################################ Product-93 ######################################

plot(p93)
## HoltWinters model  with trend  and Seasonality
p93_holtforecast1 <- HoltWinters(p93, seasonal="additive")

### 
p93_holtforecast2 <- HoltWinters(p93, seasonal="multiplicative")


### Prediction on test data
p93_holtpriceforecast1<-  forecast(p93_holtforecast1,h = 14)
plot(p93_holtpriceforecast1)

p93_holtpriceforecast2<- forecast(p93_holtforecast2,h=14)
plot(p93_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p93_hwTestMape <- regr.eval(Test$X93,p93_holtpriceforecast1$mean)
p93_hwTestMape

p93_hwTestMape2<-regr.eval(Test$X93,p93_holtpriceforecast2$mean)


p93_hwTestMape2

### ARIMA 
###  Auto Arima
p93_MODEL_ARIMA <- auto.arima(p93, ic='aic')
p93_pricearimaforecasts_autArima<- forecast(p93_MODEL_ARIMA,h=14)
plot(p93_pricearimaforecasts_autArima,flwd = 2)
p93_autoarimaTestMape <- regr.eval(Test$X93,p93_pricearimaforecasts_autArima$mean)
p93_autoarimaTestMape

############################################ Product-94 ######################################

plot(p94)
## HoltWinters model  with trend  and Seasonality
p94_holtforecast1 <- HoltWinters(p94, seasonal="additive")

### 
p94_holtforecast2 <- HoltWinters(p94, seasonal="multiplicative")


### Prediction on test data
p94_holtpriceforecast1<-  forecast(p94_holtforecast1,h = 14)
plot(p94_holtpriceforecast1)

p94_holtpriceforecast2<- forecast(p94_holtforecast2,h=14)
plot(p94_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p94_hwTestMape <- regr.eval(Test$X94,p94_holtpriceforecast1$mean)


p94_hwTestMape

p94_hwTestMape2<-regr.eval(Test$X94,p94_holtpriceforecast2$mean)
p94_hwTestMape2

### ARIMA 
###  Auto Arima
p94_MODEL_ARIMA <- auto.arima(p94, ic='aic')
p94_pricearimaforecasts_autArima<- forecast(p94_MODEL_ARIMA,h=14)
plot(p94_pricearimaforecasts_autArima,flwd = 2)
p94_autoarimaTestMape <- regr.eval(Test$X94,p94_pricearimaforecasts_autArima$mean)
p94_autoarimaTestMape

############################################ Product-95 ######################################

plot(p95)
## HoltWinters model  with trend  and Seasonality
p95_holtforecast1 <- HoltWinters(p95, seasonal="additive")

### 
p95_holtforecast2 <- HoltWinters(p95, seasonal="multiplicative")


### Prediction on test data
p95_holtpriceforecast1<-  forecast(p95_holtforecast1,h = 14)
plot(p95_holtpriceforecast1)

p95_holtpriceforecast2<- forecast(p95_holtforecast2,h=14)
plot(p95_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p95_hwTestMape <- regr.eval(Test$X95,p95_holtpriceforecast1$mean)
p95_hwTestMape

p95_hwTestMape2<-regr.eval(Test$X95,p95_holtpriceforecast2$mean)

p95_hwTestMape2

### ARIMA 
###  Auto Arima
p95_MODEL_ARIMA <- auto.arima(p95, ic='aic')
p95_pricearimaforecasts_autArima<- forecast(p95_MODEL_ARIMA,h=14)
plot(p95_pricearimaforecasts_autArima,flwd = 2)
p95_autoarimaTestMape <- regr.eval(Test$X95,p95_pricearimaforecasts_autArima$mean)
p95_autoarimaTestMape

############################################ Product-96 ######################################

plot(p96)
## HoltWinters model  with trend  and Seasonality
p96_holtforecast1 <- HoltWinters(p96, seasonal="additive")

### 
p96_holtforecast2 <- HoltWinters(p96, seasonal="multiplicative")


### Prediction on test data
p96_holtpriceforecast1<-  forecast(p96_holtforecast1,h = 14)
plot(p96_holtpriceforecast1)

p96_holtpriceforecast2<- forecast(p96_holtforecast2,h=14)
plot(p96_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p96_hwTestMape <- regr.eval(Test$X96,p96_holtpriceforecast1$mean)
p96_hwTestMape

p96_hwTestMape2<-regr.eval(Test$X96,p96_holtpriceforecast2$mean)
p96_hwTestMape2

### ARIMA 
###  Auto Arima
p96_MODEL_ARIMA <- auto.arima(p96, ic='aic')
p96_pricearimaforecasts_autArima<- forecast(p96_MODEL_ARIMA,h=14)
plot(p96_pricearimaforecasts_autArima,flwd = 2)
p96_autoarimaTestMape <- regr.eval(Test$X96,p96_pricearimaforecasts_autArima$mean)
p96_autoarimaTestMape

############################################ Product-97 ######################################

plot(p97)
## HoltWinters model  with trend  and Seasonality
p97_holtforecast1 <- HoltWinters(p97, seasonal="additive")

### 
p97_holtforecast2 <- HoltWinters(p97, seasonal="multiplicative")


### Prediction on test data
p97_holtpriceforecast1<-  forecast(p97_holtforecast1,h = 14)
plot(p97_holtpriceforecast1)

p97_holtpriceforecast2<- forecast(p97_holtforecast2,h=14)
plot(p97_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p97_hwTestMape <- regr.eval(Test$X97,p97_holtpriceforecast1$mean)
p97_hwTestMape

p97_hwTestMape2<-regr.eval(Test$X97,p97_holtpriceforecast2$mean)
p97_hwTestMape2

### ARIMA 
###  Auto Arima
p97_MODEL_ARIMA <- auto.arima(p97, ic='aic')
p97_pricearimaforecasts_autArima<- forecast(p97_MODEL_ARIMA,h=14)
plot(p97_pricearimaforecasts_autArima,flwd = 2)
p97_autoarimaTestMape <- regr.eval(Test$X97,p97_pricearimaforecasts_autArima$mean)
p97_autoarimaTestMape

############################################ Product-98 ######################################

plot(p98)
## HoltWinters model  with trend  and Seasonality
p98_holtforecast1 <- HoltWinters(p98, seasonal="additive")

### 
p98_holtforecast2 <- HoltWinters(p98, seasonal="multiplicative")


### Prediction on test data
p98_holtpriceforecast1<-  forecast(p98_holtforecast1,h = 14)
plot(p98_holtpriceforecast1)

p98_holtpriceforecast2<- forecast(p98_holtforecast2,h=14)
plot(p98_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p98_hwTestMape <- regr.eval(Test$X98,p98_holtpriceforecast1$mean)
p98_hwTestMape

p98_hwTestMape2<-regr.eval(Test$X98,p98_holtpriceforecast2$mean)
p98_hwTestMape2

### ARIMA 
###  Auto Arima
p98_MODEL_ARIMA <- auto.arima(p98, ic='aic')
p98_pricearimaforecasts_autArima<- forecast(p98_MODEL_ARIMA,h=14)
plot(p98_pricearimaforecasts_autArima,flwd = 2)
p98_autoarimaTestMape <- regr.eval(Test$X98,p98_pricearimaforecasts_autArima$mean)
p98_autoarimaTestMape

############################################ Product-99 ######################################

plot(p99)
## HoltWinters model  with trend  and Seasonality
p99_holtforecast1 <- HoltWinters(p99, seasonal="additive")

### 
p99_holtforecast2 <- HoltWinters(p99, seasonal="multiplicative")


### Prediction on test data
p99_holtpriceforecast1<-  forecast(p99_holtforecast1,h = 14)
plot(p99_holtpriceforecast1)

p99_holtpriceforecast2<- forecast(p99_holtforecast2,h=14)
plot(p99_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p99_hwTestMape <- regr.eval(Test$X99,p99_holtpriceforecast1$mean)
p99_hwTestMape

p99_hwTestMape2<-regr.eval(Test$X99,p99_holtpriceforecast2$mean)
p99_hwTestMape2

### ARIMA 
###  Auto Arima
p99_MODEL_ARIMA <- auto.arima(p99, ic='aic')
p99_pricearimaforecasts_autArima<- forecast(p99_MODEL_ARIMA,h=14)
plot(p99_pricearimaforecasts_autArima,flwd = 2)
p99_autoarimaTestMape <- regr.eval(Test$X99,p99_pricearimaforecasts_autArima$mean)
p99_autoarimaTestMape

############################################ Product-100 ######################################

plot(p100)
## HoltWinters model  with trend  and Seasonality
p100_holtforecast1 <- HoltWinters(p100, seasonal="additive")

### 
p100_holtforecast2 <- HoltWinters(p100, seasonal="multiplicative")


### Prediction on test data
p100_holtpriceforecast1<-  forecast(p100_holtforecast1,h = 14)
plot(p100_holtpriceforecast1)

p100_holtpriceforecast2<- forecast(p100_holtforecast2,h=14)
plot(p100_holtpriceforecast2,ylim = c(-100,200))

### Define the metric hw 
p100_hwTestMape <- regr.eval(Test$X100,p100_holtpriceforecast1$mean)
p100_hwTestMape

p100_hwTestMape2<-regr.eval(Test$X100,p100_holtpriceforecast2$mean)
p100_hwTestMape2

### ARIMA 
###  Auto Arima
p100_MODEL_ARIMA <- auto.arima(p100, ic='aic')
p100_pricearimaforecasts_autArima<- forecast(p100_MODEL_ARIMA,h=14)
plot(p100_pricearimaforecasts_autArima,flwd = 2)
p100_autoarimaTestMape <- regr.eval(Test$X100,p100_pricearimaforecasts_autArima$mean)
p100_autoarimaTestMape


############################################################################################
############################################################################################
############################################################################################

######################################## Products Prediction ######################################

p1_actual_df=ts(final_df$X1, frequency =7)
p2_actual_df=ts(final_df$X2, frequency =7)
p3_actual_df=ts(final_df$X3, frequency =7)
p4_actual_df=ts(final_df$X4, frequency =7)
p5_actual_df=ts(final_df$X5, frequency =7)
p6_actual_df=ts(final_df$X6, frequency =7)
p7_actual_df=ts(final_df$X7, frequency =7)
p8_actual_df=ts(final_df$X8, frequency =7)
p9_actual_df=ts(final_df$X9, frequency =7)
p10_actual_df=ts(final_df$X10, frequency =7)
p11_actual_df=ts(final_df$X11, frequency =7)
p12_actual_df=ts(final_df$X12, frequency =7)
p13_actual_df=ts(final_df$X13, frequency =7)
p14_actual_df=ts(final_df$X14, frequency =7)
p15_actual_df=ts(final_df$X15, frequency =7)
p16_actual_df=ts(final_df$X16, frequency =7)
p17_actual_df=ts(final_df$X17, frequency =7)
p18_actual_df=ts(final_df$X18, frequency =7)
p19_actual_df=ts(final_df$X19, frequency =7)
p20_actual_df=ts(final_df$X20, frequency =7)
p21_actual_df=ts(final_df$X21, frequency =7)
p22_actual_df=ts(final_df$X22, frequency =7)
p23_actual_df=ts(final_df$X23, frequency =7)
p24_actual_df=ts(final_df$X24, frequency =7)
p25_actual_df=ts(final_df$X25, frequency =7)
p26_actual_df=ts(final_df$X26, frequency =7)
p27_actual_df=ts(final_df$X27, frequency =7)
p28_actual_df=ts(final_df$X28, frequency =7)
p29_actual_df=ts(final_df$X29, frequency =7)
p30_actual_df=ts(final_df$X30, frequency =7)
p31_actual_df=ts(final_df$X31, frequency =7)
p32_actual_df=ts(final_df$X32, frequency =7)
p33_actual_df=ts(final_df$X33, frequency =7)
p34_actual_df=ts(final_df$X34, frequency =7)
p35_actual_df=ts(final_df$X35, frequency =7)
p36_actual_df=ts(final_df$X36, frequency =7)
p37_actual_df=ts(final_df$X37, frequency =7)
p38_actual_df=ts(final_df$X38, frequency =7)
p39_actual_df=ts(final_df$X39, frequency =7)
p40_actual_df=ts(final_df$X40, frequency =7)
p41_actual_df=ts(final_df$X41, frequency =7)
p42_actual_df=ts(final_df$X42, frequency =7)
p43_actual_df=ts(final_df$X43, frequency =7)
p44_actual_df=ts(final_df$X44, frequency =7)
p45_actual_df=ts(final_df$X45, frequency =7)
p46_actual_df=ts(final_df$X46, frequency =7)
p47_actual_df=ts(final_df$X47, frequency =7)
p48_actual_df=ts(final_df$X48, frequency =7)
p49_actual_df=ts(final_df$X49, frequency =7)
p50_actual_df=ts(final_df$X50, frequency =7)
p51_actual_df=ts(final_df$X51, frequency =7)
p52_actual_df=ts(final_df$X52, frequency =7)
p53_actual_df=ts(final_df$X53, frequency =7)
p54_actual_df=ts(final_df$X54, frequency =7)
p55_actual_df=ts(final_df$X55, frequency =7)
p56_actual_df=ts(final_df$X56, frequency =7)
p57_actual_df=ts(final_df$X57, frequency =7)
p58_actual_df=ts(final_df$X58, frequency =7)
p59_actual_df=ts(final_df$X59, frequency =7)
p60_actual_df=ts(final_df$X60, frequency =7)
p61_actual_df=ts(final_df$X61, frequency =7)
p62_actual_df=ts(final_df$X62, frequency =7)
p63_actual_df=ts(final_df$X63, frequency =7)
p64_actual_df=ts(final_df$X64, frequency =7)
p65_actual_df=ts(final_df$X65, frequency =7)
p66_actual_df=ts(final_df$X66, frequency =7)
p67_actual_df=ts(final_df$X67, frequency =7)
p68_actual_df=ts(final_df$X68, frequency =7)
p69_actual_df=ts(final_df$X69, frequency =7)
p70_actual_df=ts(final_df$X70, frequency =7)
p71_actual_df=ts(final_df$X71, frequency =7)
p72_actual_df=ts(final_df$X72, frequency =7)
p73_actual_df=ts(final_df$X73, frequency =7)
p74_actual_df=ts(final_df$X74, frequency =7)
p75_actual_df=ts(final_df$X75, frequency =7)
p76_actual_df=ts(final_df$X76, frequency =7)
p77_actual_df=ts(final_df$X77, frequency =7)
p78_actual_df=ts(final_df$X78, frequency =7)
p79_actual_df=ts(final_df$X79, frequency =7)
p80_actual_df=ts(final_df$X80, frequency =7)
p81_actual_df=ts(final_df$X81, frequency =7)
p82_actual_df=ts(final_df$X82, frequency =7)
p83_actual_df=ts(final_df$X83, frequency =7)
p84_actual_df=ts(final_df$X84, frequency =7)
p85_actual_df=ts(final_df$X85, frequency =7)
p86_actual_df=ts(final_df$X86, frequency =7)
p87_actual_df=ts(final_df$X87, frequency =7)
p88_actual_df=ts(final_df$X88, frequency =7)
p89_actual_df=ts(final_df$X89, frequency =7)
p90_actual_df=ts(final_df$X90, frequency =7)
p91_actual_df=ts(final_df$X91, frequency =7)
p92_actual_df=ts(final_df$X92, frequency =7)
p93_actual_df=ts(final_df$X93, frequency =7)
p94_actual_df=ts(final_df$X94, frequency =7)
p95_actual_df=ts(final_df$X95, frequency =7)
p96_actual_df=ts(final_df$X96, frequency =7)
p97_actual_df=ts(final_df$X97, frequency =7)
p98_actual_df=ts(final_df$X98, frequency =7)
p99_actual_df=ts(final_df$X99, frequency =7)
p100_actual_df=ts(final_df$X100, frequency =7)


#################### Running Models for actual prediction  ############

###product1
actual_p1_model <- HoltWinters(p1_actual_df,seasonal="multiplicative")
actual_p1_pricearimaforecasts_model4<- forecast(actual_p1_model,h=29)
plot(actual_p1_pricearimaforecasts_model4,flwd = 2)
p1_preds=actual_p1_pricearimaforecasts_model4$mean

###product2
actual_p2_model <- auto.arima(p2_actual_df, ic='aic')
actual_p2_pricearimaforecasts_model4<- forecast(actual_p2_model,h=29)
plot(actual_p2_pricearimaforecasts_model4,flwd = 2)
p2_preds=actual_p2_pricearimaforecasts_model4$mean

###product3
actual_p3_model <- HoltWinters(p3_actual_df,seasonal="additive")
actual_p3_pricearimaforecasts_model4<- forecast(actual_p3_model,h=29)
plot(actual_p3_pricearimaforecasts_model4,flwd = 2)
p3_preds=actual_p3_pricearimaforecasts_model4$mean

###product4
actual_p4_model <- auto.arima(p4_actual_df, ic='aic')
actual_p4_pricearimaforecasts_model4<- forecast(actual_p4_model,h=29)
plot(actual_p4_pricearimaforecasts_model4,flwd = 2)
p4_preds=actual_p4_pricearimaforecasts_model4$mean

###product5
actual_p5_model <- auto.arima(p5_actual_df, ic='aic')
actual_p5_pricearimaforecasts_model4<- forecast(actual_p5_model,h=29)
plot(actual_p5_pricearimaforecasts_model4,flwd = 2)
p5_preds=actual_p5_pricearimaforecasts_model4$mean

###product6
actual_p6_model <- HoltWinters(p6_actual_df,seasonal="additive")
actual_p6_pricearimaforecasts_model4<- forecast(actual_p6_model,h=29)
plot(actual_p6_pricearimaforecasts_model4,flwd = 2)
p6_preds=actual_p6_pricearimaforecasts_model4$mean

###product7
actual_p7_model <- HoltWinters(p7_actual_df,seasonal="mul")
actual_p7_pricearimaforecasts_model4<- forecast(actual_p7_model,h=29)
plot(actual_p7_pricearimaforecasts_model4,flwd = 2)
p7_preds=actual_p7_pricearimaforecasts_model4$mean

###product8
actual_p8_model <- HoltWinters(p8_actual_df,seasonal="additive")
actual_p8_pricearimaforecasts_model4<- forecast(actual_p8_model,h=29)
plot(actual_p8_pricearimaforecasts_model4,flwd = 2)
p8_preds=actual_p8_pricearimaforecasts_model4$mean

###product9
actual_p9_model <- HoltWinters(p9_actual_df,seasonal="additive")
actual_p9_pricearimaforecasts_model4<- forecast(actual_p9_model,h=29)
plot(actual_p9_pricearimaforecasts_model4,flwd = 2)
p9_preds=actual_p9_pricearimaforecasts_model4$mean

###product10
actual_p10_model <- HoltWinters(p10_actual_df,seasonal="additive")
actual_p10_pricearimaforecasts_model4<- forecast(actual_p10_model,h=29)
plot(actual_p10_pricearimaforecasts_model4,flwd = 2)
p10_preds=actual_p10_pricearimaforecasts_model4$mean

###product11
actual_p11_model <- HoltWinters(p11_actual_df,seasonal="additive")
actual_p11_pricearimaforecasts_model4<- forecast(actual_p11_model,h=29)
plot(actual_p11_pricearimaforecasts_model4,flwd = 2)
p11_preds=actual_p11_pricearimaforecasts_model4$mean

###product12
actual_p12_model <- auto.arima(p12_actual_df, ic='aic')
actual_p12_pricearimaforecasts_model4<- forecast(actual_p12_model,h=29)
plot(actual_p12_pricearimaforecasts_model4,flwd = 2)
p12_preds=actual_p12_pricearimaforecasts_model4$mean

###product13
actual_p13_model <- HoltWinters(p13_actual_df,seasonal="additive")
actual_p13_pricearimaforecasts_model4<- forecast(actual_p13_model,h=29)
plot(actual_p13_pricearimaforecasts_model4,flwd = 2)
p13_preds=actual_p13_pricearimaforecasts_model4$mean

###product14
actual_p14_model <- HoltWinters(p14_actual_df,seasonal="additive")
actual_p14_pricearimaforecasts_model4<- forecast(actual_p14_model,h=29)
plot(actual_p14_pricearimaforecasts_model4,flwd = 2)
p14_preds=actual_p14_pricearimaforecasts_model4$mean

###product15
actual_p15_model <- auto.arima(p15_actual_df, ic='aic')
actual_p15_pricearimaforecasts_model4<- forecast(actual_p15_model,h=29)
plot(actual_p15_pricearimaforecasts_model4,flwd = 2)
p15_preds=actual_p15_pricearimaforecasts_model4$mean

###product16
actual_p16_model <- HoltWinters(p16_actual_df,seasonal="additive")
actual_p16_pricearimaforecasts_model4<- forecast(actual_p16_model,h=29)
plot(actual_p16_pricearimaforecasts_model4,flwd = 2)
p16_preds=actual_p16_pricearimaforecasts_model4$mean

###product17
actual_p17_model <- HoltWinters(p17_actual_df,seasonal="additive")
actual_p17_pricearimaforecasts_model4<- forecast(actual_p17_model,h=29)
plot(actual_p17_pricearimaforecasts_model4,flwd = 2)
p17_preds=actual_p17_pricearimaforecasts_model4$mean

###product18
actual_p18_model <- auto.arima(p18_actual_df, ic='aic')
actual_p18_pricearimaforecasts_model4<- forecast(actual_p18_model,h=29)
plot(actual_p18_pricearimaforecasts_model4,flwd = 2)
p18_preds=actual_p18_pricearimaforecasts_model4$mean

###product19
actual_p19_model <- auto.arima(p19_actual_df, ic='aic')
actual_p19_pricearimaforecasts_model4<- forecast(actual_p19_model,h=29)
plot(actual_p19_pricearimaforecasts_model4,flwd = 2)
p19_preds=actual_p19_pricearimaforecasts_model4$mean

###product20
actual_p20_model <- HoltWinters(p20_actual_df,seasonal="additive")
actual_p20_pricearimaforecasts_model4<- forecast(actual_p20_model,h=29)
plot(actual_p20_pricearimaforecasts_model4,flwd = 2)
p20_preds=actual_p20_pricearimaforecasts_model4$mean

###product21
actual_p21_model <- HoltWinters(p21_actual_df,seasonal="additive")
actual_p21_pricearimaforecasts_model4<- forecast(actual_p21_model,h=29)
plot(actual_p21_pricearimaforecasts_model4,flwd = 2)
p21_preds=actual_p21_pricearimaforecasts_model4$mean

###product22
actual_p22_model <- HoltWinters(p22_actual_df,seasonal="additive")
actual_p22_pricearimaforecasts_model4<- forecast(actual_p22_model,h=29)
plot(actual_p22_pricearimaforecasts_model4,flwd = 2)
p22_preds=actual_p22_pricearimaforecasts_model4$mean

###product23
actual_p23_model <- HoltWinters(p23_actual_df,seasonal="additive")
actual_p23_pricearimaforecasts_model4<- forecast(actual_p23_model,h=29)
plot(actual_p23_pricearimaforecasts_model4,flwd = 2)
p23_preds=actual_p23_pricearimaforecasts_model4$mean

###product24
actual_p24_model <- auto.arima(p24_actual_df, ic='aic')
actual_p24_pricearimaforecasts_model4<- forecast(actual_p24_model,h=29)
plot(actual_p24_pricearimaforecasts_model4,flwd = 2)
p24_preds=actual_p24_pricearimaforecasts_model4$mean

###product25
actual_p25_model <- HoltWinters(p25_actual_df,seasonal="additive")
actual_p25_pricearimaforecasts_model4<- forecast(actual_p25_model,h=29)
plot(actual_p25_pricearimaforecasts_model4,flwd = 2)
p25_preds=actual_p25_pricearimaforecasts_model4$mean

###product26
actual_p26_model <- auto.arima(p26_actual_df, ic='aic')
actual_p26_pricearimaforecasts_model4<- forecast(actual_p26_model,h=29)
plot(actual_p26_pricearimaforecasts_model4,flwd = 2)
p26_preds=actual_p26_pricearimaforecasts_model4$mean

###product27
actual_p27_model <- HoltWinters(p27_actual_df,seasonal="additive")
actual_p27_pricearimaforecasts_model4<- forecast(actual_p27_model,h=29)
plot(actual_p27_pricearimaforecasts_model4,flwd = 2)
p27_preds=actual_p27_pricearimaforecasts_model4$mean

###product28
actual_p28_model <- HoltWinters(p28_actual_df,seasonal="additive")
actual_p28_pricearimaforecasts_model4<- forecast(actual_p28_model,h=29)
plot(actual_p28_pricearimaforecasts_model4,flwd = 2)
p28_preds=actual_p28_pricearimaforecasts_model4$mean

###product29
actual_p29_model <- HoltWinters(p29_actual_df,seasonal="additive")
actual_p29_pricearimaforecasts_model4<- forecast(actual_p29_model,h=29)
plot(actual_p29_pricearimaforecasts_model4,flwd = 2)
p29_preds=actual_p29_pricearimaforecasts_model4$mean

###product30
actual_p30_model <- HoltWinters(p30_actual_df,seasonal="additive")
actual_p30_pricearimaforecasts_model4<- forecast(actual_p30_model,h=29)
plot(actual_p30_pricearimaforecasts_model4,flwd = 2)
p30_preds=actual_p30_pricearimaforecasts_model4$mean

###product31 
actual_p31_model <- HoltWinters(p31_actual_df,seasonal="additive")
actual_p31_pricearimaforecasts_model4<- forecast(actual_p31_model,h=29)
plot(actual_p31_pricearimaforecasts_model4,flwd = 2)
p31_preds=actual_p31_pricearimaforecasts_model4$mean

###product32
actual_p32_model <- HoltWinters(p32_actual_df,seasonal="additive")
actual_p32_pricearimaforecasts_model4<- forecast(actual_p32_model,h=29)
plot(actual_p32_pricearimaforecasts_model4,flwd = 2)
p32_preds=actual_p32_pricearimaforecasts_model4$mean

###product33
actual_p33_model <- auto.arima(p33_actual_df, ic='aic')
actual_p33_pricearimaforecasts_model4<- forecast(actual_p33_model,h=29)
plot(actual_p33_pricearimaforecasts_model4,flwd = 2)
p33_preds=actual_p33_pricearimaforecasts_model4$mean

###product34
actual_p34_model <- HoltWinters(p34_actual_df,seasonal="additive")
actual_p34_pricearimaforecasts_model4<- forecast(actual_p34_model,h=29)
plot(actual_p34_pricearimaforecasts_model4,flwd = 2)
p34_preds=actual_p34_pricearimaforecasts_model4$mean

###product35
actual_p35_model <- auto.arima(p35_actual_df, ic='aic')
actual_p35_pricearimaforecasts_model4<- forecast(actual_p35_model,h=29)
plot(actual_p35_pricearimaforecasts_model4,flwd = 2)
p35_preds=actual_p35_pricearimaforecasts_model4$mean

###product36
actual_p36_model <- auto.arima(p36_actual_df, ic='aic')
actual_p36_pricearimaforecasts_model4<- forecast(actual_p36_model,h=29)
plot(actual_p36_pricearimaforecasts_model4,flwd = 2)
p36_preds=actual_p36_pricearimaforecasts_model4$mean

###product37
actual_p37_model <- auto.arima(p37_actual_df, ic='aic')
actual_p37_pricearimaforecasts_model4<- forecast(actual_p37_model,h=29)
plot(actual_p37_pricearimaforecasts_model4,flwd = 2)
p37_preds=actual_p37_pricearimaforecasts_model4$mean

###product38
actual_p38_model <- HoltWinters(p38_actual_df,seasonal="additive")
actual_p38_pricearimaforecasts_model4<- forecast(actual_p38_model,h=29)
plot(actual_p38_pricearimaforecasts_model4,flwd = 2)
p38_preds=actual_p38_pricearimaforecasts_model4$mean

###product39
actual_p39_model <- HoltWinters(p39_actual_df,seasonal="additive")
actual_p39_pricearimaforecasts_model4<- forecast(actual_p39_model,h=29)
plot(actual_p39_pricearimaforecasts_model4,flwd = 2)
p39_preds=actual_p39_pricearimaforecasts_model4$mean

###product40
actual_p40_model <- HoltWinters(p40_actual_df,seasonal="additive")
actual_p40_pricearimaforecasts_model4<- forecast(actual_p40_model,h=29)
plot(actual_p40_pricearimaforecasts_model4,flwd = 2)
p40_preds=actual_p40_pricearimaforecasts_model4$mean

###product41
actual_p41_model <- HoltWinters(p41_actual_df,seasonal="additive")
actual_p41_pricearimaforecasts_model4<- forecast(actual_p41_model,h=29)
plot(actual_p41_pricearimaforecasts_model4,flwd = 2)
p41_preds=actual_p41_pricearimaforecasts_model4$mean

###product42
actual_p42_model <- HoltWinters(p42_actual_df,seasonal="additive")
actual_p42_pricearimaforecasts_model4<- forecast(actual_p42_model,h=29)
plot(actual_p42_pricearimaforecasts_model4,flwd = 2)
p42_preds=actual_p42_pricearimaforecasts_model4$mean

###product43
actual_p43_model <- HoltWinters(p43_actual_df,seasonal="additive")
actual_p43_pricearimaforecasts_model4<- forecast(actual_p43_model,h=29)
plot(actual_p43_pricearimaforecasts_model4,flwd = 2)
p43_preds=actual_p43_pricearimaforecasts_model4$mean

###product44
actual_p44_model <- auto.arima(p44_actual_df, ic='aic')
actual_p44_pricearimaforecasts_model4<- forecast(actual_p44_model,h=29)
plot(actual_p44_pricearimaforecasts_model4,flwd = 2)
p44_preds=actual_p44_pricearimaforecasts_model4$mean

###product45
actual_p45_model <- HoltWinters(p45_actual_df,seasonal="additive")
actual_p45_pricearimaforecasts_model4<- forecast(actual_p45_model,h=29)
plot(actual_p45_pricearimaforecasts_model4,flwd = 2)
p45_preds=actual_p45_pricearimaforecasts_model4$mean

###product46
actual_p46_model <- auto.arima(p46_actual_df, ic='aic')
actual_p46_pricearimaforecasts_model4<- forecast(actual_p46_model,h=29)
plot(actual_p46_pricearimaforecasts_model4,flwd = 2)
p46_preds=actual_p46_pricearimaforecasts_model4$mean

###product47
actual_p47_model <- HoltWinters(p47_actual_df,seasonal="additive")
actual_p47_pricearimaforecasts_model4<- forecast(actual_p47_model,h=29)
plot(actual_p47_pricearimaforecasts_model4,flwd = 2)
p47_preds=actual_p47_pricearimaforecasts_model4$mean

###product48
actual_p48_model <- HoltWinters(p48_actual_df,seasonal="additive")
actual_p48_pricearimaforecasts_model4<- forecast(actual_p48_model,h=29)
plot(actual_p48_pricearimaforecasts_model4,flwd = 2)
p48_preds=actual_p48_pricearimaforecasts_model4$mean

###product49
actual_p49_model <- HoltWinters(p49_actual_df,seasonal="additive")
actual_p49_pricearimaforecasts_model4<- forecast(actual_p49_model,h=29)
plot(actual_p49_pricearimaforecasts_model4,flwd = 2)
p49_preds=actual_p49_pricearimaforecasts_model4$mean

###product50
actual_p50_model <- HoltWinters(p50_actual_df,seasonal="additive")
actual_p50_pricearimaforecasts_model4<- forecast(actual_p50_model,h=29)
plot(actual_p50_pricearimaforecasts_model4,flwd = 2)
p50_preds=actual_p50_pricearimaforecasts_model4$mean

###product51
actual_p51_model <- auto.arima(p51_actual_df, ic='aic')
actual_p51_pricearimaforecasts_model4<- forecast(actual_p51_model,h=29)
plot(actual_p51_pricearimaforecasts_model4,flwd = 2)
p51_preds=actual_p51_pricearimaforecasts_model4$mean

###product52
actual_p52_model <- HoltWinters(p52_actual_df,seasonal="additive")
actual_p52_pricearimaforecasts_model4<- forecast(actual_p52_model,h=29)
plot(actual_p52_pricearimaforecasts_model4,flwd = 2)
p52_preds=actual_p52_pricearimaforecasts_model4$mean

###product53
actual_p53_model <- HoltWinters(p53_actual_df,seasonal="additive")
actual_p53_pricearimaforecasts_model4<- forecast(actual_p53_model,h=29)
plot(actual_p53_pricearimaforecasts_model4,flwd = 2)
p53_preds=actual_p53_pricearimaforecasts_model4$mean

###product54
actual_p54_model <- HoltWinters(p54_actual_df,seasonal="additive")
actual_p54_pricearimaforecasts_model4<- forecast(actual_p54_model,h=29)
plot(actual_p54_pricearimaforecasts_model4,flwd = 2)
p54_preds=actual_p54_pricearimaforecasts_model4$mean

###product55
actual_p55_model <- HoltWinters(p55_actual_df,seasonal="additive")
actual_p55_pricearimaforecasts_model4<- forecast(actual_p55_model,h=29)
plot(actual_p55_pricearimaforecasts_model4,flwd = 2)
p55_preds=actual_p55_pricearimaforecasts_model4$mean

###product56
actual_p56_model <- HoltWinters(p56_actual_df,seasonal="additive")
actual_p56_pricearimaforecasts_model4<- forecast(actual_p56_model,h=29)
plot(actual_p56_pricearimaforecasts_model4,flwd = 2)
p56_preds=actual_p56_pricearimaforecasts_model4$mean

###product57
actual_p57_model <- HoltWinters(p57_actual_df,seasonal="additive")
actual_p57_pricearimaforecasts_model4<- forecast(actual_p57_model,h=29)
plot(actual_p57_pricearimaforecasts_model4,flwd = 2)
p57_preds=actual_p57_pricearimaforecasts_model4$mean

###product58
actual_p58_model <- HoltWinters(p58_actual_df,seasonal="additive")
actual_p58_pricearimaforecasts_model4<- forecast(actual_p58_model,h=29)
plot(actual_p58_pricearimaforecasts_model4,flwd = 2)
p58_preds=actual_p58_pricearimaforecasts_model4$mean

###product59
actual_p59_model <- HoltWinters(p59_actual_df,seasonal="additive")
actual_p59_pricearimaforecasts_model4<- forecast(actual_p59_model,h=29)
plot(actual_p59_pricearimaforecasts_model4,flwd = 2)
p59_preds=actual_p59_pricearimaforecasts_model4$mean

###product60
actual_p60_model <- HoltWinters(p60_actual_df,seasonal="additive")
actual_p60_pricearimaforecasts_model4<- forecast(actual_p60_model,h=29)
plot(actual_p60_pricearimaforecasts_model4,flwd = 2)
p60_preds=actual_p60_pricearimaforecasts_model4$mean

###product61
actual_p61_model <- HoltWinters(p61_actual_df,seasonal="additive")
actual_p61_pricearimaforecasts_model4<- forecast(actual_p61_model,h=29)
plot(actual_p61_pricearimaforecasts_model4,flwd = 2)
p61_preds=actual_p61_pricearimaforecasts_model4$mean

###product62
actual_p62_model <- auto.arima(p62_actual_df, ic='aic')
actual_p62_pricearimaforecasts_model4<- forecast(actual_p62_model,h=29)
plot(actual_p62_pricearimaforecasts_model4,flwd = 2)
p62_preds=actual_p62_pricearimaforecasts_model4$mean

###product63
actual_p63_model <- HoltWinters(p63_actual_df,seasonal="additive")
actual_p63_pricearimaforecasts_model4<- forecast(actual_p63_model,h=29)
plot(actual_p63_pricearimaforecasts_model4,flwd = 2)
p63_preds=actual_p63_pricearimaforecasts_model4$mean

###product64
actual_p64_model <- HoltWinters(p64_actual_df,seasonal="additive")
actual_p64_pricearimaforecasts_model4<- forecast(actual_p64_model,h=29)
plot(actual_p64_pricearimaforecasts_model4,flwd = 2)
p64_preds=actual_p64_pricearimaforecasts_model4$mean

###product65
actual_p65_model <- HoltWinters(p65_actual_df,seasonal="additive")
actual_p65_pricearimaforecasts_model4<- forecast(actual_p65_model,h=29)
plot(actual_p65_pricearimaforecasts_model4,flwd = 2)
p65_preds=actual_p65_pricearimaforecasts_model4$mean

###product66
actual_p66_model <- auto.arima(p66_actual_df, ic='aic')
actual_p66_pricearimaforecasts_model4<- forecast(actual_p66_model,h=29)
plot(actual_p66_pricearimaforecasts_model4,flwd = 2)
p66_preds=actual_p66_pricearimaforecasts_model4$mean

###product67
actual_p67_model <- auto.arima(p67_actual_df, ic='aic')
actual_p67_pricearimaforecasts_model4<- forecast(actual_p67_model,h=29)
plot(actual_p67_pricearimaforecasts_model4,flwd = 2)
p67_preds=actual_p67_pricearimaforecasts_model4$mean

###product68
actual_p68_model <- HoltWinters(p68_actual_df,seasonal="additive")
actual_p68_pricearimaforecasts_model4<- forecast(actual_p68_model,h=29)
plot(actual_p68_pricearimaforecasts_model4,flwd = 2)
p68_preds=actual_p68_pricearimaforecasts_model4$mean

###product69
actual_p69_model <- HoltWinters(p69_actual_df,seasonal="additive")
actual_p69_pricearimaforecasts_model4<- forecast(actual_p69_model,h=29)
plot(actual_p69_pricearimaforecasts_model4,flwd = 2)
p69_preds=actual_p69_pricearimaforecasts_model4$mean

###product70
actual_p70_model <- auto.arima(p70_actual_df, ic='aic')
actual_p70_pricearimaforecasts_model4<- forecast(actual_p70_model,h=29)
plot(actual_p70_pricearimaforecasts_model4,flwd = 2)
p70_preds=actual_p70_pricearimaforecasts_model4$mean

###product71
actual_p71_model <- HoltWinters(p71_actual_df,seasonal="additive")
actual_p71_pricearimaforecasts_model4<- forecast(actual_p71_model,h=29)
plot(actual_p71_pricearimaforecasts_model4,flwd = 2)
p71_preds=actual_p71_pricearimaforecasts_model4$mean

###product72
actual_p72_model <- HoltWinters(p72_actual_df,seasonal="additive")
actual_p72_pricearimaforecasts_model4<- forecast(actual_p72_model,h=29)
plot(actual_p72_pricearimaforecasts_model4,flwd = 2)
p72_preds=actual_p72_pricearimaforecasts_model4$mean

###product73
actual_p73_model <- HoltWinters(p73_actual_df,seasonal="additive")
actual_p73_pricearimaforecasts_model4<- forecast(actual_p73_model,h=29)
plot(actual_p73_pricearimaforecasts_model4,flwd = 2)
p73_preds=actual_p73_pricearimaforecasts_model4$mean

###product74
actual_p74_model <- HoltWinters(p74_actual_df,seasonal="additive")
actual_p74_pricearimaforecasts_model4<- forecast(actual_p74_model,h=29)
plot(actual_p74_pricearimaforecasts_model4,flwd = 2)
p74_preds=actual_p74_pricearimaforecasts_model4$mean

###product75
actual_p75_model <- HoltWinters(p75_actual_df,seasonal="additive")
actual_p75_pricearimaforecasts_model4<- forecast(actual_p75_model,h=29)
plot(actual_p75_pricearimaforecasts_model4,flwd = 2)
p75_preds=actual_p75_pricearimaforecasts_model4$mean

###product76
actual_p76_model <- HoltWinters(p76_actual_df,seasonal="additive")
actual_p76_pricearimaforecasts_model4<- forecast(actual_p76_model,h=29)
plot(actual_p76_pricearimaforecasts_model4,flwd = 2)
p76_preds=actual_p76_pricearimaforecasts_model4$mean

###product77
actual_p77_model <- HoltWinters(p77_actual_df,seasonal="additive")
actual_p77_pricearimaforecasts_model4<- forecast(actual_p77_model,h=29)
plot(actual_p77_pricearimaforecasts_model4,flwd = 2)
p77_preds=actual_p77_pricearimaforecasts_model4$mean

###product78
actual_p78_model <- auto.arima(p78_actual_df, ic='aic')
actual_p78_pricearimaforecasts_model4<- forecast(actual_p78_model,h=29)
plot(actual_p78_pricearimaforecasts_model4,flwd = 2)
p78_preds=actual_p78_pricearimaforecasts_model4$mean

###product79
actual_p79_model <- HoltWinters(p79_actual_df,seasonal="additive")
actual_p79_pricearimaforecasts_model4<- forecast(actual_p79_model,h=29)
plot(actual_p79_pricearimaforecasts_model4,flwd = 2)
p79_preds=actual_p79_pricearimaforecasts_model4$mean

###product80
actual_p80_model <- HoltWinters(p80_actual_df,seasonal="additive")
actual_p80_pricearimaforecasts_model4<- forecast(actual_p80_model,h=29)
plot(actual_p80_pricearimaforecasts_model4,flwd = 2)
p80_preds=actual_p80_pricearimaforecasts_model4$mean

###product81
actual_p81_model <- HoltWinters(p81_actual_df,seasonal="additive")
actual_p81_pricearimaforecasts_model4<- forecast(actual_p81_model,h=29)
plot(actual_p81_pricearimaforecasts_model4,flwd = 2)
p81_preds=actual_p81_pricearimaforecasts_model4$mean

###product82
actual_p82_model <- HoltWinters(p82_actual_df,seasonal="additive")
actual_p82_pricearimaforecasts_model4<- forecast(actual_p82_model,h=29)
plot(actual_p82_pricearimaforecasts_model4,flwd = 2)
p82_preds=actual_p82_pricearimaforecasts_model4$mean

###product83
actual_p83_model <- HoltWinters(p83_actual_df,seasonal="additive")
actual_p83_pricearimaforecasts_model4<- forecast(actual_p83_model,h=29)
plot(actual_p83_pricearimaforecasts_model4,flwd = 2)
p83_preds=actual_p83_pricearimaforecasts_model4$mean

###product84
actual_p84_model <- HoltWinters(p84_actual_df,seasonal="additive")
actual_p84_pricearimaforecasts_model4<- forecast(actual_p84_model,h=29)
plot(actual_p84_pricearimaforecasts_model4,flwd = 2)
p84_preds=actual_p84_pricearimaforecasts_model4$mean

###product85
actual_p85_model <- auto.arima(p85_actual_df, ic='aic')
actual_p85_pricearimaforecasts_model4<- forecast(actual_p85_model,h=29)
plot(actual_p85_pricearimaforecasts_model4,flwd = 2)
p85_preds=actual_p85_pricearimaforecasts_model4$mean

###product86
actual_p86_model <- HoltWinters(p86_actual_df,seasonal="additive")
actual_p86_pricearimaforecasts_model4<- forecast(actual_p86_model,h=29)
plot(actual_p86_pricearimaforecasts_model4,flwd = 2)
p86_preds=actual_p86_pricearimaforecasts_model4$mean

###product87
actual_p87_model <- auto.arima(p87_actual_df, ic='aic')
actual_p87_pricearimaforecasts_model4<- forecast(actual_p87_model,h=29)
plot(actual_p87_pricearimaforecasts_model4,flwd = 2)
p87_preds=actual_p87_pricearimaforecasts_model4$mean

###product88
actual_p88_model <- auto.arima(p78_actual_df, ic='aic')
actual_p88_pricearimaforecasts_model4<- forecast(actual_p88_model,h=29)
plot(actual_p88_pricearimaforecasts_model4,flwd = 2)
p88_preds=actual_p88_pricearimaforecasts_model4$mean

###product89
actual_p89_model <- HoltWinters(p89_actual_df,seasonal="additive")
actual_p89_pricearimaforecasts_model4<- forecast(actual_p89_model,h=29)
plot(actual_p89_pricearimaforecasts_model4,flwd = 2)
p89_preds=actual_p89_pricearimaforecasts_model4$mean

###product90
actual_p90_model <- HoltWinters(p90_actual_df,seasonal="additive")
actual_p90_pricearimaforecasts_model4<- forecast(actual_p90_model,h=29)
plot(actual_p90_pricearimaforecasts_model4,flwd = 2)
p90_preds=actual_p90_pricearimaforecasts_model4$mean

###product91
actual_p91_model <- HoltWinters(p91_actual_df,seasonal="additive")
actual_p91_pricearimaforecasts_model4<- forecast(actual_p91_model,h=29)
plot(actual_p91_pricearimaforecasts_model4,flwd = 2)
p91_preds=actual_p91_pricearimaforecasts_model4$mean

###product92
actual_p92_model <- HoltWinters(p92_actual_df,seasonal="additive")
actual_p92_pricearimaforecasts_model4<- forecast(actual_p92_model,h=29)
plot(actual_p92_pricearimaforecasts_model4,flwd = 2)
p92_preds=actual_p92_pricearimaforecasts_model4$mean

###product93
actual_p93_model <- HoltWinters(p93_actual_df,seasonal="additive")
actual_p93_pricearimaforecasts_model4<- forecast(actual_p93_model,h=29)
plot(actual_p93_pricearimaforecasts_model4,flwd = 2)
p93_preds=actual_p93_pricearimaforecasts_model4$mean

###product94
actual_p94_model <- HoltWinters(p94_actual_df,seasonal="additive")
actual_p94_pricearimaforecasts_model4<- forecast(actual_p94_model,h=29)
plot(actual_p94_pricearimaforecasts_model4,flwd = 2)
p94_preds=actual_p94_pricearimaforecasts_model4$mean

###product95
actual_p95_model <- HoltWinters(p95_actual_df,seasonal="additive")
actual_p95_pricearimaforecasts_model4<- forecast(actual_p95_model,h=29)
plot(actual_p95_pricearimaforecasts_model4,flwd = 2)
p95_preds=actual_p95_pricearimaforecasts_model4$mean

###product96
actual_p96_model <- HoltWinters(p96_actual_df,seasonal="additive")
actual_p96_pricearimaforecasts_model4<- forecast(actual_p96_model,h=29)
plot(actual_p96_pricearimaforecasts_model4,flwd = 2)
p96_preds=actual_p96_pricearimaforecasts_model4$mean

###product97
actual_p97_model <- HoltWinters(p97_actual_df,seasonal="additive")
actual_p97_pricearimaforecasts_model4<- forecast(actual_p97_model,h=29)
plot(actual_p97_pricearimaforecasts_model4,flwd = 2)
p97_preds=actual_p97_pricearimaforecasts_model4$mean

###product98
actual_p98_model <- HoltWinters(p98_actual_df,seasonal="additive")
actual_p98_pricearimaforecasts_model4<- forecast(actual_p98_model,h=29)
plot(actual_p98_pricearimaforecasts_model4,flwd = 2)
p98_preds=actual_p98_pricearimaforecasts_model4$mean

###product99
actual_p99_model <- HoltWinters(p99_actual_df,seasonal="additive")
actual_p99_pricearimaforecasts_model4<- forecast(actual_p99_model,h=29)
plot(actual_p99_pricearimaforecasts_model4,flwd = 2)
p99_preds=actual_p99_pricearimaforecasts_model4$mean

###product100
actual_p100_model <- HoltWinters(p100_actual_df,seasonal="additive")
actual_p100_pricearimaforecasts_model4<- forecast(actual_p100_model,h=29)
plot(actual_p100_pricearimaforecasts_model4,flwd = 2)
p100_preds=actual_p100_pricearimaforecasts_model4$mean


##################################################################################################
##########################Create a data frame and write the values out############################
##################################################################################################

pred_df=cbind.data.frame(p1_preds,p2_preds,p3_preds,p4_preds,p5_preds,p6_preds,p7_preds,p8_preds,p9_preds,p10_preds,
                         p11_preds,p12_preds,p13_preds,p14_preds,p15_preds,p16_preds,p17_preds,p18_preds,p19_preds,p20_preds,
                         p21_preds,p22_preds,p23_preds,p24_preds,p25_preds,p26_preds,p27_preds,p28_preds,p29_preds,p30_preds,
                         p31_preds,p32_preds,p33_preds,p34_preds,p35_preds,p36_preds,p37_preds,p38_preds,p39_preds,p40_preds,
                         p41_preds,p42_preds,p43_preds,p44_preds,p45_preds,p46_preds,p47_preds,p48_preds,p49_preds,p50_preds,
                         p51_preds,p52_preds,p53_preds,p54_preds,p55_preds,p56_preds,p57_preds,p58_preds,p59_preds,p60_preds,
                         p61_preds,p62_preds,p63_preds,p64_preds,p65_preds,p66_preds,p67_preds,p68_preds,p69_preds,p70_preds,
                         p71_preds,p72_preds,p73_preds,p74_preds,p75_preds,p76_preds,p77_preds,p78_preds,p79_preds,p80_preds,
                         p81_preds,p82_preds,p83_preds,p84_preds,p85_preds,p86_preds,p87_preds,p88_preds,p89_preds,p90_preds,
                         p91_preds,p92_preds,p93_preds,p94_preds,p95_preds,p96_preds,p97_preds,p98_preds,p99_preds,p100_preds)

View(pred_df)
m2 <- t(pred_df)
converted_df2 <- data.frame(r1= row.names(m2), m2, row.names=NULL)

converted_df2[converted_df2<0]=0
converted_df2=converted_df2[,-1]
converted_df2=cbind.data.frame(product_distribution_training_set$product_id,converted_df2)

m3 <- t(overall_preds)
overall_df <- data.frame(r1= row.names(m3), m3, row.names=NULL)
product_id=0
overall_df=overall_df[,-1]
overall_df=cbind.data.frame(product_id,overall_df)

colnames(converted_df2)[1] <- "product_id"

Final_predictions=rbind.data.frame(overall_df,converted_df2)

View(Final_predictions)


write.csv(Final_predictions, file = "product_predictions.csv",col.names = FALSE,row.names = TRUE)

