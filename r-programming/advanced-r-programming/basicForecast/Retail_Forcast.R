##########################
#R data for Monthly sales projection
#We will use this data to forcast next few months of sales per month
##########################

#remove all existing global variable 
rm(list = ls())

#Load forcasting package
#if not exit then import it 
#install.packages("fpp2")
library(fpp2)
#this will load ggplot package behind the scene 

#Load the data 
data = read.csv("c:/Users/Aman/Desktop/Forcast_Test/TestData.csv")

#declare this as time series data 
tsQty = ts(as.numeric(data[,2]) , start = c(2017,11) , frequency = 12)

################################
#Prelimanary Analysis
################################

#Time Plot as per inserted data
autoplot(tsQty) + ggtitle("Time Plot for sold Qty per Month") + ylab("Qty")

#Data have some strong trend, we are going to  analyse Investigative Transformation
#Take first diffrence of Data to remove trend 
#check change of sales month to month basis 
DY = diff(tsQty)

#Time Plot diffrence data
autoplot(DY) + ggtitle("difference of Qty sold per Month") + ylab("Qty")

#series appear trend-stationary, used to investigate seasionality
ggseasonplot(DY) + ggtitle("Seasonal Plot: Change Monthly retail sales") + ylab("Qty")

#alternative seasonal plot
#blue line represent mean of sub series 
#ggsubseriesplot(DY) + ggtitle("Sub Seasonal Plot: Change Monthly retail sales") + ylab("Qty")
#lack of data to plot this 
#Error : "Each season requires at least 2 observations. Your series length may be too short for this graphic."


#Forcast

##benchmark method 
##sessional naive method as benchmark
## y_t = y_{t-s} + e_t
fit = snaive(DY)
print(summary(fit)) #Residual sd: 68.5006 so we are missing avg by 68.5006
#Prediction Plot
checkresiduals(fit)


##Exponential Smothing model 
fit2 = ets(tsQty)
print(summary(fit2))
checkresiduals(fit2)


##Fit an ARIMA Model
fit_arima = auto.arima(tsQty, d=1 , D = 1, stepwise = FALSE , approximation = FALSE , trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)


#Forcast with ARIMA Model 
fcst = forecast(fit_arima , h = 12)
autoplot(fcst)
print(summary(fcst))
  