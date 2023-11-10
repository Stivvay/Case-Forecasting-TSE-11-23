#todo:
# residuals van forecasts

library('forecast')
library("MCS")
#Part 1: IN-SAMPLE ------------------------------------------------------------
expanding <- read.csv("forecast_expanding.csv")
rolling <-read.csv('forecast_rolling.csv')
#we define the data as a proper time series
demand = data.frame(demand = citibike[,6])
insampleseries = citibike[0:2880,6] #1jan - 30apr
start_time <- as.POSIXct("2023-01-01 00:00:00")  
end_time <- as.POSIXct("2023-04-30 23:00:00")   
timeseries <- ts(insampleseries, start = as.Date(start_time), end = as.Date(end_time), frequency = 24)

#basic data plots
plot(demand[c(0:48),1],type="l",col="black",lwd=3,xlab = 'First two days', ylab='demand')
abline(v=24)
plot(demand[c(0:336),1],type="l",col="black",lwd=3,xlab = 'First two weeks', ylab='demand')
abline(v=168)
plot(demand[,1],type="l",col="black",lwd=3,xlab = 'Full sample', ylab='demand')
#sarima_t = arima(timeseries, order=c(2,0,0), seasonal = list(order=c(3,1,0)))
#summary(sarima_t)
#sarima_t2 = arima(timeseries, order=c(2,0,0), seasonal = list(order=c(1,1,0)))
#summary(sarima_t2)

#we fit the arima,sarima and ets models automatically using the forecast package
auto_arima = auto.arima(timeseries,seasonal=FALSE)
summary(auto_arima)

auto_sarima = auto.arima(timeseries)
summary(auto_sarima)

exp_sm = ets(timeseries,model="ZZZ")
summary(exp_sm)
#forecast.ets(exp_sm,744)
#plot(forecast.ets(exp_sm,744))

#res = residuals(exp_sm)
#plot(res)
#fit = fitted(exp_sm)
#plot(insampleseries[2500:2880],type='l',lwd=2)
#lines(fit[2501:2881],col='red',lwd=2) #shift because t0 exp sm. 

#part 2.1 OUT-OF-SAMPLE ROLLING
exp_sm_res_rw<-c()
arima_res_rw<-c()
sarima_res_rw<-c()
#rolling window forecast
for (i in 0:30){  
  #we calculate the new time series
  insampleseries = citibike[(1+24*i):(2880+24*i),6] #Hour 2880 is 2023-04-30 23:00:00
  start_time <- as.POSIXct("2023-01-01 00:00:00")
  start_time = start_time + (i*60*60*24) # 1 day = 60s * 60min * 24u
  end_time <- as.POSIXct("2023-04-30 23:00:00")  
  end_time = end_time + (i*60*60*24)
  timeseries <- ts(insampleseries, start = as.Date(start_time), end = as.Date(end_time), frequency = 24)
  
  #model exp. smoothing
  exp_sm = ets(timeseries,model="ZZZ")
  x = forecast.ets(exp_sm,24)
  exp_sm_res_rw<-append(exp_sm_res_rw,x$mean)
  
  #model arima
  arima = arima(timeseries, order=c(2,1,0))
  y = forecast(arima,24)
  arima_res_rw <- append(arima_res_rw,y$mean)
  
  #model sarima
  sarima = arima(timeseries, order=c(2,0,0), seasonal = list(order=c(2,1,0)))
  z = forecast(sarima,24)
  sarima_res_rw <- append(sarima_res_rw,z$mean)
}
#to reduce computation time we have the data in a csv file, this skips the loop
 # arima_res_rw <-rolling$arima
 # sarima_res_rw <-rolling$sarima
 # exp_sm_res_rw <-rolling$exp_sm

#print(exp_sm_res_rw)
#print(arima_res_rw)
#print(sarima_res_rw)
#we collect the out of sample data and plot it against the forecasts.
series_may = demand[2881:3624,1]
plot(demand[2881:3624,1],type="l",col="green",lwd=3,xlab = 'Observation in May', ylab='demand')
lines(exp_sm_res_rw,lwd=2,col="red",type="l")
lines(arima_res_rw,lwd=2,col="yellow",type="l")
lines(sarima_res_rw,lwd=2,col='blue',type='l')
legend("topleft", legend = c("Sample", "ETS",'ARIMA','SARIMA'), 
       col = c("green", "red","yellow",'blue'),lwd=2)
  accuracy(series_may, exp_sm_res_rw)
accuracy(series_may, arima_res_rw)
accuracy(series_may, sarima_res_rw)

#code to export the csv
#forecast_rolling = data.frame(demand = series_may, exp_sm = exp_sm_res_rw, arima = arima_res_rw, sarima = sarima_res_rw)
#print(forecast_rolling)
#write.csv(forecast_rolling, file="forecast_rolling.csv", row.names=FALSE)

#Part 2.2 OUT-OF-SAMPLE EXPANDING WINDOW ------------------------------------------------------------
exp_sm_res_ew<-c()
arima_res_ew<-c()
sarima_res_ew<-c()
start_time <- as.POSIXct("2023-01-01 00:00:00")
#expanding window forecast
for (i in 0:30){  
  series = citibike[(1+24*i):(2880+24*i),6]#Hour 2880 is 2023-04-30 23:00:00
  end_time <- as.POSIXct("2023-04-30 23:00:00")  
  end_time = end_time + (i*60*60*24)
  timeseries <- ts(series, start = as.Date(start_time), end = as.Date(end_time), frequency = 24)
  
  #model exp. smoothing
  exp_sm = ets(timeseries,model="ZZZ")
  x_2 = forecast.ets(exp_sm,24)
  exp_sm_res_ew<-append(exp_sm_res_ew,x_2$mean)
  plot(x_2)
  
  #model arima
  arima = arima(timeseries, order=c(2,1,0))
  y_2 = forecast(arima,24)
  arima_res_ew <- append(arima_res_ew,y_2$mean)
  plot(y_2)
  
  #model sarima
  sarima = arima(timeseries, order=c(2,0,0), seasonal = list(order=c(2,1,0)))
  z_2 = forecast(sarima,24)
  sarima_res_ew <- append(sarima_res_ew,z_2$mean)
  plot(z_2)
}

#to reduce computation time we have the data in a csv file, this skips the loop
#arima_res_ew <-expanding$arima
 #sarima_res_ew <-expanding$sarima
 #exp_sm_res_ew <-expanding$exp_sm
#print(exp_sm_res_ew)
#print(arima_res_ew)
#print(sarima_res_ew)


plot(demand[2881:3624,1],type="l",col="green",lwd=3,xlab = 'Observation in May', ylab='demand')
lines(exp_sm_res_ew,lwd=2,col="red",type="l")
lines(arima_res_ew,lwd=2,col="yellow",type="l")
lines(sarima_res_ew,lwd=2,col='blue',type='l')
legend("topleft", legend = c("Sample", "ETS",'ARIMA','SARIMA'), 
       col = c("green", "red","yellow",'blue'),lwd=2)
#forecast_expanding = data.frame(demand = series_may, exp_sm = exp_sm_res_ew, arima = arima_res_ew, sarima = sarima_res_ew)
#print(forecast_expanding)
#write.csv(forecast_expanding, file="forecast_expanding.csv", row.names=FALSE)

#PART 3: EVALUATION ------------------------------------------------------------

#Residuals rolling
resid_exp_rw = exp_sm_res_rw - series_may
resid_arima_rw = arima_res_rw - series_may
resid_sarima_rw = sarima_res_rw - series_may
#Residuals expanding
resid_exp_ew = exp_sm_res_ew - series_may
resid_arima_ew = arima_res_ew - series_may
resid_sarima_ew = sarima_res_ew - series_may

#Basic statistics forecast
accuracy(series_may,exp_sm_res_rw)
accuracy(series_may,exp_sm_res_ew)
accuracy(series_may,arima_res_rw)
accuracy(series_may,arima_res_ew)
accuracy(series_may,sarima_res_rw)
accuracy(series_may,sarima_res_ew)

#Diebold Mariano 
#H=24
dm.test(resid_exp_rw,resid_sarima_rw,h=24) #exp - sarima (rolling)
dm.test(resid_exp_ew,resid_sarima_ew,h=24) #exp - sarima (expanding)
dm.test(resid_exp_rw,resid_arima_rw,h=24) #exp - arima (rolling)
dm.test(resid_exp_ew,resid_arima_ew,h=24) #exp - arima (expanding)
dm.test(resid_arima_rw,resid_sarima_rw,h=24) #arima - sarima (rolling)
dm.test(resid_arima_ew,resid_sarima_ew,h=24) #arima - sarima (expanding)
 

#Model Confidence Set 
r_1 = LossLevel(realized=series_may,evaluated=exp_sm_res_rw) #exp rolling
r_2 = LossLevel(realized=series_may,evaluated=arima_res_rw) #arima rolling
r_3 = LossLevel(realized=series_may,evaluated=sarima_res_rw) #sarima rolling
e_1 = LossLevel(realized=series_may,evaluated=exp_sm_res_ew) #exp expanding
e_2 = LossLevel(realized=series_may,evaluated=arima_res_ew) #arima expanding
e_3 = LossLevel(realized=series_may,evaluated=sarima_res_ew) #sarima expanding

MCSprocedure(data.frame(r_1,r_2,r_3)) #rolling comparison
MCSprocedure(data.frame(e_1,e_2,e_3)) 
MCSprocedure(data.frame(e_1,e_2,e_3,r_1,r_2,r_3))


#plots between window selection
plot(demand[2881:3624,1],type="l",col="green",lwd=3,xlab = 'Observation in May', ylab='demand')
lines(exp_sm_res_ew,lwd=2,col="red",type="l")
lines(exp_sm_res_rw,lwd=2,col="blue",type="l")

legend("topleft", legend = c("Sample", "Rolling ETS",'EXP. ETS'), 
       col = c("green", "red",'blue'),lwd=2)

plot(demand[2881:3624,1],type="l",col="green",lwd=3,xlab = 'Observation in May', ylab='demand')
lines(sarima_res_ew,lwd=2,col="red",type="l")
lines(sarima_res_rw,lwd=2,col="blue",type="l")

legend("topleft", legend = c("Sample", "Rolling SARIMA",'EXP. SARIMA'), 
       col = c("green", "red",'blue'),lwd=2)


plot(demand[2881:3624,1],type="l",col="green",lwd=3,xlab = 'Observation in May', ylab='demand')
lines(arima_res_ew,lwd=2,col="red",type="l")
lines(arima_res_rw,lwd=2,col="blue",type="l")

legend("topleft", legend = c("Sample", "Rolling ARIMA",'EXP. ARIMA'), 
       col = c("green", "red",'blue'),lwd=2)


