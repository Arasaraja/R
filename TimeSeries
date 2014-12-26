library(forecast)
library(plyr)
library(tis)

#inputs for deployR (values are assigned for testing purposes)
ts.frequency<-12
ts.value<-c(55216,91780,100854,276855,358831,327625,311527,125824,35955,20555,13719,13986,49750,83213,107067,316945,270953,387567,375434,91299,38410,25714,14590,24491,40489,71438,89187,308723,239086,434629,431949,113034,41066,19811,15296,21204,46299,100546,165425,260206)
ts.start.year<-2009
ts.start.value<-1
model.type<-1
model.BoxCox<-1
forecast.period_type = "m"
forecast.period<-12
graph.title<-"Sales Forecast"
graph.x<-"Date"
graph.y<-"Revenue"
graph.type<-0


#determine the forecast period to use (monthly or weekly)  
if (forecast.period_type == "m"){
  forecast.period_t<-forecast.period * (ts.frequency/12)
} else {
  forecast.period_t<-forecast.period
}

#create the time series object
my.ts <- ts(ts.value,start=c(ts.start.year, ts.start.value), frequency=ts.frequency)

if (model.BoxCox == 0) {
  lambda <- NULL
} else {
  lambda <- BoxCox.lambda(my.ts)
}

if (model.type == 0) {
  #create an arima model
  model<-auto.arima(my.ts, lambda = lambda)
} else {
  model<-tslm(my.ts ~ trend + season, lambda = lambda)
}
#run a forecast
f<-forecast(model, h=forecast.period_t,)

if (graph.type == 0) {
  
  #create a plot with a loses curve
  df1<-data.frame(as.vector(f$x),time=c(as.Date(time(f$x))))
  names(df1) <- c("Demand","Date")
  df2<-data.frame(as.vector(f$mean), time=c(as.Date(time(f$mean))))
  names(df2) <- c("Demand","Date")
  df3<-join(df1, df2, type="full")
  
  library(ggplot2)
  library(scales)
  
  myplot<-qplot(Date,Demand,data=df3,geom=c("line","smooth"),size=I(1), main=graph.title, xlab=graph.x, ylab=graph.y) + scale_y_continuous(labels = dollar)
  print(myplot)
}else {
  
  #use the standard plot for the forecast
  plot(f, main=graph.title, xlab=graph.x, ylab=graph.y, yaxt="n")
  axis(2, axTicks(2), label=sprintf("$%0.0f", axTicks(2)))
  
}

forecast.output<-print(f)
output <- list(); 
output$observed <- f$x; 
output$predictions <- f$mean;
output$upper <- f$upper; 
output$lower <- f$lower; 
