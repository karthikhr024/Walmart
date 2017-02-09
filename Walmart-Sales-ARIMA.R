  require(forecast)
  
  #Pre-Req
  #1. set the working directory
  #2. copy train.csv and stores.csv files into wd/data.
  #3. install above 3 packages.
  getwd()
  data<-read.csv("data/train.csv")
  stores<-read.csv("data/stores.csv")
  #Filtering dept1
  data.dept<-subset(data,data$Dept == 1)
  
  #Filtering storeA from Above
  storeA<-subset(stores,stores$Type=='A')
  uniqueStoresA <-unique(storeA$Store)
  data.dept.StoreA<-subset(data.dept, is.element(data.dept$Store,uniqueStoresA))
  
  #Filtering storeB from Above
  storeB<-subset(stores,stores$Type=='B')
  uniqueStoresB <-unique(storeB$Store)
  data.dept.StoreB<-subset(data.dept, is.element(data.dept$Store,uniqueStoresB))
  
  #Filtering storeC from Above
  storeC<-subset(stores,stores$Type=='C')
  uniqueStoresC <-unique(storeC$Store)
  data.dept.StoreC<-subset(data.dept, is.element(data.dept$Store,uniqueStoresC))
  
  uniqueDates <- unique(data.dept$Date)
  
  data.dept.Total <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
  data.dept.Total.StoreA <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
  data.dept.Total.StoreB <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
  data.dept.Total.StoreC <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
  for(uDate in uniqueDates){
    ssTotal<-subset(data.dept,data.dept$Date==uDate)
    data.dept.Total <- rbind( data.dept.Total, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotal$Weekly_Sales)))
    
    ssTotalA<-subset(data.dept.StoreA,data.dept.StoreA$Date==uDate)
    data.dept.Total.StoreA <- rbind( data.dept.Total.StoreA, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotalA$Weekly_Sales)))
    
    ssTotalB<-subset(data.dept.StoreB,data.dept.StoreB$Date==uDate)
    data.dept.Total.StoreB <- rbind( data.dept.Total.StoreB, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotalB$Weekly_Sales)))
    
    ssTotalC<-subset(data.dept.StoreC,data.dept.StoreC$Date==uDate)
    data.dept.Total.StoreC <- rbind( data.dept.Total.StoreC, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotalC$Weekly_Sales)))
  }
  
  
  plotForecastErrors <- function(forecasterrors)
  {
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd   <- sd(forecasterrors)
    mymin  <- min(forecasterrors) - mysd*5
    mymax  <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  }
  
  
  #Run Models for Aggregate Tests
  tseries <- ts(data.dept.Total$Weekly_Sales, start=c(2010), end= c(2013),frequency=52)
  
  fitAggreateAutoArima <- auto.arima(tseries, stepwise=FALSE, approximation=FALSE)
  plot(fitAggreateAutoArima$x,col="red",xlab = 'Fit Aggreate Auto.Arima',ylab = 'Average Sales of All Stores')
  lines(fitted(fitAggreateAutoArima),col="blue")
  plotForecastErrors(fitAggreateAutoArima$residuals)
  
  fitAggreate011011 <- Arima(tseries,order=c(0,1,1),seasonal = c(0,1,1))
  plot(fitAggreate011011$x,col="red",xlab = 'Fit Aggreate (0,1,1)(0,1,1)',ylab = 'Average Sales of All Stores')
  lines(fitted(fitAggreate011011),col="blue")
  plotForecastErrors(fitAggreate011011$residuals)
  
  fitAggreate010010 <- Arima(tseries,order=c(0,1,0),seasonal = c(0,1,0))
  plot(fitAggreate010010$x,col="red",xlab = 'Fit Aggreate (0,1,0)(0,1,0)',ylab = 'Average Sales of All Stores')
  lines(fitted(fitAggreate010010),col="blue")
  plotForecastErrors(fitAggreate010010$residuals)
  
  fitAggreate000010 <- Arima(tseries,order=c(0,0,0),seasonal = c(0,1,0))
  plot(fitAggreate000010$x,col="red",xlab = 'Fit Aggreate (0,0,0)(0,1,0)',ylab = 'Average Sales of All Stores')
  lines(fitted(fitAggreate000010),col="blue")
  plotForecastErrors(fitAggreate000010$residuals)
  
  
  
  #Run Models for Store Type A
  tseriesA <- ts(data.dept.Total.StoreA$Weekly_Sales, start=c(2010,2), end= c(2013),frequency=52)
  
  fitAAutoArima <- auto.arima(tseriesA, stepwise=FALSE, approximation=FALSE)
  plot(fitAAutoArima$x,col="red",xlab = 'Fit Store Type-A Auto.Arima',ylab = 'Average Sales of Store Type A')
  lines(fitted(fitAAutoArima),col="blue")
  plotForecastErrors(fitAAutoArima$residuals)
  
  fitStoreTypeA011011 <- Arima(tseriesA,order=c(0,1,1),seasonal = c(0,1,1))
  plot(fitStoreTypeA011011$x,col="red",xlab = 'Fit Store Type-A (0,1,1)(0,1,1)',ylab = 'Average Sales of Store Type A')
  lines(fitted(fitStoreTypeA011011),col="blue")
  plotForecastErrors(fitStoreTypeA011011$residuals)
  
  fitStoreTypeA010010 <- Arima(tseriesA,order=c(0,1,0),seasonal = c(0,1,0))
  plot(fitStoreTypeA010010$x,col="red",xlab = 'Fit Store Type-A (0,1,0)(0,1,0)',ylab = 'Average Sales of Store Type A')
  lines(fitted(fitStoreTypeA010010),col="blue")
  plotForecastErrors(fitStoreTypeA010010$residuals)
  
  fitStoreTypeA000010 <- Arima(tseriesA,order=c(0,0,0),seasonal = c(0,1,0))
  plot(fitStoreTypeA000010$x,col="red",xlab = 'Fit Store Type-A (0,0,0)(0,1,0)',ylab = 'Average Sales of Store Type A')
  lines(fitted(fitStoreTypeA000010),col="blue")
  plotForecastErrors(fitStoreTypeA000010$residuals)
  
  
  
  #Run Models for Store Type B
  tseriesB <- ts(data.dept.Total.StoreB$Weekly_Sales, start=c(2010), end= c(2013),frequency=52)
  
  fitBAutoArima <- auto.arima(tseriesB, stepwise=FALSE, approximation=FALSE)
  plot(fitBAutoArima$x,col="red",xlab = 'Fit Store Type-B Auto.Arima',ylab = 'Average Sales of Store Type B')
  lines(fitted(fitBAutoArima),col="blue")
  plotForecastErrors(fitBAutoArima$residuals)
  
  fitStoreTypeB011011 <- Arima(tseriesB,order=c(0,1,1),seasonal = c(0,1,1))
  plot(fitStoreTypeB011011$x,col="red",xlab = 'Fit Store Type-B (0,1,1)(0,1,1)',ylab = 'Average Sales of Store Type B')
  lines(fitted(fitStoreTypeB011011),col="blue")
  plotForecastErrors(fitStoreTypeB011011$residuals)
  
  fitStoreTypeB010010 <- Arima(tseriesB,order=c(0,1,0),seasonal = c(0,1,0))
  plot(fitStoreTypeB010010$x,col="red",xlab = 'Fit Store Type-B (0,1,0)(0,1,0)',ylab = 'Average Sales of Store Type B')
  lines(fitted(fitStoreTypeB010010),col="blue")
  plotForecastErrors(fitStoreTypeB010010$residuals)
  
  fitStoreTypeB000010 <- Arima(tseriesB,order=c(0,0,0),seasonal = c(0,1,0))
  plot(fitStoreTypeB000010$x,col="red",xlab = 'Fit Store Type-B (0,0,0)(0,1,0)',ylab = 'Average Sales of Store Type B')
  lines(fitted(fitStoreTypeB000010),col="blue")
  plotForecastErrors(fitStoreTypeB000010$residuals)
  
  
  
  #Run Models for Store Type C
  tseriesC <- ts(data.dept.Total.StoreC$Weekly_Sales, start=c(2010), end= c(2013),frequency=52)
  
  fitCAutoArima <- auto.arima(tseriesC, stepwise=FALSE, approximation=FALSE)
  plot(fitCAutoArima$x,col="red",xlab = 'Fit Store Type-C Auto.Arima',ylab = 'Average Sales of Store Type C')
  lines(fitted(fitCAutoArima),col="blue")
  plotForecastErrors(fitCAutoArima$residuals)
  
  fitStoreTypeC011011 <- Arima(tseriesC,order=c(0,1,1),seasonal = c(0,1,1))
  plot(fitStoreTypeC011011$x,col="red",xlab = 'Fit Store Type-C (0,1,1)(0,1,1)',ylab = 'Average Sales of Store Type C')
  lines(fitted(fitStoreTypeC011011),col="blue")
  plotForecastErrors(fitStoreTypeC011011$residuals)
  
  fitStoreTypeC010010 <- Arima(tseriesC,order=c(0,1,0),seasonal = c(0,1,0))
  plot(fitStoreTypeC010010$x,col="red",xlab = 'Fit Store Type-C (0,1,0)(0,1,0)',ylab = 'Average Sales of Store Type C')
  lines(fitted(fitStoreTypeC010010),col="blue")
  plotForecastErrors(fitStoreTypeC010010$residuals)
  
  fitStoreTypeC000010 <- Arima(tseriesC,order=c(0,0,0),seasonal = c(0,1,0))
  plot(fitStoreTypeC000010$x,col="red",xlab = 'Fit Store Type-C (0,0,0)(0,1,0)',ylab = 'Average Sales of Store Type C')
  lines(fitted(fitStoreTypeC000010),col="blue")
  plotForecastErrors(fitStoreTypeC000010$residuals)