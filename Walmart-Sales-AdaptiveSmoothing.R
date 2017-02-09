
  #Pre-Req
  #1. set the working directory
  #2. copy train.csv file into wd/data.
  getwd()
  data<-read.csv("data/train.csv")
  stores<-read.csv("data/stores.csv")
  #Filtering dept1
  data.dept<-subset(data,data$Dept == 1)
  
  uniqueDates <- unique(data.dept$Date)
  
  data.dept.Total <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
  for(uDate in uniqueDates){
    ssTotal<-subset(data.dept,data.dept$Date==uDate)
    data.dept.Total <- rbind( data.dept.Total, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotal$Weekly_Sales)))
  }

  alpha = 0.3
  beta = 0.2
  plusAlpha=0
  minusAlpha=0
   
  actualData = data.dept.Total$Weekly_Sales;
  forecastData = array(1:length(actualData))
  forecastData[1] = mean(data.dept.Total$Weekly_Sales)
  error = actualData[1] - forecastData[1]
  for(i in 2:143){
		  forecastData[i] = forecastData[i-1]+  alpha*(actualData[i-1]-forecastData[i-1]);
		        plusAlpha = forecastData[i-1]+ (alpha+beta)*(actualData[i-1]-forecastData[i-1]);
		       minusAlpha = forecastData[i-1]+ (alpha-beta)*(actualData[i-1]- forecastData[i-1]);
		  if(plusAlpha>minusAlpha){
		    	a=sort(c(plusAlpha, minusAlpha, actualData[i-1]), decreasing = TRUE)
		  }else{
		    	a=sort(c(plusAlpha, minusAlpha, actualData[i-1]))
		  }
		  pos=match(actualData[i-1],a)
		  if(pos==1){
		    	alpha = alpha+beta;
		  }else if(pos==3){
		    	alpha = alpha - beta;
		  }else{
			   #Do Nothing
		  }	
  }
  df= data.frame(actualData,forecastData)
  names(df) = c("Actual", "Forecasted")
  print(df)
  write.csv(df,file = "expo-aggre.csv")