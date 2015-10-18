### Kaggle Rossmann completition 2015_10_11 ###

#### Intro and packages ####
  setwd("C:/Users/Hana/R/Kaggle_Rossmann")

## delete everything old
  rm(list=ls())

#install.packages("ggplot2")
library("ggplot2")

#### Import and Clean Up ####

## import train and test data ##
  train<-read.csv("train.csv")
  test<-read.csv("test.csv")
  store<-read.csv("store.csv")
  str(train)
  str(test)

## see what they are - change numbers to factors
  train$Store<-as.factor(train$Store)
  train$DayOfWeek<-as.factor(train$DayOfWeek)
  train$Open<-as.factor(train$Open)
  train$Promo<-as.factor(train$Promo)
  train$SchoolHoliday<-as.factor(train$SchoolHoliday)
  
  test$Store<-as.factor(test$Store)
  test$DayOfWeek<-as.factor(test$DayOfWeek)
  test$Open<-as.factor(test$Open)
  test$Promo<-as.factor(test$Promo)
  test$SchoolHoliday<-as.factor(test$SchoolHoliday)


## manage date format (adds year and month column)
  train$Date<-as.Date(as.character(train$Date))
  train$year<-as.factor(format(train$Date, "%Y"))
  train$month<-as.factor(format(train$Date, "%m"))
  
  test$Date<-as.Date(as.character(test$Date))
  test$year<-as.factor(format(test$Date, "%Y"))
  test$month<-as.factor(format(test$Date, "%m"))

## add store info to either train and test
  train<-merge(train, store)
  test<-merge(test, store)


# looking at only stores that were open in the train set
  train <- train[ which(train$Open== "1"),]
  
  
#### Are some data missing? ####
summary(train$StateHoliday) #OK
summary(train$DayOfWeek) #OK
summary(train$Open) #OK
summary(test$Open) #11 NA's
summary(train$year) #OK



#### Some graphics ####

## By school holiday - small difference only
  By_schoolH <- aggregate(train$Sales,by = list(train$SchoolHoliday),function(x){mean(as.numeric(x))})
  names(By_schoolH) <- c("SchoolHoliday","Average.Sales")
  ggplot(data=By_schoolH,aes(x=SchoolHoliday,y=Average.Sales,fill=SchoolHoliday)) + geom_bar(stat="identity") +
    ggtitle("Average sales by school holiday")

## By State holiday - makes no sense (test data only contains "0" and few "a")
  By_stateH <- aggregate(train$Sales,by = list(train$StateHoliday),function(x){mean(as.numeric(x))})
  names(By_stateH) <- c("StateHoliday","Average.Sales")
  ggplot(data=By_stateH,aes(x=StateHoliday,y=Average.Sales,fill=StateHoliday)) + geom_bar(stat="identity") +
    ggtitle("Average sales by State holiday")

## Compare by year
 
  By_Year <- aggregate(train$Sales,by = list(train$year),function(x){mean(as.numeric(x))})
  names(By_Year) <- c("Year","Average.Sales")
  ggplot(data=By_Year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") +
    ggtitle("Average sales by year")

 
## compare only Jan-Jul data
    # Jan-Jul maji nizsi nez prumerny Sales
  JanJul<-train[ which(train$month == "01" | train$month == "02" | train$month == "03" | train$month == "04" | train$month == "05"  | train$month == "06"  | train$month == "07"), ]
  
  By_Year <- aggregate(JanJul$Sales,by = list(JanJul$year),function(x){mean(as.numeric(x))})
  names(By_Year) <- c("Year","Average.Sales")
  ggplot(data=By_Year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") +
    ggtitle("Average sales by year")

  #       year  whole     JanJul
  # 1     2013 6814.392   6702.493
  # 2     2014 7025.404   6877.401
  # 3     2015 7088.127   7088.127
  

  
#### SNIPPETS ####
  cat("combine store and dayOfWeek\n")
  train$StoreANDDayOfWeek <- as.factor(paste(train$Store,train$DayOfWeek,sep="-"))
  test$StoreANDDayOfWeek <- as.factor(paste(test$Store,test$DayOfWeek,sep="-"))
  
  cat("find median sales per store per dayOfWeek\n")
  medians <- tapply(train$Sales, train$StoreANDDayOfWeek, median)
  
  cat("make predictions\n")
  pred <- medians[as.character(test$StoreANDDayOfWeek)]
  submission <- data.frame(Id=test$Id, Sales=pred)
  
  write_csv(submission, "submission.csv")

  
#### Snippets muj ####
  JanJul$newCat<-as.factor(paste(JanJul$Store, JanJul$DayOfWeek, JanJul$Promo, sep="-"))
  test$newCat <- as.factor(paste(test$Store, test$DayOfWeek, test$Promo, sep="-"))
  
  means<-tapply(JanJul$Sales, JanJul$newCat, mean)
  
  predA <- means[as.character(test$newCat)]
  
  #vynasobit vsechny trzby nejakou konstantou
  
      JJ2013<-6702.493
      JJ2014<-6877.401
      JJ2015<-7088.127
      
      konst<-JJ2015/((JJ2013+JJ2014)/2)
      konst #1.043915
  
      pred<-predA*konst
      
  test$Sales2<-pred
  
  #prepsat open=0 na sales = 0
  test$Sales2[with(test, Open == "0")] <- 0
  
  #chybejici udaje?
  
  submission <- data.frame(Id=test$Id, Sales=test$Sales2)
  
  write.csv(submission, "submission_2015_10_18_01.csv")
  
#### TODO ####
  
  # clustry na store typy?
  # konstanta sledujici pomer prazdnin v celem roce
  