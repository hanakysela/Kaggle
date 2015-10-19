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
  #str(train)
  #str(test)

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
  #   summary(train$StateHoliday) #OK
  #   summary(train$DayOfWeek) #OK
  #   summary(train$Open) #OK
  #   summary(test$Open) #11 NA's
  #   summary(train$year) #OK