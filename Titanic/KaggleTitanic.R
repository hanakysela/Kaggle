#2016_04_08

# https://www.kaggle.com/c/titanic SOURCE

# tutorials:

      # http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r
      # https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md


#### 1. packages ####
    install.packages("RCurl") #for reading data from Github
    install.packages("rpart")
    install.packages("rattle")
    install.packages("rpart.plot")
    install.packages("RColorBrewer")
    
    library(RCurl)
    library(rpart)
    library(rattle)
    library(rpart.plot)
    library(RColorBrewer)


  
#### 2. Organize everything ####
    
    #WHAT COMPUTER AM I WORKING FROM?
    
      Lenovo_WD <- "C:/Users/Hana/R/Kaggle/Titanic"
      Dell_WD <- "C:/Users/Weka_Hana/Documents/Git/Kaggle/Titanic"
      
      if(file.exists(Lenovo_WD)){
        setwd(Lenovo_WD)
      } else {
        setwd(Dell_WD)
      }
      
   

    train_url<-getURL("https://raw.githubusercontent.com/hanakysela/Kaggle/master/Titanic/train.csv")
    train<-read.csv(text=train_url)
    
    test_url<-getURL("https://raw.githubusercontent.com/hanakysela/Kaggle/master/Titanic/test.csv")
    test<-read.csv(text=test_url)
    
    str(train)
    str(test)

    train$Survived <- as.factor(train$Survived)
    train$Pclass <-as.factor(train$Pclass)
    
    
    train$Child <- "Adult"
    train$Child[train$Age < 18] <- "Children"
    train$Child[train$Age < 7] <- "Small"
    
    
    test$Child <- "Adult"
    test$Child[test$Age < 18] <- "Children"
    test$Child[test$Age < 7] <- "Small"
    
    train$Child <-as.factor(train$Child)
    test$Child <-as.factor(test$Child)
    
    
    test$Pclass <-as.factor(test$Pclass)
    
    aggregate(Survived ~ Child + Sex + Pclass, data=train, FUN=mean)
    
    
    
    
    # What about N/A's??
  
#### 3. data viz ####
  fit_01 <- rpart(Survived ~ Pclass + Sex + Age + SibSp, data=train, method="class")
  fit_02 <- rpart(Survived ~ Pclass + Sex, data=train, method="class")
  fit_03 <- rpart(Survived ~ Pclass + Sex + Age, data=train, method="class")
  fit_04 <- rpart(Survived ~ Pclass + Sex + Child, data=train, method="class")
  fit_05 <- rpart(Survived ~ Pclass + Sex + Child + SibSp, data=train, method="class")
  fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare, data=train, method="class")
  fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch, data=train, method="class")
  fit_08 <- rpart(Survived ~ Pclass + Sex + Fare, data=train, method="class")
  
  
  fancyRpartPlot(fit_01) # nice tree
  fancyRpartPlot(fit_02)
  fancyRpartPlot(fit_03)
  fancyRpartPlot(fit_04)
  fancyRpartPlot(fit_05)
  fancyRpartPlot(fit_06)
  fancyRpartPlot(fit_07)
  fancyRpartPlot(fit_08)
  
  
# very first submission
  Prediction_08<- predict(fit_08, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction_08)
  write.csv(submit, file = "prediction_08.csv", row.names = FALSE)
  
  
  
  
# Prediction_01(Survived ~ Pclass + Sex + Age + SibSp) ->                   0.75598 
# Prediction_02(Survived ~ Pclass + Sex)    ->                              0.76555 #
# Prediction_03(Survived ~ Pclass + Sex + Age ->                            0.73684
# Prediction_04(Survived ~ Pclass + Sex + Child) ->                         0.75598 
# Prediction_05(Survived ~ Pclass + Sex + Child + SibSp) ->                 0.75120
# Prediction_06(Survived ~ Pclass + Sex + Child + Fare) ->                  0.78469 ##
# Prediction_07(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch) ->  0.79426 ###
# Prediction_08(Survived ~ Pclass + Sex + Fare) ->                          0.78947 (should have been 0,77990)  