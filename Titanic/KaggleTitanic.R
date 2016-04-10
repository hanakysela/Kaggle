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
      Dell_WD <- "C:/Users/Hana_Weka/Git/Kaggle/Titanic"
      
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

    train$Survived <- as.factor(train$Survived)
    train$Pclass <-as.factor(train$Pclass)
    
    
    # What about N/A's??
  
#### 3. data viz ####♥ 
  fit_01 <- rpart(Survived ~ Pclass + Sex + Age + SibSp, data=train, method="class")
  fit_02 <- rpart(Survived ~ Pclass + Sex, data=train, method="class")
  
  
  fancyRpartPlot(fit_01) # nice tree
  fancyRpartPlot(fit_02)
  
  
  
# very first submission
  Prediction_01<- predict(fit_01, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction_01)
  write.csv(submit, file = "prediction_01.csv", row.names = FALSE)
  
  
  
  
# Prediction_01(Survived ~ Pclass + Sex + Age + SibSp) -> 0.75598
# Prediction_02(Survived ~ Pclass + Sex)    ->            0.76555