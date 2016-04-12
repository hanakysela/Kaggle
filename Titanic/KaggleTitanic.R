#2016_04_08

# https://www.kaggle.com/c/titanic SOURCE

# tutorials:

      # http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r
      # https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

  # delete everything
  rm(list=ls(all=TRUE))
  
  # close all open graphics
  graphics.off()


#### 1. packages ####
  
      packages <- c("RCurl", "rpart", "rattle", "rpart.plot", "RColorBrewer")
      
      if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
      }
      
      # RCurl         for reading data from Github
      # rpart         for  tree
      # rattle        
      # rpart.plot
      # RColorBrewer
    
  
#### 2. Organize everything ####
    
  ## WHAT COMPUTER AM I WORKING FROM?
    
      Lenovo_WD <- "C:/Users/Hana/R/Kaggle/Titanic"
      Dell_WD <- "C:/Users/Weka_Hana/Documents/Git/Kaggle/Titanic"
      
      if(file.exists(Lenovo_WD)){
        setwd(Lenovo_WD)
      } else {
        setwd(Dell_WD)
      }
      
   
  ## Read in and set classes
      
      train_url<-getURL("https://raw.githubusercontent.com/hanakysela/Kaggle/master/Titanic/train.csv")
      train<-read.csv(text=train_url)
      
      test_url<-getURL("https://raw.githubusercontent.com/hanakysela/Kaggle/master/Titanic/test.csv")
      test<-read.csv(text=test_url)
  
      train$Survived <- as.factor(train$Survived)
      train$Pclass <-as.factor(train$Pclass)
      test$Pclass <-as.factor(test$Pclass)
    
    
##### 3. Modifications ####
      
  ## Grouping Ages into "Child" variable => 3 levels only
      train$Child <- "Adult"
      train$Child[train$Age < 18] <- "Children"
      train$Child[train$Age < 7] <- "Small"
      
      
      test$Child <- "Adult"
      test$Child[test$Age < 18] <- "Children"
      test$Child[test$Age < 7] <- "Small"
      
      train$Child <-as.factor(train$Child)
      test$Child <-as.factor(test$Child)
    
  
#### MODEL 1 - Age grouped into Child (with 3 levels), assumes missing age=adult ####
      fit_01 <- rpart(Survived ~ Pclass + Sex + Age + SibSp, data=train, method="class")
      fit_02 <- rpart(Survived ~ Pclass + Sex, data=train, method="class")
      fit_03 <- rpart(Survived ~ Pclass + Sex + Age, data=train, method="class")
      fit_04 <- rpart(Survived ~ Pclass + Sex + Child, data=train, method="class")
      fit_05 <- rpart(Survived ~ Pclass + Sex + Child + SibSp, data=train, method="class")
      fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare, data=train, method="class")
      fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch, data=train, method="class")
      fit_08 <- rpart(Survived ~ Pclass + Sex + Fare, data=train, method="class")
      fit_09 <- rpart(Survived ~ Pclass + Sex + Fare + Age, data=train, method="class")
      fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch, data=train, method="class")
      
  
#### MODEL 2 - gess age (resp. Child status):
  
      train$Child <- "NA"
      train$Child[train$Age < 81] <- "Adult"
      train$Child[train$Age < 18] <- "Children"
      train$Child[train$Age < 7] <- "Small"
      train$Child <-as.factor(train$Child)
      
      test$Child <- "NA"
      test$Child[test$Age < 81] <- "Adult"
      test$Child[test$Age < 18] <- "Children"
      test$Child[test$Age < 7] <- "Small"
      test$Child <- as.factor(test$Child)
      
      test_01 <- test
      test_01$Survived <- "NA"
      test_01<-test_01[,c(1, 13, 2:12)]
    
      combined <- rbind(train, test_01)
      combined_01 <- combined[complete.cases(combined[,5:6]),]
      
      summary(test$Child)
      summary(train$Child)
      
      str(train)
      str(test)
      str(combined_01)
      summary(combined_01$Child)
      summary(test_01$Child)
      summary(train$Child)
  
  train$Child[((is.na(train$Child)) & (train$SibSp > 2))] <- "Adult" 
  train$Child[(is.na(train$Child))] <- "nevim"
  
  df$y[is.na(df$y)] = mean(df$y, na.rm=TRUE)
  test_01$Child[is.na(test_01$Child)] <- "nevim"
  
  train_knowage<-train[complete.cases(train[ ,5:6]),] #I know age
  train_dontknowage <- train[(train$Child)== "NA", ] # NA in Child column
  train_dontknowage$Child[train_dontknowage$SibSp <4] <- "Adult"
  train_dontknowage$Child[train_dontknowage$SibSp >5] <- "Children"
  
  train_03<-rbind(train_knowage, train_dontknowage)
#---------nahore je ta myslenka - preklasifikovat trees podle tohoto noveho train_03 datasetu.
  # Child je doplneno, tak by se mohl tree lepe ucit.
  

  CHfit_01<- rpart(Child ~ Pclass + Sex + Fare +SibSp + Parch, data = train, method = "class")
   # show that everyone is Adult, but when SibSp >2,5, they are Children
  CHfit_02<- rpart(Child ~ SibSp + Parch, data = train, method = "class")
   # confirms CHfit_01
  CHfit_03<- rpart(Child ~ Pclass + Sex + Fare + Parch, data = train, method = "class")
   # removing SibSp from the model makes it very complicated -> likely to overfit
  CHfit_04 <- rpart(Child ~ Pclass + Sex + Fare +SibSp + Parch, data = combined, method = "class")
   # very complicated -> overfitting
  CHfit_05 <- rpart(Child ~ Pclass + Sex + Fare +SibSp + Parch, data = combined_01, method = "class")
   # if Parch>0.5 & SibSp > 3.5 ->children
  CHfit_06 <- rpart(Child ~ Pclass +SibSp + Parch, data = combined_01, method = "class")
   # viz CHfit_05
  CHfit_07 <- rpart(Child ~ SibSp, data = combined_01, method = "class")
   # All adults except when SibSp>3,5, then Children
  
  
#### Plotting trees ####
  fancyRpartPlot(fit_01) # nice tree
  fancyRpartPlot(fit_02)
  fancyRpartPlot(fit_03)
  fancyRpartPlot(fit_04)
  fancyRpartPlot(fit_05)
  fancyRpartPlot(fit_06)
  fancyRpartPlot(fit_07)
  fancyRpartPlot(fit_08)
  fancyRpartPlot(fit_09)
  fancyRpartPlot(fit_10)
  fancyRpartPlot(CHfit_01)
  fancyRpartPlot(CHfit_02)
  fancyRpartPlot(CHfit_03)
  fancyRpartPlot(CHfit_04)
  fancyRpartPlot(CHfit_05)
  fancyRpartPlot(CHfit_06)
  fancyRpartPlot(CHfit_07)
  
  
  
#### KAGGLE SCORES ####
  # Prediction_02(Survived ~ Pclass + Sex)    ->                              0.76555 
  # Prediction_08(Survived ~ Pclass + Sex + Fare) ->                          0.78947 ## (should have been 0,77990)
  # Prediction_03(Survived ~ Pclass + Sex + Age ->                            0.73684
  # Prediction_04(Survived ~ Pclass + Sex + Child) ->                         0.75598
  # Prediction_01(Survived ~ Pclass + Sex + Age + SibSp) ->                   0.75598 
  # Prediction_05(Survived ~ Pclass + Sex + Child + SibSp) ->                 0.75120
  # Prediction_09(Survived ~ Pclass + Sex + Age + Fare) ->                    0.78469 #
  # Prediction_06(Survived ~ Pclass + Sex + Child + Fare) ->                  0.78469 #
  # Prediction_07(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch) ->  0.79426 ###
  # Prediction_10(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch) ->    0.79426 ###
  

  
#### write to the submission file ####
  Prediction_10<- predict(fit_10, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction_10)
  write.csv(submit, file = "prediction_10.csv", row.names = FALSE)