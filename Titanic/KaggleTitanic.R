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
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(RCurl, rpart, rattle, rpart.plot, RColorBrewer, party)
  

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
      
  
      
#### MODEL 2 - guess age (resp. Child status): ####
  
      train$Child <- "NA"
      train$Child[train$Age < 81] <- "Adult"
      train$Child[train$Age < 18] <- "Children"
      train$Child[train$Age < 7] <- "Small"
      train$Child <-as.factor(train$Child)
        a<-train$Child
      
      test$Child <- "NA"
      test$Child[test$Age < 81] <- "Adult"
      test$Child[test$Age < 18] <- "Children"
      test$Child[test$Age < 7] <- "Small"
      test$Child <- as.factor(test$Child)
        b<-test$Child
      
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
  
      
      
      
#### MODEL 2 - GUESSING THE AGE OF PERSON ####      
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
      
 
  train_knowage<-train[complete.cases(train[ ,5:6]),] #I know age
  train_dontknowage <- train[(train$Child)== "NA", ] # NA in Child column
  train_dontknowage$Child[train_dontknowage$SibSp <4] <- "Adult"
  train_dontknowage$Child[train_dontknowage$SibSp >3] <- "Children"
  train_age<-rbind(train_knowage, train_dontknowage)
  train_age<-train_age[order(train_age$PassengerId),]
 
  
  test_knowage<-test[complete.cases(test[ ,5:6]),] #I know age
  test_dontknowage <- test[(test$Child)== "NA", ] # NA in Child column
  test_dontknowage$Child[test_dontknowage$SibSp <4] <- "Adult"
  test_dontknowage$Child[test_dontknowage$SibSp >3] <- "Children"
  test_age<-rbind(test_knowage, test_dontknowage)
  test_age<-test_age[order(test_age$PassengerId),]
   
#---------nahore je ta myslenka - preklasifikovat trees podle tohoto noveho train_age datasetu.
  # Child je doplneno, tak by se mohl tree lepe ucit.
  
  
 # Child-Age adjusted model - NOT WORKING
  ACH_fit_01 <- rpart(Survived ~ Pclass + Sex + Age + SibSp, data=train_03, method="class")
  ACH_fit_02 <- rpart(Survived ~ Pclass + Sex, data=train_03, method="class")
  # ACH_fit_03 <- rpart(Survived ~ Pclass + Sex + Age, data=train_03, method="class")
  # ACH_fit_04 <- rpart(Survived ~ Pclass + Sex + Child, data=train_03, method="class")
  # ACH_fit_05 <- rpart(Survived ~ Pclass + Sex + Child + SibSp, data=train_03, method="class")
  # ACH_fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare, data=train_03, method="class")
  ACH_fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch, data=train_03, method="class")
  # ACH_fit_08 <- rpart(Survived ~ Pclass + Sex + Fare, data=train_03, method="class")
  # ACH_fit_09 <- rpart(Survived ~ Pclass + Sex + Fare + Age, data=train_03, method="class")
  ACH_fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch, data=train_03, method="class")
  
  
  
#### MODEL 2 - FIT TREES
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
  
  
  
  
#### Plotting trees - OLD MODELS####
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
  
  
#### PLOTTING THE TREES NEW MODELS ####  
  fancyRpartPlot(ACH_fit_01)
  fancyRpartPlot(ACH_fit_02)
  fancyRpartPlot(ACH_fit_03)
  fancyRpartPlot(ACH_fit_04)
  fancyRpartPlot(ACH_fit_05)
  fancyRpartPlot(ACH_fit_06)
  fancyRpartPlot(ACH_fit_07)
  fancyRpartPlot(ACH_fit_08)
  fancyRpartPlot(ACH_fit_09)
  fancyRpartPlot(ACH_fit_10)
  fancyRpartPlot(age2_fit_06, cex=0.65)
  
  fancyRpartPlot(age2t_fit_06)
  fancyRpartPlot(age2t_fit_07)
  fancyRpartPlot(age2t_fit_10)

 
  
  
  age_fit_01 <- rpart(Survived ~ Pclass + Sex + Age + SibSp, data=train_age, method="class")
  age_fit_02 <- rpart(Survived ~ Pclass + Sex, data=train_age, method="class")
  age_fit_03 <- rpart(Survived ~ Pclass + Sex + Age, data=train_age, method="class")
  age_fit_04 <- rpart(Survived ~ Pclass + Sex + Child, data=train_age, method="class")
  age_fit_05 <- rpart(Survived ~ Pclass + Sex + Child + SibSp, data=train_age, method="class")
  age_fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare, data=train_age, method="class")
  age_fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch, data=train_age, method="class")
  age_fit_08 <- rpart(Survived ~ Pclass + Sex + Fare, data=train_age, method="class")
  age_fit_09 <- rpart(Survived ~ Pclass + Sex + Fare + Age, data=train_age, method="class")
  age_fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch, data=train_age, method="class")
  
#### KAGGLE SCORES ####
  # Prediction_02(Survived ~ Pclass + Sex)    ->                              0.76555
  # Prediction_08(Survived ~ Pclass + Sex + Fare) ->                          0.78947 
  # Prediction_03(Survived ~ Pclass + Sex + Age ->                            0.73684
  # Prediction_04(Survived ~ Pclass + Sex + Child) ->                         0.75598
  # Prediction_01(Survived ~ Pclass + Sex + Age + SibSp) ->                   0.75598 
  # Prediction_05(Survived ~ Pclass + Sex + Child + SibSp) ->                 0.75120
  # Prediction_09(Survived ~ Pclass + Sex + Age + Fare) ->                    0.78469 
  # Prediction_06(Survived ~ Pclass + Sex + Child + Fare) ->                  0.78469 #
  # Prediction_07(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch) ->  0.79426 #
  # Prediction_10(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch) ->    0.79426 
  
      # ACH_Prediction_01(Survived ~ Pclass + Sex + Age + SibSp) ->               0.56459
      # ACH_Prediction_07(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch) 0.55981
      # ACH_Prediction_10(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch) ->0.56459  
      # ACH_Prediction_02(Survived ~ Pclass + Sex)    ->                          0.58373
      
  # Age_Prediction_02(Survived ~ Pclass + Sex) ->                             0.76555
  # Age_Prediction_08(Survived ~ Pclass + Sex + Fare) ->                      0.78947
  # Age_Prediction_06(Survived ~ Pclass + Sex + Child + Fare) ->              0.78469    # 
  # Age_Prediction_07(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch)->0.79426  #
  
  
  # Age2_Prediction_06(Survived ~ Pclass + Sex + Child + Fare) ->             0.77990 !!
  # Age2_Prediciton_07(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch-> 0.79904 ####
  # age2_Prediction_10(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch) ->0.79426  #
  
  
  # age2f25_fit_06(Survived ~ Pclass + Sex + Child + Fare2,                   0.77990
  # age2f25_fit_07(Survived ~ Pclass + Sex + Child + Fare2 + SibSp + Parch    0.78947
  # age2f25_fit_10(Survived ~ Pclass + Sex + Age + Fare2 + SibSp + Parch,     0.77990
  
  # combining fare into levels by 25 does not improve anything
  
  
  # age2t_fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare + Title                  0.79426
  # age2t_fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch + Title  0.79426
  # age2t_fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch + Title    0.80383
  

  
  
#### write to the submission file ####

  

#### MODEL 3 - only 2 categories of age ####
  train$Child <- "NA"
  train$Child[train$Age < 81] <- "Adult"
  train$Child[train$Age < 15] <- "Children"
  train$Child <-as.factor(train$Child)
 
  test$Child <- "NA"
  test$Child[test$Age < 81] <- "Adult"
  test$Child[test$Age < 15] <- "Children"
  test$Child <- as.factor(test$Child)
  b<-test$Child
  
  
  train_knowage<-train[complete.cases(train[ ,5:6]),] #I know age
  train_dontknowage <- train[(train$Child)== "NA", ] # NA in Child column
  train_dontknowage$Child[train_dontknowage$SibSp <4] <- "Adult"
  train_dontknowage$Child[train_dontknowage$SibSp >3] <- "Children"
  train_age<-rbind(train_knowage, train_dontknowage)
  train_age<-train_age[order(train_age$PassengerId),]
  
  
  test_knowage<-test[complete.cases(test[ ,5:6]),] #I know age
  test_dontknowage <- test[(test$Child)== "NA", ] # NA in Child column
  test_dontknowage$Child[test_dontknowage$SibSp <4] <- "Adult"
  test_dontknowage$Child[test_dontknowage$SibSp >3] <- "Children"
  test_age<-rbind(test_knowage, test_dontknowage)
  test_age<-test_age[order(test_age$PassengerId),]
  
  age2_fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare, data=train_age, method="class")
  age2_fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch, data=train_age, method="class")
  age2_fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch, data=train_age, method="class")

#### MODEL 4 (builds on model 3) fare as factor - not an improvement though ####
  
  train_age$Fare2 <- "expensive"
  train_age$Fare2[train$Fare < 25] <- "cheap"
  train_age$Fare2<-as.factor(train_age$Fare2)
  test_age$Fare2 <- "expensive"
  test_age$Fare2[test$Fare < 25] <- "cheap"
  test_age$Fare2<-as.factor(test_age$Fare2)
  
  age2f25_fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare2, data=train_age, method="class")
  age2f25_fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare2 + SibSp + Parch, data=train_age, method="class")
  age2f25_fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare2 + SibSp + Parch, data=train_age, method="class")

#### MODEL 5 (builds on model 3) extracting titles
  train_age$Name <-as.character(train_age$Name)
  train_age$Title <- sapply(train_age$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  train_age$Title <- sub(' ', '', train_age$Title)
  train_age$Title[train_age$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  train_age$Title[train_age$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  train_age$Title[train_age$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  train_age$Title <- factor(train_age$Title)
  
  test_age$Name <-as.character(test_age$Name)
  test_age$Title <- sapply(test_age$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  test_age$Title <- sub(' ', '', test_age$Title)
  test_age$Title[test_age$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
  test_age$Title[test_age$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  test_age$Title[test_age$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  test_age$Title <- factor(test_age$Title)
  
  
  
  age2t_fit_06 <- rpart(Survived ~ Pclass + Sex + Child + Fare + Title, data=train_age, method="class")
  age2t_fit_07 <- rpart(Survived ~ Pclass + Sex + Child + Fare + SibSp + Parch + Title, data=train_age, method="class")
  age2t_fit_10 <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch + Title, data=train_age, method="class")
  

#### MODEL 6 - RANDOM FOREST BASED ON DATA FROM MODEL 5 ####
  
  ## NEFUNGUJE ####
  
  # ===========================================================
  # origos:set.seed(415)
  fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                 data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
  
  Prediction <- predict(fit, test, OOB=TRUE, type = "response")
  
  
  
#### COMMENTS ####

  Age2t_Prediction_07<- predict(age2t_fit_07, test_age, type = "class")
  submit <- data.frame(PassengerId = test_age$PassengerId, Survived = Age2t_Prediction_07)
  write.csv(submit, file = "Age2t_Prediction_07.csv", row.names = FALSE)
  
 