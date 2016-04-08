#2016_04_08

# https://www.kaggle.com/c/titanic SOURCE

# tutorials:

      # http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r
      # https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md

#### 1. packages ####
install.packages("rpart")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


  
#### 2. Organize everything ####
  setwd("~/Git/Kaggle/Titanic")

  train<-read.csv("train.csv")

  train$Pclass <-as.factor(train$Pclass)
  
#### 3. data viz ####♥ 
  fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
  
  fancyRpartPlot(fit) # nice tree
  
  # What about N/A's??
  
  
# very first submission
  Prediction <- predict(fit, test, type = "class")
  submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
  write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)