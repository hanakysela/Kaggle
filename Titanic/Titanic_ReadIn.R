Titanic_ReadIn.R

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
pacman::p_load(RCurl, rpart, rattle, rpart.plot, RColorBrewer)


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