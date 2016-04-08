setwd("C:/Users/Weka_Hana/Dropbox/R/Kaggle/Titanic")

train<-read.csv("train.csv")

train.data.file <- "train.csv"

train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)