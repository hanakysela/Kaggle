## source the old basic script
source("Kaggle_Rossmann_basic.R")

## compare only Jan-Jul data
      # Jan-Jul maji nizsi nez prumerny Sales
      JanJul<-train[ which(train$month == "01" | train$month == "02" | train$month == "03" | train$month == "04" | train$month == "05"  | train$month == "06"  | train$month == "07"), ]
      
      AugSep<-train[ which(train$month == "08" | train$month == "09"), ]
      
      By_Year <- aggregate(train$Sales,by = list(train$year),function(x){median(as.numeric(x))})
      names(By_Year) <- c("Year","Average.Sales")
      a<-mean(train$Sales)
      
#       ggplot(data=By_Year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") +
#       ggtitle("Average sales by year")

# MEANS      
      #   year  whole     JanJul      AugSep
      #   2013 6814.392   6702.493    6483.755
      #   2014 7025.404   6877.401    6735.430
      #   2015 7088.127   7088.127
      #   all  6955.514   6889.573

# MEDIANS
      #   year    whole   JanJul    AugSep
      #   2013    6218    6147      5960.5
      #   2014    6427    6301      6186.0
      #   2015    6530    6530

      
#### Submission_2015_10_18_01 ####
# 1. shluknuti podle kategorii
    JanJul$newCat<-as.factor(paste(JanJul$Store, JanJul$DayOfWeek, JanJul$Promo, sep="-"))
    test$newCat <- as.factor(paste(test$Store, test$DayOfWeek, test$Promo, sep="-"))

# 2. spocitani prumeru/medianu?    
    means<-tapply(JanJul$Sales, JanJul$newCat, mean)
    predA <- means[as.character(test$newCat)]

# 3. vynasobit vsechny trzby nejakou konstantou 
    #1.0          0.14184 sub/test = nasobit mene nez1
    #1.001446365  0.14211 sub9
    #1.043915     0.15633

    JJ2013<-6702.493
    JJ2014<-6877.401
    JJ2015<-7088.127
    
    AS2013<-6483.755
    AS2014<-6735.430
    
    #konst<-JJ2015/((JJ2013+JJ2014)/2)
     konst #1.001446365
    
    pred<-predA*konst
    
    test$Sales2<-pred

# 4. prepsat open=0 na sales = 0
    test$Sales2[with(test, Open == "0")] <- 0


# 5. submission
submission <- data.frame(Id=test$Id, Sales=test$Sales2)
write.csv(submission, "submission_test.csv", row.names=FALSE)