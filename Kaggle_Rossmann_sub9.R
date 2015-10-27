## source the old basic script
source("Kaggle_Rossmann_basic.R")

#### compare only Jan-Jul data ####
      # Jan-Jul maji nizsi nez prumerny Sales
      JanJul<-train[ which(train$month == "01" | train$month == "02" | train$month == "03" | train$month == "04" | train$month == "05"  | train$month == "06"  | train$month == "07"), ]
      
      AugSep<-train[ which(train$month == "08" | train$month == "09"), ]
      
      By_Year <- aggregate(train$Sales,by = list(train$year),function(x){median(as.numeric(x))})
      names(By_Year) <- c("Year","Average.Sales")
      a<-mean(train$Sales)
      
#       ggplot(data=By_Year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") +
#       ggtitle("Average sales by year")

#### MEANS and MEDIANS ####      
      
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
    #JanJul$newCat<-as.factor(paste(JanJul$Store, JanJul$DayOfWeek, JanJul$Promo, sep="-"))
    train$newCat<-as.factor(paste(train$Store, train$DayOfWeek,train$Promo, sep="-"))
    test$newCat <- as.factor(paste(test$Store, test$DayOfWeek, test$Promo, sep="-"))

# 2. spocitani prumeru/medianu? JanJul vs cely rok
    #means<-tapply(JanJul$Sales, JanJul$newCat, mean)
    means<-tapply(train$Sales, train$newCat, mean)
    #medians<-tapply(train$Sales, train$newCat, median)
    predA <- means[as.character(test$newCat)]

# 3. vynasobit vsechny trzby nejakou konstantou 
    #0.98         0.13954   (stary model v excelu )
    #1.0          0.14184 sub/test = nasobit mene nez1
    #1.001446365  0.14211 sub9
    #1.043915     0.15633
    
    
    konst<- 0.965
    pred<-predA*konst
    test$Sales2<-pred

# 4. prepsat open=0 na sales = 0
    test$Sales2[with(test, Open == "0")] <- 0


# 5. submission
submission <- data.frame(Id=test$Id, Sales=test$Sales2)
write.csv(submission, "submission_test.csv", row.names=FALSE)

#### poznamky a Kaggle scores ####


#Cely rok mean
  #sub33  0.14717 cely rok  mean    1.02
  #sub11  0.14066 cely rok  mean    1.00 @
  #sub34  0.13693 cely rok  mean    0.98
  #sub38  0.13647 cely rok  mean    0.975
  #sub37  0.13619 cely rok  mean    0.97
  #sub39  0.13611 cely rok  mean    0.975 ### ###
  #sub35  0.13623 cely rok  mean    0.96
  #sub36  0.13860 cely rok  mean    0.94
  
#JanJul mean
  #sub28  0.14694 JanJul    mean    1.02  
  #sub13  0.14184 JanJul    mean    1.00  @
  #sub29  0.13954 JanJul    mean    0.98
  #sub31  0.13942 JanJul    mean    0.975 ###
  #sub31  0.13949 JanJul    mean    0.97
  #sub30  0.14018 JanJul    mean    0.96


#JanJul median
  #sub15  0.14377 JanJul    median  1,02
  #sub14  0.14075 JanJul    median  1,00    @
  #sub20  0.14043 JanJul    median  0,995
  #sub18  0.14029 JanJul    median  0,99    ###
  #sub21  0.14032 JanJul    median  0,985
  #sub16  0.14054 JanJul    median  0.98
  #sub19  0.14151 JanJul    median  0.97
  #sub17  0.14317 JanJul    median  0.96

#cely rok median
  #sub23  0.14197   cely rok    median  1,02
  #sub12  0.13888   cely rok    median  1.00  @
  #sub26  0.13841   cely rok    median  0.99  ###
  #sub27  0.13844   cely rok    median  0,985
  #sub24  0.13866   cely rok    median  0.98
  #sub25  0.14130   cely rok    median  0.96


#Complete average
  #sub22  0.13950 average 
    #sub11   0.14066
    #sub12   0.13888
    #sub13   0.14184
    #sub14   0.14075

#Average of the best
  #sub40  0.13793
    #sub39  0.13611 cely rok  mean    0.975
    #sub31  0.13942 JanJul    mean    0.975
    #sub18  0.14029 JanJul    median  0,99
    #sub26  0.13841 cely rok  median  0.99  

#average of the best means
  #sub41  0.13741
    #sub39  0.13611 cely rok  mean    0.975
    #sub31  0.13942 JanJul    mean    0.975
    


# vyradit outliery?
# vzit v potaz neco dalsiho (sortiment? promo2?)
# skutecne predikce?