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
    # JanJul$newCat<-as.factor(paste(JanJul$Store, JanJul$DayOfWeek, JanJul$Promo, sep="-"))
    train$newCat<-as.factor(paste(train$Store, train$DayOfWeek,train$Promo, sep="-"))
    test$newCat <- as.factor(paste(test$Store, test$DayOfWeek, test$Promo, sep="-"))

# 2. spocitani prumeru/medianu? JanJul vs cely rok
    #medians<-tapply(JanJul$Sales, JanJul$newCat, median)
    medians<-tapply(train$Sales, train$newCat, median)
    predA <- medians[as.character(test$newCat)]

# 3. vynasobit vsechny trzby nejakou konstantou 
    #0.98         0.13954   (stary model v excelu )
    #1.0          0.14184 sub/test = nasobit mene nez1
    #1.001446365  0.14211 sub9
    #1.043915     0.15633
    
    
    konst<- 0.985
    pred<-predA*konst
    test$Sales2<-pred

# 4. prepsat open=0 na sales = 0
    test$Sales2[with(test, Open == "0")] <- 0


# 5. submission
submission <- data.frame(Id=test$Id, Sales=test$Sales2)
write.csv(submission, "submission_test.csv", row.names=FALSE)

#### poznamky a Kaggle scores ####

#sub11   0.14066  cely rok  mean    zadna konstanta
#sub13   0.14184  JanJul    mean    zadna konstanta

#JanJul median
  #sub15  0.14377 JanJul    median  1,02
  #sub14  0.14075 JanJul    median  1,00
  #sub20  0.14043 JanJul    median  0,995
  #sub18  0.14029 JanJul    median  0,99    #
  #sub21  0.14032 JanJul    median  0,985
  #sub16  0.14054 JanJul    median  0.98
  #sub19  0.14151 JanJul    median  0.97
  #sub17  0.14317 JanJul    median  0.96

#cely rok median
  #sub23  0.14197   cely rok    median  1,02
  #sub12  0.13888   cely rok    median  1.00
  #sub26  0.13841   cely rok    median  0.99  #
  #sub27  0.13844   cely rok    median  0,985
  #sub24  0.13866   cely rok    median  0.98
  #sub25  0.14130   cely rok    median  0.96


#Complete average
#sub22  0.13950 average 
#sub11   0.14066
#sub12   0.13888
#sub13   0.14184
#sub14   0.14075



# Means
JJ2013<-6702.493
JJ2014<-6877.401
JJ2015<-7088.127

AS2013<-6483.755
AS2014<-6735.430

#Medians
JJ2013<-6147
JJ2014<-6301
JJ2015<-6530

AS2013<-5960.5
AS2014<-6186.0