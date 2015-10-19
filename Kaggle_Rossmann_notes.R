#### Some graphics ####

## By school holiday - small difference only
By_schoolH <- aggregate(train$Sales,by = list(train$SchoolHoliday),function(x){mean(as.numeric(x))})
names(By_schoolH) <- c("SchoolHoliday","Average.Sales")
ggplot(data=By_schoolH,aes(x=SchoolHoliday,y=Average.Sales,fill=SchoolHoliday)) + geom_bar(stat="identity") +
  ggtitle("Average sales by school holiday")

## By State holiday - makes no sense (test data only contains "0" and few "a")
By_stateH <- aggregate(train$Sales,by = list(train$StateHoliday),function(x){mean(as.numeric(x))})
names(By_stateH) <- c("StateHoliday","Average.Sales")
ggplot(data=By_stateH,aes(x=StateHoliday,y=Average.Sales,fill=StateHoliday)) + geom_bar(stat="identity") +
  ggtitle("Average sales by State holiday")

## Compare by year

By_Year <- aggregate(train$Sales,by = list(train$year),function(x){mean(as.numeric(x))})
names(By_Year) <- c("Year","Average.Sales")
ggplot(data=By_Year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") +
  ggtitle("Average sales by year")


## compare only Jan-Jul data
# Jan-Jul maji nizsi nez prumerny Sales
JanJul<-train[ which(train$month == "01" | train$month == "02" | train$month == "03" | train$month == "04" | train$month == "05"  | train$month == "06"  | train$month == "07"), ]

By_Year <- aggregate(JanJul$Sales,by = list(JanJul$year),function(x){mean(as.numeric(x))})
names(By_Year) <- c("Year","Average.Sales")
ggplot(data=By_Year,aes(x=Year,y=Average.Sales,fill=Year)) + geom_bar(stat="identity") +
  ggtitle("Average sales by year")

#       year  whole     JanJul
# 1     2013 6814.392   6702.493
# 2     2014 7025.404   6877.401
# 3     2015 7088.127   7088.127



#### SNIPPETS ####
cat("combine store and dayOfWeek\n")
train$StoreANDDayOfWeek <- as.factor(paste(train$Store,train$DayOfWeek,sep="-"))
test$StoreANDDayOfWeek <- as.factor(paste(test$Store,test$DayOfWeek,sep="-"))

cat("find median sales per store per dayOfWeek\n")
medians <- tapply(train$Sales, train$StoreANDDayOfWeek, median)

cat("make predictions\n")
pred <- medians[as.character(test$StoreANDDayOfWeek)]
submission <- data.frame(Id=test$Id, Sales=pred)

write_csv(submission, "submission.csv")



#### TODO ####

# clustry na store typy?
# konstanta sledujici pomer prazdnin v celem roce (JunJul : AugSep)
mean vs median()
