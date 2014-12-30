library(caret)
library(party)

##read training and test data sets

training<-read.csv("train.csv",stringsAsFactor=FALSE)
testing<-read.csv("test.csv",,stringsAsFactor=FALSE)

 ##a<-createDataPartition(train$count,p = .5,list = FALSE)

##training<-train[a,]## training set 
##testing<-train[-a,]## testing set made from original training set

## datapartition not working coz in sets like weather 3 factor levels are created 
## instead of 4

time<-strsplit(x = training[,1],split = " ")
hourcombo<-c()
for(i in 1:length(time)){
  hourcombo<-c(hourcombo,strsplit(x = time[[i]][2],split = ":"))
}
hour<-c()
for(i in 1:length(hourcombo)){
  hour<-c(hour,hourcombo[[i]][1])
}
training$hour<-hour ## extracted hour from training datetime
train_date<-lapply(training$datetime,as.Date)

day<-c()
for(i in 1:length(train_date)){
  day<-c(day,weekdays(train_date[[i]][1]))
}
## extracted day from training datetime
month<-c()
for(i in 1:length(train_date)){
  month<-c(month,months(train_date[[i]][1]))
}
## extracted month from training datetime
training$day<-day
training$month<-month
training$sunday_mnday<-0
##created sunday_mnday variable since it shows some usefulness from plot

training$sunday_mnday[training$day=="Sunday"| training$day=="Monday"]<-1
training$jan_feb<-1

##created jan_feb variable since it shows some usefulness from plot
training$jan_feb[training$month=="January" | training$month=="February"]<-0
training$sunday_mnday<-as.factor(training$sunday_mnday)

##created factors of the rest of the variables
training$jan_feb<-as.factor(training$jan_feb)
training$season<-as.factor(training$season)
training$weather<-as.factor(training$weather)
training$workingday<-as.factor(training$workingday)
training$holiday<-as.factor(training$holiday)
training$hour<-as.factor(training$hour)
names(training)
## for test

time_test<-strsplit(x = testing[,1],split = " ")
hourcombo_test<-c()
for(i in 1:length(time_test)){
  hourcombo_test<-c(hourcombo_test,strsplit(x = time_test[[i]][2],split = ":"))
}
hour_test<-c()
for(i in 1:length(hourcombo_test)){
  hour_test<-c(hour_test,hourcombo_test[[i]][1])
}
testing$hour<-hour_test
day1<-c()
test_date<-lapply(testing$datetime,as.Date)
for(i in 1:length(test_date)){
  day1<-c(day1,weekdays(test_date[[i]][1]))
}
month1<-c()
for(i in 1:length(test_date)){
  month1<-c(month1,months(test_date[[i]][1]))
}
testing$day<-day1
testing$month<-month1
testing$sunday_mnday<-0
testing$sunday_mnday[testing$day=="Sunday"| testing$day=="Monday"]<-1
testing$sunday_mnday<-as.factor(testing$sunday_mnday)
testing$jan_feb<-1
testing$jan_feb[testing$month=="January" | testing$month=="February"]<-0
testing$jan_feb<-as.factor(testing$jan_feb)
testing$season<-as.factor(testing$season)
testing$weather<-as.factor(testing$weather)
testing$workingday<-as.factor(testing$workingday)
testing$holiday<-as.factor(testing$holiday)
testing$hour<-as.factor(testing$hour)
names(testing)

##model fit using ctree,here rpart doesn't give high accuracy

formula<-count~season+hour+workingday+holiday+sunday_mnday+hour+jan_feb+atemp+humidity+windspeed
fit.ctree <- ctree(formula, data=training)
predict.ctree <- predict(fit.ctree, testing)
df<-data.frame("datetime"=test[,1],"count"=predict.ctree)
write.csv(df,"bike6ctree.csv",row.names=FALSE)