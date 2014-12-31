library(caret)
library(party)

##read training and test data sets

train<-read.csv("train.csv",stringsAsFactor=FALSE)
test<-read.csv("test.csv",stringsAsFactor=FALSE)

a<-createDataPartition(train$count,p = .5,list = FALSE)

training<-train[a,]## training set 
testing<-train[-a,]## testing set made from original training set

training$weather[training$weather==4]<-3
testing$weather[testing$weather==4]<-3
## because weather 4 occurs very rarely 


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

training$sunday_mnday[training$day=="Sunday"]<-1
training$dec<-1

##created jan_feb variable since it shows some usefulness from plot
training$dec[training$month=="December"]<-0
training$sunday<-as.factor(training$sunday)

##created factors of the rest of the variables
training$dec<-as.factor(training$dec)
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
testing$sunday<-0
testing$sunday[testing$day=="Sunday"]<-1
testing$sunday<-as.factor(testing$sunday)
testing$dec<-1
testing$dec[testing$month=="December"]<-0
testing$dec<-as.factor(testing$dec)
testing$season<-as.factor(testing$season)
testing$weather<-as.factor(testing$weather)
testing$workingday<-as.factor(testing$workingday)
testing$holiday<-as.factor(testing$holiday)
testing$hour<-as.factor(testing$hour)
names(testing)

##model fit using ctree,here rpart doesn't give high accuracy

formula<-count~season+hour+workingday+holiday+sunday+hour+dec+atemp+humidity+windspeed
fit.ctree <- ctree(formula, data=training)
predict.ctree <- predict(fit.ctree, testing)
## to see the deviation between predicted and actual values
tests<-predict.ctree-testing$count
plot(tests) ## a plot near 0 means good fit but it 
## doesn't show good fit 1st time maybe some factors aren't necessary
## so lets change the factors a bit

##we haven't looked into the factors casual and registered
##which have different outcomes based on factors
##so we predict them separately and add them to find total count
##and see if the plot is nicer

formula<-registered~season+hour+workingday+holiday+sunday+hour+dec+atemp+humidity+windspeed
fit.ctree <- ctree(formula, data=training)
predict.ctree <- predict(fit.ctree, testing)

formula1<-casual~season+hour+workingday+holiday+sunday+hour+dec+atemp+humidity+windspeed
fit.ctree1 <- ctree(formula1, data=training)
predict.ctree1 <- predict(fit.ctree1, testing)
##final count
prediction<-predict.ctree+predict.ctree1
plot(prediction-testing$count)
##this gives a much better fit
##lets put this up in kaggle

df<-data.frame("datetime"=test[,1],"count"=prediction)
colnames(df)<-c("datetime","count")
write.csv(df,"bike6ctree.csv",row.names=FALSE)