train<-read.csv("train.csv",stringsAsFactor=FALSE)
test<-read.csv("test.csv",,stringsAsFactor=FALSE)

time<-strsplit(x = train[,1],split = " ")
hourcombo<-c()
for(i in 1:length(time)){
hourcombo<-c(hourcombo,strsplit(x = time[[i]][2],split = ":"))
}
hour<-c()
for(i in 1:length(hourcombo)){
  hour<-c(hour,hourcombo[[i]][1])
  }
train$hour<-hour
train_date<-lapply(train$datetime,as.Date)

day<-c()
for(i in 1:length(train_date)){
   day<-c(day,weekdays(train_date[[i]][1]))
   }
train$day<-day
train$sunday_mnday[train$day=="Sunday"| train$day=="Monday"]<-1

names(train)
## for test

time_test<-strsplit(x = test[,1],split = " ")
hourcombo_test<-c()
for(i in 1:length(time_test)){
  hourcombo_test<-c(hourcombo_test,strsplit(x = time_test[[i]][2],split = ":"))
}
hour_test<-c()
for(i in 1:length(hourcombo_test)){
  hour_test<-c(hour_test,hourcombo_test[[i]][1])
}
test$hour<-hour_test
day1<-c()
test_date<-lapply(test$datetime,as.Date)
for(i in 1:length(test_date)){
  day1<-c(day1,weekdays(test_date[[i]][1]))
}
test$day<-day1
test$sunday_mnday[test$day=="Sunday"| test$day=="Monday"]<-1

names(test)
##linear model
model<-lm(count~season+weather+atemp+humidity+windspeed+sunday_mnday+hour,data = train)
prediction<-predict.lm(model,test)
residual_error<-resid(model)
plot(train$count,residual_error)

## plot shows non linear relation

prediction[prediction<0]<-0
prediction<-ceiling(prediction)
df<-data.frame("datetime"=test[,1],"count"=prediction)
write.csv(df,"bike_sharing1st",row.names=FALSE)



