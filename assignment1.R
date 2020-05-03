
train.data<-read.csv(file.choose(), header=TRUE, sep=",") #load the train data into the train.data dataframe
test.data<-read.csv(file.choose(), header=TRUE, sep=",") #load the test data into the test dataframe

#observe data
str(train.data)
head(train.data,4)
tail(train.data,4)

#remove outliers
train.data <- train.data[-(c(9000,9009,9010)),]

#####################################################
#                  ATTEMPT 1                        #
#            finding baseline score                 #
#####################################################

#use the current variables to see what kind of prediction we get. kind of like establishing the baseline
fit<-lm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, train.data)

#find outliers
plot(fit)

predicted.count<- predict(fit,test.data)

write.csv(predicted.count, file = "PredictCount.csv")


#####################################################
#                  ATTEMPT 2                        #
#      Using Log & exp functions to fix error       #
#####################################################


#initial submission failed due to negative counts, added log function to avoid negatives
fit1<-lm(log(count) ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, train.data)
plot(fit1)

#used exp function to bring log numbers to actual predictions
predicted.count<- exp(predict(fit1,test.data))

write.csv(predicted.count, file = "PredictCount1.csv")


#####################################################
#                  ATTEMPT 3                        #
#                 using Lasso                       #
#####################################################

#use Lasso

library(glmnet)

y<-log(train.data$count)
X<-model.matrix(datetime~
                  temp*season*weather
                + atemp*season*weather
                + season*weather
                + windspeed*season*weather
                + holiday
                + workingday
                , combine.data)[,-1]
X<-cbind(combine.data$datetime,X)


X.prediction<-subset(X,X[,1]>=10888)

lasso.fit <-glmnet(x = X, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")


#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-8.5,-6),ylim=c(0.006,0.008)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# comparing the performance on the testing set, LASSO is better, so use it for prediction
predicted.count <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
write.csv(predicted.count, file = "PreditCount2.csv") # export the predicted prices into a CSV file


#####################################################
#                  ATTEMPT 4                        #
#              feature engineering                  #
#####################################################

#linear regression with feature engineering

#variables were in integer, converted them into factors
train.data$season <- factor(train.data$season)
train.data$weather<- factor(train.data$weather)

test.data$season <- factor(test.data$season)
test.data$weather<- factor(test.data$weather)


#separating datetime to use as variables for prediction
date <- ymd_hms(train.data$datetime)
train.data$hour <- factor(hour(date))
train.data$day <- day(date)
train.data$weekday <- wday(date, label = TRUE)
train.data$month <- factor(month(date))
train.data$year <-factor(year(date))


date <- ymd_hms(test.data$datetime)
test.data$hour <- factor(hour(date))
test.data$day <- day(date)
test.data$weekday <- wday(date, label = TRUE)
test.data$month <- factor(month(date))
test.data$year <-factor(year(date))


fit2<-lm(log(count) ~ weekday*hour*workingday +
                      weekday*workingday +
                      weekday*
                      hour*workingday +
                      weather*weekday*hour +
                      weather*weekday + 
                      weather*hour*workingday +
                      season*hour*workingday +
                      season + holiday + workingday + weather + temp + atemp + humidity + windspeed, train.data)
plot(fit2)


predicted.count<- exp(predict(fit2,test.data))

final <- data.frame(
  datetime = test.data$datetime,
  count = predicted.count
)

write.csv(final, file = "PredictCount3.csv")






























