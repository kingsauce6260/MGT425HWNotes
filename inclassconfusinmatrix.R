library(forecast)

toyota.corolla.df <- read.csv("ToyotaCorolla.csv")

#randomly generate training and validation
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

#run linear regression model
reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset=training,
          na.action = na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata = toyota.corolla.df[validation, -c(1,2,8,11)],
                 na.action = na.pass)

#Evaluate Performance
accuracy(pred_t,toyota.corolla.df[training,]$Price)
accuracy(pre_v, toyota.corolla.df[validation,]$Price)

##Run Regression on Validation Dataset
#predictions
pred_v <- predict(reg, newdata = toyota.corolla.df[validation, -c(1,2,8,11)])



#load package gains, compute gains
library(gains)
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])

#Run cummulative lift chart
options(scipen = 999)#avoid scientific notation
#we will continue the gain relative to price
price <- toyota.corolla.df[validation,]$Price[!is.na(toyota.corolla.df[validation,]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative Price", main = "Lift Chart", type = "l")

#baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col = "gray", lty=1)

#Decline wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")

#Run classification probability
library(caret)
library(e1071)

owner.df <- read.csv("ownerExample.csv")
confusionMatrix(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner'), owner.df$Class)
confusionMatrix(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner'), owner.df$Class)
confusionMatrix(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner'), owner.df$Class)
