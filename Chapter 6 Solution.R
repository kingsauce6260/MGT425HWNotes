## prob 1

library(forecast)
library(leaps)
library(gains)
library(caret)
library(dplyr)

housing.df <- read.csv("BostonHousing.csv")

head(housing.df)



# b

reg <- lm(MEDV ~ CRIM + CHAS + RM, data = housing.df)

summary(reg)



# c

reg$coef %*% c(1, 0.1, 0, 6)



# d

cor(housing.df[,c("CRIM","CHAS","RM")], housing.df)

cor(housing.df[,c("INDUS", "NOX", "TAX")])

cor(housing.df)



library(leaps)

set.seed(1)

train.index <- sample(c(1:dim(housing.df)[1]),

                      0.6*dim(housing.df)[1])

valid.index <- setdiff(c(1:dim(housing.df)[1]), train.index)

train.df <- housing.df[train.index, ]

valid.df <- housing.df[valid.index, ]





search <- regsubsets(MEDV ~ .,

                     data = housing.df,

                     nbest = 1,

                     nvmax = dim(train.df)[2],

                     method = "exhaustive")

sum <- summary(search)

sum$which

sum$adjr2



# top 3 models

models <-  order(sum$adjr2, decreasing = T)[1:3]



library(gains)


# run model on training and validation



par(mfcol=c(1,3))

for (model in models){

  print(model)

  selected.vars = names(train.df)[sum$which[model,]]

  reg.model <- lm(MEDV ~ ., data = train.df[,selected.vars])



  # training

  print(accuracy(reg.model$fitted.values, train.df$MEDV)[2])



  # validation

  pred <- predict(reg.model, valid.df)

  print(accuracy(pred, valid.df$MEDV)[2])



  gain <- gains(valid.df$MEDV, pred)

  plot(c(0, gain$cume.pct.of.total*sum(valid.df$CRIM)) ~

         c(0, gain$cume.obs),

       xlab="# cases", ylab="Cumulative", main="", type="l")

}





reg2 <- lm(MEDV ~ ., data = housing.df)

s <- step(reg2)

summary(s)


Version 2
###6.1 d iii
housing.lm <- lm(MEDV ~., data = train.df)
boston.lm.step<-step(housing.lm,direction="backward")

summary(boston.lm.step)

boston.lm.step.pred<-predict(boston.lm.step,valid.df)

accuracy(boston.lm.step.pred,valid.df$MEDV)

#mean error=0.1644297 RMSE=4.049972 MAPE=15.59571

boston.lm.step2<-step(housing.lm,direction="forward")

summary(boston.lm.step2)

boston.lm.step.pred2<-predict(boston.lm.step2,valid.df)

accuracy(boston.lm.step.pred2,valid.df$MEDV)

#mean error=0.1431813 RMSE=4.048066 MAPE=15.60664

boston.lm.step3<-step(housing.lm,direction="both")

summary(boston.lm.step3)

boston.lm.step.pred3<-predict(boston.lm.step3,valid.df)

accuracy(boston.lm.step.pred3,valid.df$MEDV)

#mean error=0.1644297 RMSE=4.049972 MAPE=15.59571

training<-sample(housing.df$MEDV,354) #randomly generate training and validation sets

validation<-sample(setdiff(housing.df$MEDV,training),152)

reg<-lm(MEDV~.,data=housing.df[,-c(4,14)],subset=training,na.action=na.exclude) #run linear regression model

pred_t<-predict(reg,na.action=na.pass)

pred_v<-predict(reg,newdata=housing.df[validation,-c(4,14)],na.action=na.pass)

library(gains)

gain<-gains(housing.df[validation,]$MEDV[!is.na(pred_v)],pred_v[!is.na(pred_v)])

options(scipen=999) #cumulative lift chart

MEDV<-housing.df[validation,]$MEDV[!is.na(housing.df[validation,]$MEDV)]#computing the gain relative to MEDV

plot(c(0,gain$cume.pct.of.total*sum(MEDV))~c(0,gain$cume.obs),xlab="% of data sets",ylab="Lift",main="Lift Chart",type="l")

lines(c(0,sum(MEDV))~c(0,dim(housing.df[validation,])[1]),col="gray",lty=2)#baseline





## prob 2



tayko.df <- read.csv("Tayko.csv")

head(tayko.df)

summary(tayko.df)



# variables

FREQ

LAST UPDATE

WEB

GENDER

ADDRESS RES

ADDRESS US



# a

aggregate(tayko.df$Spending, list(tayko.df$Gender.male), sd)

aggregate(tayko.df$Spending, list(tayko.df$Gender.male), mean)

aggregate(tayko.df$Spending, list(tayko.df$Web.order), mean)

aggregate(tayko.df$Spending, list(tayko.df$Address_is_res), mean)

aggregate(tayko.df$Spending, list(tayko.df$US), mean)



# b

plot(tayko.df$Freq ~ tayko.df$Spending)

plot(tayko.df$last_update_days_ago ~ tayko.df$Spending)





set.seed(1)

train.index <- sample(c(1:dim(tayko.df)[1]),

                      0.6*dim(tayko.df)[1])

valid.index <- setdiff(c(1:dim(tayko.df)[1]), train.index)

train.df <- tayko.df[train.index, ]

valid.df <- tayko.df[valid.index, ]



reg <- lm(Spending ~ Gender.male +

            Web.order +

            Address_is_res +

            US +

            Freq +

            last_update_days_ago, data = train.df)

summary(reg)

step(reg, direction = "backward")



pred <- predict(reg, valid.df)

pred[1]

valid.df$Spending[1] - pred[1]



hist(reg$residuals)



