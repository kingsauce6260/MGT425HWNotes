bank.df <-read.csv("UniversalBank.csv")
head(bank.df,9)

#########Question A
## Charts for Education and Income
data.for.plot <- aggregate(bank.df$Income, by = list(bank.df$Education), FUN = mean)
names(data.for.plot) <- c("Education", "Income")

library(ggplot2)
ggplot(bank.df) + geom_point(aes(x = Education, y = Income), colour = "navy", alpha = 0.7)



library(ggplot2)
ggplot(data.for.plot) + geom_bar(aes(x = Education, y = Income), stat = "identity")


ggplot(bank.df) + geom_bar(aes(x = Education, y = Income), stat = "identity")

#####
#Their relationship is the lower the education number the higher the income for education 1. However for 2 & 3 level
#education it is relatively similar.

#Charts for Mortgage and Income
data.for.plot <- aggregate(bank.df$Income, by = list(bank.df$Mortgage), FUN = mean)

ggplot(bank.df) + geom_point(aes(x = Mortgage, y = Income), colour = "navy", alpha = 0.7)


#Seeing the below chart I would look to exclude the people with no mortgage
ggplot(bank.df) + geom_bar(aes(x = Mortgage, y = Income), stat = "identity")

#The relationship here is the lower the mortgage the higher the income.


Correlation

cor(bank.df[,c("Education", "Income")])
cor(bank.df[,c("Mortgage", "Income")])
cor(housing.df)
format(round(cor(bank.df), 3), nsmall = 3)

#b - Experience and Age I would remove




library(forecast)
library(leaps)
library(gains)
library(caret)
library(dplyr)



# c



reg <- lm(Income ~., data = bank.df)

summary(reg)

#PArtition Data
set.seed(1)

train.index <- sample(c(1:dim(bank.df)[1]),

                      0.6*dim(bank.df)[1])

valid.index <- setdiff(c(1:dim(bank.df)[1]), train.index)

train.df <- bank.df[train.index, ]

valid.df <- bank.df[valid.index, ]




#Exhaustive Search
search <- regsubsets(Income ~ .,

                     data = bank.df,

                     nbest = 1,

                     nvmax = dim(train.df)[2],

                     method = "exhaustive")

sum <- summary(search)

sum$which

sum$adjr2



# top 3 models

models <-  order(sum$adjr2, decreasing = T)[1:3]



# run model on training and validation



par(mfcol=c(1,3))

for (model in models){

  print(model)

  selected.vars = names(train.df)[sum$which[model,]]

  reg.model <- lm(Income ~ ., data = train.df[,selected.vars])



  # training

  print(accuracy(reg.model$fitted.values, train.df$Income)[2])



  # validation

  pred <- predict(reg.model, valid.df)

  print(accuracy(pred, valid.df$Income)[2])



}





reg2 <- lm(Income ~ ., data = bank.df)

s <- step(reg2)

summary(s)


#Another Regression Using stepwise
#Use stepwise regression to reduce remaining predictors
#Run stepwise on training set
#backward
bank.lm <- lm(Income ~., data = train.df)
bank.step <- step(bank.lm, direction = "backward")
options(decimal = 3)
summary(bank.step)
bank.pred <- predict(bank.step, valid.df)
a.b <- accuracy(bank.pred, valid.df$Income)
a.b
#ME = 0.1644297
#RMSE= 4.049972
#MAE = 3.049598
#MPE = -3.027123
#MAPE = 15.59571




#foreward
bank.lm <- lm(Income ~., data = train.df)
bank.step <- step(bank.lm, direction = "forward")
options(decimal = 3)
summary(bank.step)
bank.pred <- predict(bank.step, valid.df)
a.f <- accuracy(bank.pred, valid.df$Income)
a.f
#The models perform about the same

bank.lm <- lm(Income ~., data = train.df)
bank.step <- step(bank.lm, direction = "both")
options(decimal = 3)
summary(bank.step)
bank.pred <- predict(bank.step, valid.df)
a.bb <- accuracy(bank.pred, valid.df$Income)
a.bb
#All step models perform relatively the same with about 30 RMSE.


##I do recommend using this model do to the regression model being about 30 RSME off.
