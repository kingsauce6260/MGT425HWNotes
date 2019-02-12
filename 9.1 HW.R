ebaya.df <- read.csv("eBayAuctions.csv")

library(treemap)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ISLR)
library(forecast)
library(leaps)
library(gains)
library(dplyr)

#Data Preprocessing
ebaya.df$Duration <- factor(ebaya.df$Duration)
levels(ebaya.df$Duration)
***#a)
#Data Partitioning
set.seed(1)
train.i <- sample(c(1:dim(ebaya.df)[1]), dim(ebaya.df)[1]*0.6)
train.df <- ebaya.df[train.i,]

valid.df <- ebaya.df[-train.i,]

#classification tree
default.ct <- rpart(Competitive.~., data = train.df, method = "class", minbucket = 50, maxdepth = 7)
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
tree <- prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
tree
summary(default.ct)

Tree Pruning
#argument xval refers to the number of folds to use in rpart's built-in
#cross-validation procedure
#argument cp sets the smallest value for the complexity parameter
cv.ct <- rpart(Competitive.~., data = train.df, method = "class", maxdepth = 7, minbucket = 50)
#use printcp() to print the table
printcp(cv.ct)
#prune by lower cp
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)



***#b No I don't believe it's practical to use for a prediction model due to Close price being measured as the most important
  #variable and if the goal is to predict competitive or non competitive becfore the auction ends then you won't have the
  #close price. If they are trying to predict it after the fact then it is a viable model to be used for prediction.


***#c
  #some interesting information that these rules provide is that the close price is the most important variable but it wouldn't
  #be available if they are trying to predict it before the close of the auction.

***#d - extra credit attempt
ebaya2.df <- read.csv("eBayAuctions.csv")
ebaya.df$Duration <- factor(ebaya.df$Duration)
levels(ebaya.df$Duration)
ebaya2.df <- ebaya2.df[,-c(1,3)]

#Data Partitioning
set.seed(1)
train.i <- sample(c(1:dim(ebaya2.df)[1]), dim(ebaya2.df)[1]*0.6)
train.df <- ebaya2.df[train.i,]
valid.df <- ebaya2.df[-train.i,]

#classification tree
default.ct <- rpart(Competitive.~., data = train.df, method = "class", minbucket = 50, maxdepth = 7)
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

levels(default.ct)

Tree Pruning
#argument xval refers to the number of folds to use in rpart's built-in
#cross-validation procedure
#argument cp sets the smallest value for the complexity parameter
cv.ct <- rpart(Competitive.~., data = train.df, method = "class", maxdepth = 7, minbucket = 50)
#use printcp() to print the table
printcp(cv.ct)
#prune by lower cp
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)
summary(pruned.ct)

#e

ggplot(ebaya2.df) + geom_point(aes(x = ClosePrice, y= OpenPrice, color= Competitive.), alpha = 0.7)


#f
library(gains)
Confusion Matrix and Accuracy MEasures
#classify records in the validation data
#set argument type = "class" in predict() to generate predicted class membership
default.ct.point.pred.train <- predict(default.ct, train.df, type = "class")
levels(default.ct.point.pred.train)
factor(train.df$Competitive.)
levels(train.df$Competitive.)
#generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, train.df$Competitive.)
#repeat the code for the validation set and the deeper tree

