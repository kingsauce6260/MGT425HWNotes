13.1 Homework

library(forecast)
library(leaps)
library(caret)
library(FNN)
library(class)
library(rpart)

#Read data
ub.df <- read.csv("UniversalBank.csv")
ub.df <- ub.df[,-c(1,5)]

#Partition Data
set.seed(1)
t.i <- sample(c(1:dim(ub.df)[1]), 0.6*dim(ub.df)[1])
train.df <- ub.df[t.i,]
valid.df <- ub.df[-t.i,]

#Regression
#create reference categories
#ub.df$isLoaned <- 1 * (ub.df$Personal.Loan == "Yes")
ub.reg <- glm(Personal.Loan ~., data = train.df, family = "binomial")
options(scipen=999)
summary(ub.reg)


confusionMatrix(table(ifelse(ub.reg$fitted.values >0.5, 'Yes', 'No'), valid.df$Personal.Loan))


#KNN
#use preProcess() from the caret package to normalize Income and Lot_Size.

kn <- class::knn(train = train.df[, -8],
                 test = valid.df[,-8],
                 cl = train.df[, 8],
                 k = 3, prob=TRUE)
confusionMatrix(table(kn, valid.df[,8]))


#Decision Tree
tr <- rpart(Personal.Loan ~., data = ub.df)

confusionMatrix(table(ifelse(predict(tr, valid.df)>0.5, 1, 0), valid.df$Personal.Loan))


#b
res<- data.frame(valid.df$Personal.Loan,
                 LogisticProb = predict(ub.reg, valid.df, type = "response"),
                 LogisticPred = ifelse(predict(ub.reg, valid.df, type = "response")>0.5, 1, 0),
                 KNNProb = 1-attr(kn, "prob"),
                 KNNPred = kn,
                 TREEProb = predict(tr, valid.df),
                 TREEPred = ifelse(predict(tr, valid.df)>0.5, 1, 0))
head(res, 10)

#c
res$majority <- rowMeans(data.frame(res$LogisticPred, as.numeric(res$KNNPred),
                                    res$TREEPred))>0.5

res$avg <- rowMeans(data.frame(res$LogisticProb, res$KNNProb, res$TREEProb))


confusionMatrix(table(res$majority * 1, valid.df[,8]))
confusionMatrix(table((res$avg > 0.5)* 1, valid.df[,8]))

#d
Majority Ensemble accuracy rate= 94.45%
Average Ensemble accuracy rate= 97.35%
Logistic Regression accuracy rate= %***Was unable to get Confusion Matrix to Work
KNN accuracy rate= 90.15%
Decision Tree accuracy rate= 98.85%


