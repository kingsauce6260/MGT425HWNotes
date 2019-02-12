##Homework 10.2 & 10.3
library(ggplot2)
library(gains)
library(caret)
library(class)
library(rpart)

sa.df <- read.csv("SystemAdministrators.csv")
#a
ggplot(sa.df) + geom_point(aes(x = Experience, y = Training, color = Completed.task), alpha = 0.7)


#b
# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic
# regression.

#create reference categories
sa.df$isCompleted <- 1 * (sa.df$Completed.task == "Yes")
sa.reg <- glm(isCompleted ~ Experience + Training, data = sa.df, family = "binomial")
options(scipen=999)
summary(sa.reg)
#b
pred <- predict(sa.reg, sa.df)
confusionMatrix(table(ifelse(pred > .5, 1, 0), sa.df$isCompleted))
9+6
6/15
40% were misclassified

#c
#to decrease percentage in part b should cutoff probability be increased or decreased
pred <- predict(sa.reg, sa.df)

confusionMatrix(table(ifelse(pred > 1, 1, 0), sa.df$isCompleted))
#The cutoff should be decreased

#d ?
-10.9813 = 1.1269x + 0.1805
.405= -10.9813+ 1.1269x + .1805(4)
.1805*4
.722+ -10.9813
.405+10.2593
10.6643/1.1269
9.463395 Amount of Experience needed





#10.3
library(ggplot2)
library(gains)
library(caret)
#a
ml.df <- read.csv("RidingMowers.csv")

ml.df$isOwned <- 1 * (ml.df$Ownership == "Owner")
#Partition the data
set.seed(1)
t.i <- sample(c(1:dim(ml.df)[1]), dim(ml.df)[1]*0.6)
train.df <- ml.df[t.i, ]
valid.df <- ml.df[-t.i, ]

ml.reg <- glm(isOwned ~ Income + Lot_Size, data = train.df, family = "binomial")
options(scipen=999)
summary(ml.reg)


#a
pred <- predict(ml.reg, valid.df)
confusionMatrix(table(ifelse(pred > 0.5, 1, 0), valid.df$isOwned))
#50% were owners of a lawn mower

#b
ggplot(ml.df) + geom_point(aes(x = Income, y = Lot_Size, color = Ownership), alpha = 0.7)
#Owners seem to have higher income

#c percentage of households classified correctly?
#100% (5/5)



#d
confusionMatrix(table(ifelse(pred > 0.5, 1, 0), valid.df$isOwned))
#The cuttoff should be increased to increase percentage of correctly classified owners.



#e
summary(ml.reg)

e^(-22.14181 = 0.08699(60) + 0.85067(20))
0.08699*60
5.2194
0.85067*20
17.0134
22.14181+5.2194+17.0134
=e^44.37461
1.8692E19


#f
It is classified as a nonowner.


#g
0+0.08699x + 0.85067(16)

