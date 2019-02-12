#Reading and Selecting Variables for Regression
car.df <- read.csv("ToyotaCorolla.csv")

#use first 1000 rows of data
car.df <- car.df[1:1000,]

#Select variables for regression
selected.var <- c(3,4,7,8,10,12,13,14,17,18)

#Partitioning data
set.seed(1) #set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

##USE LINEAR REGRESSION
#Use lm() to run a linear regression of Price on all
#11 predictors in the training set
#use . after ~ to include all the remaining columns
#in the train.df as predictors
car.lm <- lm(Price ~., data = train.df)
#Use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(car.lm)
#has to be lower than 0.05 to be relevant

##PREDICTION FUNCTION
library(forecast)
#use predict() to make predictions on a new set
car.lm.pred <- predict(car.lm, valid.df)
options(scipen = 999, digits = 3)
#use accuracy() to compute common accuracy measures; difference between actual and predicted
accuracy(car.lm.pred, valid.df$Price)
#have to take these numbers and compare them to another model
#Trying to get the lowest RSME number possible
#Takes actual minus predicted and square it
#It is based on the unit of study such as in this model Price

#Histogram
library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

#EXHUASTIVE SEARCH(finding best Xi)
#use regsubsets() in package leaps to run an exhaustive search
#unlike with lm, categorical predictors must be turned into dummies manually
library(leaps)
#dim(train.df)[2] get the 11 predictors
search <- regsubsets(Price~., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

#show models
sum$which
#using the metrics below where the point stops increasing significantly you choose
#that model # in this case it is seven so go to row seven if it is true use if false then don't

#show metrics
sum$rsq
#where the point it increases without significant improvement stop
sum$adjr2
sum$cp

##More Traditional approach with stepwise regression
#use step() to run a stepwise regression
#set directions = to either "backward", "forward", or "both"
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)
#which variables did it drop?

car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
