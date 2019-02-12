Chapter 6: Regression Analysis
Reading and Selection Variables for Regression
car.df <- read.csv("ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
Let’s start by partitioning your data
# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
Use Linear Regression
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
Let’s run prediction using a Predict() function on a new dataset
library(forecast)
# use predict() to make predictions on a new set.
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)
How good is the prediction? Let’s look at a histogram.
library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
Are your ready to run an exhaustive search? (finding best Xi)
# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
# dim(train.df)[2] get the 11 predictors
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
# show models
sum$which
# show metrics
sum$rsq
sum$adjr2
sum$cp
Are you ready for a more traditional approach with stepwise regression?
  # use step() to run stepwise regression.
  # set directions =  to either "backward", "forward", or "both".
  car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables did it drop?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

