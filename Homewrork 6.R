##6.1
housing.df <- read.csv("BostonHousing.csv")
head(housing.df)
library(forecast)
library(leaps)
library(gains)
library(caret)
library(dplyr)
##a.)
#The data set should be seperated into training and validation sets
#so you are able to build a model based off of the training data set then
#validate the model you choose with the validation data.

##b.)


#partition data
set.seed(1)
train.index <- sample(1:nrow(housing.df), 0.7*nrow(housing.df))
train.df <- housing.df[train.index, ]
valid.df <- housing.df[-train.index, ]
options(decimal = 3, scipen = 999)

#lm()
housing.lm <- lm(MEDV ~ CRIM + CHAS + RM, data = housing.df)
summary(housing.lm)
#Equation: ŷi= -28.81068 -0.26072x + 3.76304x + 8.27818x + 6.17^2######
#ε∼N(0, σ̂2)
pred_t <- predict(housing.lm)
pred_v <- predict(housing.lm, valid.df)



## c.)*****Question
#multiplication as a method betweeen percentage
coef <- coef(housing.lm)
#>> place coefficient values for x1, x2, x3, and x4 based on Part b
housing.lm$coefficients %*% c(1, 0.1, 0, 6)
#20.69916

##d.)
###i.& ii.)
##Reduce the number of predictors
##At first glance I believe the DIS variable will share a relationship between RAD due to going off the
##assumption that if you're near an employement center it is usually close to a highway.
##I am unsure if TAX is important because it is a dependent varibale because it is being dependent on
## the square feet per house or acres in lawn.
#In question iii.) I ran a backwards stepwise regression to see what variables were dropped


#Trying to decipher the relationship betweeen INDUS, NOX, TAX I came up with that all three a strong positive
#correlation with each other.
cor(housing.df[,c("CRIM", "CHAS", "RM")], housing.df)
cor(housing.df[,c("INDUS", "NOX", "TAX")])
cor(housing.df)
format(round(cor(housing.df), 3), nsmall = 3)
#The chart shows that NOX, INDUS, and TAX are all highly positively correlated and I would be able to
#use only one of those because they are all positively correlated. Also my assumption with variables
#DIS and RAD were wrong.
#Other highly positively correlated variables(0.7+): NOX & AGE, RAD & TAX, CAT..MEDV & MEDV
#Other highly negatively correlated variables(-0.7+): DIS & INDUS, DIS & NOX, DIS & AGE, LSTAT & MEDV

#Going off of this I would remove AGE because NOX is highly positively correlated to AGE.
#Going off of this I would remove RAD because TAX is highly positively correlated to RAD.
#I would remove NOX and TAX and keep INDUS because INDUS is highly correlated with NOX and TAX.





#iii.)
#EXHUASTIVE SEARCH(finding best Xi)
#use regsubsets() in package leaps to run an exhaustive search
#unlike with lm, categorical predictors must be turned into dummies manually
search <- regsubsets(MEDV~., data= housing.df, nbest = 1, nvmax = dim(train.df)[2],
                      method = "exhaustive")

sum <- summary(search)
#shows models
sum$which
#show metrics
sum$rsq
#where the point it increases without significant improvement stop
sum$adjr2
sum$cp
#With the eye test I would choose to run run model #6 because it is where it stops decreasing substaintally
#Choosing the top three models usingg the adjr2- side note smaller adjr2 is the better.


#Use stepwise regression to reduce remaining predictors
#Run stepwise on training set
#backward
housing.lm <- lm(MEDV ~., data = train.df)
housing.lm.step <- step(housing.lm, direction = "backward")
options(decimal = 3)
summary(housing.lm.step)
housing.lm.step.pred <- predict(housing.lm.step, valid.df)
a.b <- accuracy(housing.lm.step.pred, valid.df$MEDV)
a.b
#ME = 0.1644297
#RMSE= 4.049972
#MAE = 3.049598
#MPE = -3.027123
#MAPE = 15.59571




#foreward
housing.lm <- lm(MEDV ~., data = train.df)
housing.lm.step <- step(housing.lm, direction = "forward")
summary(housing.lm.step)
housing.lm.step.pred <- predict(housing.lm.step, valid.df)
a.f <- accuracy(housing.lm.step.pred, valid.df$MEDV)
a.f
#ME= 0.1431813
#RMSE= 4.048066
#MAE = 3.051583
#MPE = -3.113282
#MAPE = 15.60664


#both
housing.lm <- lm(MEDV ~., data = train.df)
housing.lm.step <- step(housing.lm, direction = "both")
summary(housing.lm.step)
housing.lm.step.pred <- predict(housing.lm.step, valid.df)
a.bo <- accuracy(housing.lm.step.pred, valid.df$MEDV)
a.bo
#ME = 0.1644297
#RMSE = 4.049972
#MAE = 3.049598
#MPE = -3.027123
#MAPE = 15.59571

#The foreward model seems to be the best model because it has the better ME and RSME.
#However, it does use the most variables which means added complexity. To decide the absolute best model
#I would have to know if they cared more about the complexity or accuracy.


######Question with lift chart

##Lift Chart
##When I was running the code commented out below I kept getting "Error in xj[i] : invalid subscript type 'list'"
#So I just did what you sent me.
#gain <- gains(housing.df[valid.df,]$MEDV[!is.na(pred_v)], pred_v[!is.na(pred_v)])
#options(scipen = 999)
#medv <- housing.df[valid.df,]$MEDV[!is.na(housing.df[valid.df,]$MEDV)]
#plot(c(0,gain$cume.pct.of.total*sum(medv))~c(0, gain$cume.obs),
#      main = "Lift Chart")
#I tried both the one in the book and one from you

training <- sample(housing.df$MEDV, 354)
validation <- sample(setdiff(housing.df$MEDV, training), 152)
reg <- lm(MEDV ~., data = housing.df[,-c(4,14)], subset = training, na.action = na.exclude)
pred_t <- predict(reg, na.action = na.pass)
pred_v <- predict(reg, newdata = housing.df[validation,-c(4,14)], na.action = na.pass)
gain <- gains(housing.df[validation,]$MEDV[!is.na(pred_v)], pred_v[!is.na(pred_v)])
options(scipen = 999)
medv <- housing.df[validation,]$MEDV[!is.na(housing.df[validation,]$MEDV)]
plot(c(0,gain$cume.pct.of.total*sum(medv))~c(0, gain$cume.obs), xlab = "% of data sets", ylab = "Lift",
      main = "Lift Chart", type = "I")
lines(c(0,sum(MEDV))~ c(0, dim(housing.df[validation,])[1]), col("gray", Ity = 2))
####End Question













#6.2
tayko.df <- read.csv("Tayko.csv")

#a.) Pivot Table  ---- Question on how to add average and std dev of spending in each cat to pivot table
pv.df <- data.frame(tayko.df$Web.order, tayko.df$Gender.male, tayko.df$Address_is_res, tayko.df$US,
                    tayko.df$Spending)
pv.df
sd(tayko.df$last_update_days_ago) #Std Dev = 1141.303
summary(tayko.df)
#I don't know how to add the mean spending in each category and std dev to pivot table.
aggregate(tayko.df$Spending, list(tayko.df$Gender.male),sd)
aggregate(tayko.df$Spending, list(tayko.df$Gender.male), mean)
aggregate(tayko.df$Spending, list(tayko.df$Web.order), mean)
aggregate(tayko.df$Spending, list(tayko.df$Address_is_res), mean)
aggregate(tayko.df$Spending, list(tayko.df$US), mean)

#b.)
#Scatterplot Frequency vs. Spending---- unsure if I am doing the line correctly in both plots
plot(tayko.df$Freq, tayko.df$Spending, pch = 1, main = "Spending vs. Frequency",
     xlab = "Spending", ylab = "Frequency") + abline(lm(Spending~Freq, data = tayko.df))
#It seems to have a small linear relationship

#Scatterplot Frequency vs. Spending
plot(tayko.df$last_update_days_ago, tayko.df$Spending, pch = 1, main = "Spending vs. Last Update Days Ago",
     xlab = "Spending", ylab = "Last Update Days Ago") + abline(lm(Spending~last_update_days_ago, data = tayko.df))
#It seems to have a small positive linear relationship




#.c)
#Predictive Model for Spending
#i.)
#Partition the 2000 records into training and validation sets
set.seed(1)
train.index.t <- sample(1:nrow(tayko.df), 0.7*nrow(tayko.df))
train.df.t <- tayko.df[train.index.t,]
valid.df.t <- tayko.df[-train.index.t,]


#ii
#run a multiple linear regression model for Spending vs. all six predictors.
tayko.lm <- lm(Spending ~ Freq + last_update_days_ago + Web.order + Gender.male + Address_is_res + US,
               data = train.df.t)
summary(tayko.lm)
#Estimated predictive equation: #Equation: ŷi= -24.7172178 + 101.4825935x - 0.0006195x + 9.3023191x -
#                                 0.1307348x - 85.0639886x - 2.2318774x 125.6^2  ######

#iii
#The purchaser most likely to spend a large amount of money are high frequency purchasers that the address is residence
#based on them being the top signifigant factors.


#iv
#backward elimination model
tayko.lm.step <- step(tayko.lm, direction = "backward")
summary(tayko.lm.step)
#Gender.male is the first factor dropped



#v --- Queestion ---- How to do prediction error for first purchase
#The prediction error is defined as the difference between its actual outcome value and its predicted outcome value:
#e = y - ŷ
tayko.lm.step.pred <- predict(tayko.lm.step, valid.df.t)
t.a.b <- accuracy(tayko.lm.step.pred, valid.df.t$Spending)
valid.df.t$Spending[1]-tayko.lm.step.pred[1]
#36.99856


#vi
#Evaluate predictive accuracy
t.a.b <- accuracy(tayko.lm.step.pred, valid.df.t$Spending)
t.a.b
#ME= 5.420179
#RMSE= 138.4779
#MAE = 82.49567
#MPE = NaN
#MAPE = Inf




#vii
#Histogram
#They follow a normal distribution
hist(tayko.lm$residuals)

