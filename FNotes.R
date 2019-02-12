Thomas Gow Final
taxi.df <- read.csv("Taxi-cancellation-case1.csv")

summary(taxi.df)

A)
For the missing data
cor(as.numeric(taxi.df$to_area_id), as.numeric(taxi.df$to_city_id))
I would leaave out the variable package ID and to city ID and from city ID and use the area instead due to the large amount of
missing data in these variables. I would also leave out all latitude and longitude variables due to it being redundant
of location. For all of the rest of NULL values I would make them equal to the median value in their column.

Since the to area id has a lot less NULLS than to city id and to lat I chose to use this one over them.

str(taxi.df$car_cancellation)

library(ggplot2)
ggplot(taxi.df) + geom_point(aes(x = user_id, y= from_date, color= car_cancellation), alpha = 0.7)


Relationship between travel type id and car cancellation and online booking and car cancellation
ggplot(taxi.df) + geom_point(aes(x = travel_type_id, y= car_cancellation, color= car_cancellation), alpha = 0.7)
ggplot(taxi.df) + geom_point(aes(x = online_booking, y= car_cancellation, color= car_cancellation), alpha = 0.7)
ggplot(taxi.df) + geom_bar(aes(x=travel_type_id, y=car_cancellation), stat = "identity")
ggplot(taxi.df) + geom_bar(aes(x=online_booking, y=car_cancellation), stat = "identity")
-this may show that if it is easier to cancel then more people may do it.
-travel_type_id shows that type 2 has the most cancellations by far compared to type 1 and 2.
-there are most instances car cancellation when booking online




summary(taxi.df)
#Select variables for regression
taxi.df$from_area_id <- as.factor(taxi.df$from_area_id)
#excluded from area id both to and from because new levels error was appearing

selected.var <- c(2,3,5,6,7,10,12,13,14,19)

#Partitioning data
set.seed(1)

train.index <- sample(c(1:dim(taxi.df)[1]), 0.6*dim(taxi.df)[1])

valid.index <- setdiff(c(1:dim(taxi.df)[1]), train.index)

train.df <- taxi.df[train.index, selected.var ]

valid.df <- taxi.df[valid.index, selected.var ]



If thinking of outcome as binary



library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

class.tree<- rpart(car_cancellation~., data = taxi.df, control = rpart.control(maxdepth = 2), method = "class")

#Plot tree
#use prp() to plot the tree. You can control plotting parameters such as color, shaoe,
#and information displayed (which and where)

prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)


Decision Tree


train.index <- sample(c(1:dim(taxi.df)[1]), dim(taxi.df)[1]*0.6)
train.df <- taxi.df[train.index,]
valid.df <- taxi.df[-train.index,]

#Classification Tree
default.ct <- rpart(car_cancellation~., data = train.df, method = "class")
#plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

Create deeper Tree
library(caret)
deeper.ct <- rpart(car_cancellation ~., data = train.df, method = "class", cp =0, minsplit = 1)
#count number of leaves
#important because it
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
#plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col = ifelse(deeper.ct$frame$var =="<leaf>", 'gray', 'white'))

Confusion Matrix and Accuracy MEasures
#classify records in the validation data
#set argument type = "class" in predict() to generate predicted class membership
default.ct.point.pred.train <- predict(default.ct, train.df, type = "class")
#generate confusion matrix for training data
confusionMatrix(table(default.ct.point.pred.train, train.df$car_cancellation))
#repeat the code for the validation set and the deeper tree
I could not get the increase of the cutoff rate


Tree Pruning
#argument xval refers to the number of folds to use in rpart's built-in
#cross-validation procedure
#argument cp sets the smallest value for the complexity parameter
cv.ct <- rpart(car_cancellation~., data = train.df, method = "class", cp = 0.00001, minsplit = 5, xval = 5)
#use printcp() to print the table
printcp(cv.ct)

#prune by lower cp
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


Random Forest
#random forest
rf <- randomForest(as.factor(car_cancellation)~., data = train.df, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)
#ntree = number of trees
#mtry = to balance the use of predicted variables, decides on number of variables to analyze
#nodesize- how many decisions to have in each


#variable importance plot
varImpPlot(rf, type = 1)

#confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(table(rf.pred, valid.df$car_cancellation))

Tree Boosting
library(adabag)
train.df$car_cancellation <- as.factor(train.df$car_cancellation)

set.seed(1)
boost <- boosting(car_cancellation ~., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(table(pred$class, valid.df$car_cancellation))
92.4% accuracy rate







Logistic Regression

logit.reg <- glm(car_cancellation ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)
#CD.Account most important
# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(logit.reg, valid.df[,], type = "response")
confusionMatrix(table(ifelse(logit.reg$fitted.values >0.5, 'Yes', 'No'), valid.df$car_cancellation))
confusionMatrix(table(ifelse(logit.reg.pred > .5, 1, 0), valid.df$car_cancellation))

# first 5 actual and predicted records
data.frame(actual = valid.df$car_cancellation[1:5], predicted = logit.reg.pred[1:5])
#Evaluate Performance
library(gains)
gain <- gains(valid.df$car_cancellation, logit.reg.pred, groups=10)
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$car_cancellation))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$car_cancellation))~c(0, dim(valid.df)[1]), lty=2)
# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$car_cancellation)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)



Cannot get confusion matrix for logistic regression to work but looking at the lift chart it looks this the
model is running well above the basic model which is what it would be if the variable outcome was guessed.

Thanks for a great semester!!!!
