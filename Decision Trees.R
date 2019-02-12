Chapter 9: Decision Trees

library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

mower.df <- read.csv("RidingMowers.csv")



#use rpart() in rpart() to determine the depth of the tree
#define rpart.control() in rpart() to determine the depth of the tree
class.tree<- rpart(Ownership~., data = mower.df, control = rpart.control(maxdepth = 2), method = "class")

#Plot tree
#use prp() to plot the tree. You can control plotting parameters such as color, shaoe,
#and information displayed (which and where)

prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)


Decision Tree

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[,-c(1,5)] #drop ID and zip code columns
#partition
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

#Classification Tree
default.ct <- rpart(Personal.Loan~., data = train.df, method = "class")
#plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

Create deeper Tree
library(caret)
deeper.ct <- rpart(Personal.Loan ~., data = train.df, method = "class", cp =0, minsplit = 1)
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
confusionMatrix(default.ct.point.pred.train, train.df$Personal.Loan)
#repeat the code for the validation set and the deeper tree


Tree Pruning
#argument xval refers to the number of folds to use in rpart's built-in
#cross-validation procedure
#argument cp sets the smallest value for the complexity parameter
cv.ct <- rpart(Personal.Loan~., data = train.df, method = "class", cp = 0.00001, minsplit = 5, xval = 5)
#use printcp() to print the table
printcp(cv.ct)

#prune by lower cp
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


Random Forest
#random forest
rf <- randomForest(as.factor(Personal.Loan)~., data = train.df, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)
#ntree = number of trees
#mtry = to balance the use of predicted variables, decides on number of variables to analyze
#nodesize- how many decisions to have in each


#variable importance plot
varImpPlot(rf, type = 1)

#confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(table(rf.pred, valid.df$Personal.Loan))

Tree Boosting
library(adabag)
train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)

set.seed(1)
boost <- boosting(Personal.Loan ~., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(table(pred$class, valid.df$Personal.Loan))

