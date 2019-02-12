Chapter 13 R code
Chapter 13 In-class
library(uplift)
voter.df <- read.csv("Voter-Persuasion_0.csv")
# transform variable MOVED_AD to numerical
voter.df$MOVED_AD_NUM <- ifelse(voter.df$MOVED_AD == "Y", 1, 0)
set.seed(1)
train.index <- sample(c(1:dim(voter.df)[1]), dim(voter.df)[1]*0.6)
train.df <- voter.df[train.index, ]
valid.df <- voter.df[-train.index, ]
# use upliftRF to apply a Random Forest (alternatively use upliftKNN() to apply kNN).
up.fit <- upliftRF(MOVED_AD_NUM ~ AGE + NH_WHITE + COMM_PT + H_F1 + REG_DAYS+
                     PR_PELIG + E_PELIG + POLITICALC  + trt(MESSAGE_A),
                   data = train.df, mtry = 3, ntree = 100, split_method = "KL",
                   minsplit = 200, verbose = TRUE)
pred <- predict(up.fit, newdata = valid.df)
# first colunm: p(y | treatment)
# second colunm: p(y | control)
head(data.frame(pred, "uplift" = pred[,1] - pred[,2]))
#Higher uplift more chance that person will change their mind
Now, run a new Uplift model using the following variables:
  AGE	NH_ASIAN	COMM_WALK
HH_ND	NH_MULT	GENDER_F
HH_NR	HISP	GENDER_M
HH_NI	COMM_CAR
NH_WHITE	COMM_CP
NH_AA	COMM_PT
MESSAGE_A


