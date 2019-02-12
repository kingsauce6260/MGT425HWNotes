housing.df <- read.csv("BostonHousing.csv")
head(housing.df,9)

##barchart of CHAS vs. mean MEDV
#compute mean MEDV per CHAS = (0,1)
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV, names.arg = data.for.plot$CHAS,
        xlab = "CHAS", ylab = "Avg.MEDV")

#alternative plot with ggplot
#bring in library ggplot2

library(ggplot2)
ggplot(data.for.plot) + geom_bar(aes(x=CHAS, y=MeanMEDV), stat = "identity")


##Creating Time-Series
Amtrak.df <- read.csv("Amtrak.csv")
library(forecast)
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991,1), end = c(2004, 3), freq=12)
plot(ridership.ts, xlab = "Year", ylab = "Ridership(in 000s)", ylim = c(1300, 2300))


##side-by-side boxplots
#use par() to split the plots into panels
par(mfcol = c(1,4))
boxplot(housing.df$NOX~housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
boxplot(housing.df$LSTAT~housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab="LSTAT")


