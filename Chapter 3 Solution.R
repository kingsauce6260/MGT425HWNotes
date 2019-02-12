Chapter 3 In-class Hands-on Work
###Includes Barchart, ggplot, scatter plot, time series, histogram, heatmap, panel plat, scatterplot matrix, tree plot
housing.df <-read.csv("BostonHousing.csv")
head(housing.df,9)

Simple Charts

Creating Bar Chart
## barchart of CHAS vs. mean MEDV
# compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV,  names.arg = data.for.plot$CHAS,
        xlab = "CHAS", ylab = "Avg. MEDV")

# alternative plot with ggplot
# make sure you bring in library ggplot2

library(ggplot2)
ggplot(data.for.plot) + geom_bar(aes(x = CHAS, y = MeanMEDV), stat = "identity")

Creating Time-series
Amtrak.df <- read.csv("Amtrak.csv")
library(forecast)
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
#first variable before ~ is the dependent variable


Creating Scatter Plot
## scatter plot with axes names
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "MDEV", ylab = "LSTAT")
# alternative plot with ggplot

library(ggplot2)
ggplot(housing.df) + geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.7)

Creating Histogram
## histogram of MEDV
hist(housing.df$MEDV, xlab = "MEDV")

# alternative plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_histogram(aes(x = MEDV), binwidth = 5)

Heatmap
## simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv = NA, Colv = NA)

## heatmap with values
library(ggplot2)
heatmap.2 <- heatmap(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(housing.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# alternative plot with ggplot
library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(housing.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  geom_text(aes(x = X1, y = X2, label = value))

Challenge Questions
1-	Display the relationship between MDEV and TAX.
cor(housing.df[,c("MEDV", "TAX")])
2-	Create a histogram of AGE.
hist(housing.df$AGE)
3-	Create a boxplot of MEDV for different values of CHAS
par(mfcol = c(1,2))
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")






Chapter 3 In-class Hands-on Work

Advanced Charts
## color plot
par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
# Plot with ggplot
library(ggplot2)
ggplot(housing.df, aes(y = NOX, x = LSTAT, colour= CAT..MEDV)) +
  geom_point(alpha = 0.6)

## panel plots
# compute mean MEDV per RAD and CHAS
# In aggregate() use argument drop = FALSE to include all combinations
# (exiting and missing) of RAD X CHAS.
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, housing.df$CHAS),
                           FUN = mean, drop = FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "meanMEDV")
# Plot with ggplot
ggplot(data.for.plot) +
  geom_bar(aes(x = as.factor(RAD), y = `meanMEDV`), stat = "identity") +
  xlab("RAD") + facet_grid(CHAS ~ .)

## Scatter Plot Matrix
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal,
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

# alternative, nicer plot (displayed)
library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)])

# Rescaling
options(scipen=999) # avoid scientific notation
library(ggplot2)
# First examine the skewness in the data
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV))
# Now let’s rescale it
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV)) +
  scale_x_log10(breaks = 10^(-2:2),
                labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_log10(breaks = c(5, 10, 20, 40))

#### Tree Map

library(treemap)
tree.df <- read.csv("EbayTreemap.csv")

# add column for negative feedback
tree.df$negative.feedback <- 1* (tree.df$Seller.Feedback < 0)

# draw treemap
treemap(tree.df, index = c("Category","Sub.Category", "Brand"),
        vSize = "High.Bid", vColor = "negative.feedback", fun.aggregate = "mean",
        align.labels = list(c("left", "top"), c("right", "bottom"), c("center", "center")),
        palette = rev(gray.colors(3)), type = "manual", title = "")





Chapter 3 In-class Hands-on Work

Advanced Charts
## color plot
par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
# Plot with ggplot
library(ggplot2)
ggplot(housing.df, aes(y = NOX, x = LSTAT, colour= CAT..MEDV)) +
  geom_point(alpha = 0.6)

## panel plots
# compute mean MEDV per RAD and CHAS
# In aggregate() use argument drop = FALSE to include all combinations
# (exiting and missing) of RAD X CHAS.
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, housing.df$CHAS),
                           FUN = mean, drop = FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "meanMEDV")
# Plot with ggplot
ggplot(data.for.plot) +
  geom_bar(aes(x = as.factor(RAD), y = `meanMEDV`), stat = "identity") +
  xlab("RAD") + facet_grid(CHAS ~ .)

## Scatter Plot Matrix
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal,
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

# alternative, nicer plot (displayed)
library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)])

# Rescaling
options(scipen=999) # avoid scientific notation
library(ggplot2)
# First examine the skewness in the data
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV))
# Now let’s rescale it
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV)) +
  scale_x_log10(breaks = 10^(-2:2),
                labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_log10(breaks = c(5, 10, 20, 40))

#### Tree Map

library(treemap)
tree.df <- read.csv("EbayTreemap.csv")

# add column for negative feedback
tree.df$negative.feedback <- 1* (tree.df$Seller.Feedback < 0)

# draw treemap
treemap(tree.df, index = c("Category","Sub.Category", "Brand"),
        vSize = "High.Bid", vColor = "negative.feedback", fun.aggregate = "mean",
        align.labels = list(c("left", "top"), c("right", "bottom"), c("center", "center")),
        palette = rev(gray.colors(3)), type = "manual", title = "")

Challenge Questions
•	Use GGally library to create correlation matrix for CRIM (1) and MEDV (13)
# alternative, nicer plot (displayed)
library(GGally)
ggpairs(housing.df[, c(1, 13)])

# Rescaling
options(scipen=999) # avoid scientific notation
library(ggplot2)
# First examine the skewness in the data
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV))
# Now let’s rescale it
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV)) +
  scale_x_log10(breaks = 10^(-2:2),
                labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_log10(breaks = c(5, 10, 20, 40))

•	For AGE, RAD, and CAT. MEDV draw an appropriate chart.
plot(housing.df[, c(7, 9, 14)])
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$AGE, housing.df$RAD, housing.df$CAT..MEDV),
                          FUN = mean, drop = FALSE)
names(data.for.plot) <- c("AGE", "RAD", "CAT..MEDV")
ggplot(data.for.plot) +
  geom_bar(aes(x = as.factor(AGE), y = `RAD`), stat = "identity") +
  xlab("RAD") + facet_grid(CAT..MEDV ~ .)


