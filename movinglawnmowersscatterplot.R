movinglawnmowers.df <- read.csv("RidingMowers.csv")

plot(movinglawnmowers.df$Lot_Size ~ movinglawnmowers.df$Income, xlab = "Lot Size", ylab = "Income")
par(xpd=TRUE)
library(ggplot2)

ggplot(movinglawnmowers.df) + geom_point(aes(x = Income, y= Lot_Size, color= Ownership), alpha = 0.7)


