laptopjanuarysales.df <- read.csv("LaptopSalesJanuary2008.csv", header = TRUE)
library(ggplot2)
#Barplot
data.ml.plot <- aggregate(laptopjanuarysales.df$Retail.Price, by=list(laptopjanuarysales.df$Store.Postcode), FUN=mean)


ggplot(data.ml.plot) + geom_bar(aes(x = as.factor(Group.1), y=`x`), stat = "identity")+
  xlab("Store Postcode") + ylab("Retail Mean") + coord_cartesian(ylim=c(480,495))



##SidebySide boxplots
par(mfcol = c(1,1))
boxplot(laptopjanuarysales.df$Retail.Price~laptopjanuarysales.df$Store.Postcode, xlab = "Postcode",
        ylab = "Retail Mean", log = 'y')
