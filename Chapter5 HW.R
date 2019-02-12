##5.1- 88frad 30corfrad 952nonf 920cornon
pfraud <- 88
cfraud <- 30
pnfraud <- 952
cnfraud <- 920
fall <- (pfraud+cfraud+pnfraud+cnfraud)
fraud <- c(cfraud, (pnfraud - cnfraud))
nfraud <- c((pfraud - cfraud), cnfraud)

df <- data.frame(fraud, nfraud)

df
confusionMatrix(ifelse(df$fraud, 'fraud', 'nonfraud') , df$nfraud)

#fraudulant error rate
ferr <- (32+58)/(30+32+58+920)
##The overall error rate is 8.65%

#truly fraudulant error rate


50+30/

tferr <- 58/(30+58)
#truly fraudulant error rate is 65.91%

#truly nonfraudulant error rate
tnferr <- 32/(32+920)
#truly nonfraudulant error rate is 3.36%

#Interpret the meaning of the first and second bars from the left

#Explain how you might use this information in practice

#When classifying everything as nonfraudulant what is the error rate?

#Comment on the usefulness, in this situation, of these two metrics
#of model performance (error rate & lift)
