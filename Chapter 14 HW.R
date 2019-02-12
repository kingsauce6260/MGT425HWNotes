14

14.1
This approach will work well because it allows the analyst to focus on potential customers who are related to current buyers.
This allows the analyst to uncover relationships between customers that may have been unknown.
The association technique uses association rules that due this. They allow for the analyst to see how much better
the chances would be of getting a certain outcome if the rule is used compared to choosing randomly.
This allows them to find the most important relationships.


14.2

library(arules)
library(caret)


ct.df <- read.csv("Coursetopics.csv")

# remove first column and convert to matrix
ct.mat <- as.matrix(ct.df[, ])

# convert the binary incidence matrix into a transactions database
ct.trans <- as(ct.mat, "transactions")
inspect(ct.trans)

## get rules
# when running apriori(), include minimum support & confidence, & target as arguments.

rules <- apriori(ct.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))
