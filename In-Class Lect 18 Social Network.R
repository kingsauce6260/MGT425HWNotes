library(igraph)
# define links in data
edges <- rbind(
  c("Dave", "Jenny"), c("Peter", "Jenny"), c("John", "Jenny"),
  c("Dave", "Peter"), c("Dave", "John"), c("Peter", "Sam"),
  c("Sam", "Albert"), c("Peter", "John")
)
# generate and plot graph
# set argument directed = FALSE in graph.edgelist() to plot an undirected graph.
g <- graph.edgelist(edges, directed = FALSE)
plot(g, vertex.size = 1, vertex.label.dist = 0.5)

gd <- graph.edgelist(edges, directed = TRUE)
plot(gd, vertex.size = 1, vertex.label.dist = 0.5)

library(igraph)
drug.df <- read.csv("drug.csv")

# convert edges to edge list matrix
edges <- as.matrix(drug.df[, c(1,2)])
g <- graph.edgelist(edges,directed=FALSE)

# plot graph
# nodes' size is proportional to their eigenvector centrality
plot(g, vertex.label = NA, vertex.size = eigen_centrality(g)$vector * 20)




library(twitteR)
#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <-  'CVWkhvXKla1sY0jUSgYHTiYQ7'
consumer_secret <- 'DzT2e8LBaSolQJu48lfWFJcCjoUJGQDUAReXJLd71fT4D5jtvp'
access_token <- '915374479-hhjgplHHYRH4UjMICBEmoGmOtKKhOZY0sOGXmahs'
access_secret <- 'oflxrQi3yqDaUlcck2sSTBLKvM9D74IEjFsFqKxvJxEq2'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#the cainfo parameter is necessary only on Windows
search_term <- "Donald Trump"
r_stats <- searchTwitter(search_term, n=10)
##should get 100
length(r_stats)
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
write(r_stats_text, "Twitter_Data.txt", sep = "\t")
unclean_tweet <- read.delim("Twitter_Data.txt", header = FALSE, sep ="\t")
unclean_tweet
