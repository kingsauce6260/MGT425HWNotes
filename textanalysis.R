library(tm)
library(SnowballC)
text <- c("this is the first sentence",
          "this is a second sentence",
          "the third sentence is here")
#Creating a text vector made up of all these three sentences
corp <- Corpus(VectorSource(text))

#term document matrix
#takes every common word and puts them in a list and will tell you how much each occurs in each sentence.
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

look into factor analysis


LSI
Taking a lot of words describing something and reducing it to a concept

library(tm)
# step 1: import and label records
# read zip file into a corpus
corp <- Corpus(ZipSource("AutoAndElectronics.zip",
                         recursive = T))

# create an array of records labels
label <- c(rep(1, 1000), rep(0, 1000))

# tokenization
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)

# stopwords
corp <- tm_map(corp, removeWords, stopwords("english"))

#Stemming
corp <- tm_map(corp, stemDocument)



# step 3: TF-IDF and latent semantic analysis

# compute TF-IDF
tdm <- TermDocumentMatrix(corp)
tfidf <- weightTfIdf(tdm)

# extract (20) concepts
library(lsa)
lsa.tfidf <- lsa(tfidf, dim = 20)




# convert to data frame
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))

# sample 60% training data
training <- sample(c(1:2000), 0.6*2000)

# run logistic model on training
trainData = cbind(label = label[training], words.df[training,])
reg <- glm(label ~ ., data = trainData, family = 'binomial')

# compute accuracy on validation set
validData = cbind(label = label[-training], words.df[-training,])
pred <- predict(reg, newdata = validData, type = "response")

# produce confusion matrix

library(caret)
confusionMatrix(ifelse(pred>0.5, 1, 0), label[-training])
