#записать в json файл дата-фрейм
json <- toJSON(publ, pretty = TRUE)
write(json, "C:\\Users\\Polina\\Desktop\\data1.json")


library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))
text <- fromJSON('C:\\Users\\Polina\\Desktop\\data.json')
publ <- text
publ$source <- NULL
publ$display_text_width <- NULL
publ$hashtags <- NULL
publ$media_type <- NULL
publ$location <- NULL

#векторная модель
library(knitr)       # used to make kable tables
library(tm)          # text mining package
library(SnowballC)   # applies Porter's stemmming algorithm (discussed later) 
library(magrittr)    # allows pipe operator
library(tidytext)    # tidy text for plots
library(ggplot2)     # used for plots
library(dplyr) 

my_corpus <- Corpus(VectorSource(as.character(publ$text))) 

my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
my_corpus <- tm_map(my_corpus,content_transformer(tolower))
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, stemDocument)
dtm <- DocumentTermMatrix(my_corpus, control = list(weighting = weightTfIdf))
inspect(dtm)
inspect(dtm[1:17, 15:19])

#вероятностная модель
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tm)

d <- Corpus(VectorSource(as.character(publ$text))) 

d <- tm_map(d, removePunctuation)
d <- tm_map(d, removeNumbers)
d <- tm_map(d, removeWords, stopwords("english"))
d <- tm_map(d,content_transformer(tolower))
d <- tm_map(d, stripWhitespace)
d <- tm_map(d, stemDocument)
d <- data.frame(text = sapply(d, as.character), stringsAsFactors = FALSE)


bigrams <- unnest_tokens(d, bigram, text, token = "ngrams", n = 2)
coun <- count(bigrams, bigram, sort = TRUE)

d <- data.frame(text = sapply("still love   use  haters  motivation   better today  yesterday make  eat  words  pray  find faith   team", as.character), stringsAsFactors = FALSE)

r <- unnest_tokens(d, bigram, text, token = "ngrams", n = 2)

library(ngram)

d1 <- ngram(d$text[1], n=2)
get.ngrams ( d1)

#графовая
library("igraph");
library("Cairo");

words <- unlist(strsplit(gsub("[[:punct:]]", " ", tolower("still love   use  haters  motivation   better today  yesterday make  eat  words  pray  find faith   team")), "[[:space:]]+"));
g.start <- 1;
g.end <- length(words) - 1;
assocs <- matrix(nrow=g.end, ncol=2)
for (i in g.start:g.end)
{
  assocs[i,1] <- words[i];
  assocs[i,2] <- words[i+1];
}
g.assocs <- graph.data.frame(assocs, directed=F);
V(g.assocs)$label <- V(g.assocs)$name;
V(g.assocs)$color <- "Gray";
V(g.assocs)[unlist(largest.cliques(g.assocs))]$color <- "Red";
plot(g.assocs, layout=layout.random, vertex.size=4, vertex.label.dist=0);


#сочетания (чтобы построить матрицу слов)
library(gtools)
x <- c('red', 'blue', 'black', 'yellow');
#permutations(n=3,r=2,v=x);
d1 <- combn(x,2)
words <- character()
for (i in 1:6) #здесь должно быть число получившихся сочетаний в d1
  words <- c(words,paste(d1[1,i],d1[2,i])) 


#здесь начинается клстеризация
publ25 <- publ[1:10000, ,drop = FALSE] #берем первые 25 текстов

for(j in 1:10000)     
{        
  publ25$text[j] <- gsub("\"", "", publ25$text[j]) 
  publ25$text[j] <- gsub("\'", "", publ25$text[j])
  publ25$text[j] <- gsub("“", "", publ25$text[j])
  publ25$text[j] <- gsub("‘", "", publ25$text[j])
  publ25$text[j] <- gsub("’ll", "", publ25$text[j])
  publ25$text[j] <- gsub("”", "", publ25$text[j])
  publ25$text[j] <- gsub("’d", "", publ25$text[j])
  publ25$text[j] <- gsub("’s", "", publ25$text[j])
  publ25$text[j] <- gsub("[\u2013:\u2016]", "", publ25$text[j])
  publ25$text[j] <- gsub("'m", "", publ25$text[j])
  publ25$text[j] <- gsub("¦", "", publ25$text[j])
  publ25$text[j] <- gsub("’re", "", publ25$text[j])
  publ25$text[j] <- gsub("’ve", "", publ25$text[j])
  publ25$text[j] <- gsub("……", "", publ25$text[j])
  publ25$text[j] <- gsub("•", "", publ25$text[j])
  publ25$text[j] <- gsub("…", "", publ25$text[j])
  publ25$text[j] <- gsub("’", "", publ25$text[j])
  publ25$text[j] <- gsub("——————", "", publ25$text[j])
  publ25$text[j] <- gsub("[\u20AC]m", "", publ25$text[j])
  publ25$text[j] <- gsub("°", "", publ25$text[j])
  publ25$text[j] <- gsub("-", "", publ25$text[j])
  publ25$text[j] <- gsub("—", "", publ25$text[j])
  publ25$text[j] <- gsub("€m", "", publ25$text[j])
  publ25$text[j] <- gsub("a{2}", "", publ25$text[j])
}

corpus <- VCorpus(VectorSource(as.character(publ25$text))) 

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

library(rJava)
library(RWeka)
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm = DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))

inspect(tdm)
inspect(tdm[1:10, 1:5])
print(tdm$dimnames$Terms)

#кластеризация
library(dbscan)

tdm.tfidf <- tm::weightTfIdf(tdm)

rowTotals <- apply(tdm.tfidf , 1, sum) #Find the sum of words in each Document
dtm.new   <- tdm.tfidf[rowTotals > 0, ]   #remove all docs without words
dtm.new  <- tm::removeSparseTerms(dtm.new, 0.999) 

tfidf.matrix <- as.matrix(dtm.new) 

distMatrix_e <- dist(tfidf.matrix, method="euclidean")


library(proxy)
distMatrix = proxy::dist(tfidf.matrix, method = "cosine")

#так можно определить оптимальное eps
dbscan::kNNdistplot(distMatrix, k =  10)
abline(h = 0.15, lty = 2)

clustering.dbscan <- dbscan(distMatrix, eps = 0.4, minPts = 10)
clustering.dbscan

library(cluster)
s<-silhouette(clustering.dbscan$cluster, distMatrix)
plot(s)

clustering.dbscan_e <- dbscan(distMatrix_e, eps = 0.3, minPts = 10)
clustering.dbscan_e
#так смотрим кто в какой кластер: тут в какой кластер 2 документ
clustering.dbscan$cluster

for (i in 1:5000)
{
  if ((clustering.dbscan$cluster[i]) == 17)  { print (i) }
}

#классификация
library("syuzhet")
publ25 <- publ[1:2000, ,drop = FALSE] #берем первые 25 текстов


word.df <- as.vector(publ25$text)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(publ25, emotion.df) 

sent.value <- get_sentiment(word.df)

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)

category_senti2 <- cbind(publ25,category_senti)

#лаба 2

#clustering.kmeans_12 <- kmeans(distMatrix, 12)

clustering.kmeans_12 <- kmeans(tfidf.matrix, 12)
c_mean_12 <- cmeans(tfidf.matrix,12,20,verbose=TRUE,method="cmeans")
library(e1071)
resultindexes <- fclustIndex(c_mean_12,tfidf.matrix, index="separation.index")

s<-silhouette(clustering.kmeans_12$cluster, distMatrix)
plot(s)

#F-measure
library(FlowSOM)
FMeasure(clustering.dbscan$cluster, clustering.kmeans_12$cluster)

m <- as.matrix(distMatrix)
library(clusterCrit)
intCriteria(m,clustering.kmeans_12$cluster,c("SD_Scat"))

library(BCA)
sd <- SDIndex(m, minClust=2, maxClust=16, iter.max=10, num.seeds=10)

#FM - Folkes and Mallows index 
library(clues)
adjustedRand(clustering.dbscan$cluster, clustering.kmeans_12$cluster, randMethod = c("FM"))

data(wine, package='rattle')
head(wine)
wine.stand <- scale(wine[-1])  # To standarize the variables
k_mean <- kmeans(wine.stand, 3) # k = 3
c_mean <- cmeans(wine.stand,3,20,verbose=TRUE,method="cmeans")

library(clues)
cl <- as.vector(unlist(wine$Type))
adjustedRand(cl, k_mean$cluster, randMethod = c("FM"))

library(clusterCrit)
intCriteria(tfidf.matrix,clustering.kmeans_12$cluster,c("SD_Scat"))

library(FlowSOM)
FMeasure(cl, k_mean$cluster)

library(e1071)
resultindexes <- fclustIndex(c_mean,wine.stand, index="separation.index")


# prepare proper input data for SD and S_Dbw indicies
scatt <- clv.Scatt(tfidf.matrix, clustering.kmeans_12$cluster)
dis <- clv.Dis(scatt$cluster.center)

# compute  SD and S_Dbw indicies
SD <- clv.SD(scatt$Scatt, dis, alfa=4) # alfa is equal to number of clusters 
