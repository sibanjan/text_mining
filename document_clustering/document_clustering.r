
library(tm)
library(SnowballC)
#set working directory (modify path as needed)
setwd("//Users//sibanjan//datascience//studies//textmining//documentclustering")
#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern="*.txt")
#read files into a character vector
files <- lapply(filenames,readLines)
#create corpus from vector
articles.corpus <- Corpus(VectorSource(files))



articleDtm <- DocumentTermMatrix(articles.corpus, control = list(minWordLength = 3));
articleDtm2 <- removeSparseTerms(articleDtm, sparse=0.98)

articles_mat <- as.matrix(articleDtm2)

distMatrix <- dist(articles_mat, method="euclidean")

groups <- hclust(distMatrix,method="ward.D")
plot(groups, cex=0.9)
rect.hclust(groups, k=3)