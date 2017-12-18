library(tm)

#Path of the document folders
DirpathName <- "/Users/sibanjan/datascience/studies/textmining/documentclassification"

#Individual Zone Folders for training/testing. Initially we train on articles for BigData and Database zones
SubDirectories <- c("BigData","database")

#Function to build and clean the documents 
generateTDM <- function(subDir , path){
    # Create complete directories by concatinating Directory and Subdirectory
    dir <-paste(path, subDir, sep="/")
    #create a corpus using the above  full formed directory path
   
    articles.corpus <- Corpus(DirSource(directory = dir , encoding="UTF-8"))

    # make each letter lowercase
    articles.corpus <- tm_map(articles.corpus, tolower)
    # remove punctuation
    articles.corpus <- tm_map(articles.corpus, removePunctuation)
    #remove numbers
    articles.corpus <- tm_map(articles.corpus, removeNumbers);
    # remove generic and custom stopwords
    stopword <- c(stopwords('english'), "best");
    articles.corpus <- tm_map(articles.corpus, removeWords, stopword)
    articles.corpus <- tm_map(articles.corpus, stemDocument);

    #create TDM
    articles.tdm <- TermDocumentMatrix(articles.corpus)

    #remove sparse terms
    articles.tdm <- removeSparseTerms(articles.tdm,0.7)

    #return the results , which is the list of TDM for spam and ham.
    result <- list(name = subDir , tdm = articles.tdm)
}

# Let’s write a function that can convert Term document matrix to data frame

#The folder name represents the type of article. The below funtion binds the folder name
# as a target to each document
BindtargetToTDM <- function(indTDM){
    # Transpose the TDM, so that the words becomes the columns and row is the number of occurences of word

    article.matrix <- t(data.matrix(indTDM[["tdm"]]))

    #convert this matrix into data frame

    article.df <- as.data.frame(article.matrix , stringASFactors = FALSE)

    # Add the target to each row of the data frame

    article.df <- cbind(article.df , rep(indTDM[["name"]] , nrow(article.df)))

    #Give a name to the new target column

    colnames(article.df)[ncol(article.df)] <- "ArticleZone"
    return (article.df)
}

list.tdm <- lapply(SubDirectories , generateTDM , path = pathName)
article.df <- lapply(list.tdm, BindtargetToTDM)

# join both Big Data and Database Zone data frames
library(plyr)
all_article.df <- do.call(rbind.fill , article.df)
# fill the empty columns with 0
all_article.df[is.na(all_article.df)] <- 0

str(all_article.df)

target_col_position <- which(colnames(all_article.df)=="ArticleZone")

#Reorder the target and predictors
all_article.df.ordered <- all_article.df[,c(target_col_position, 1:target_col_position-1,(target_col_position+1):ncol(all_article.df))]

#Prepare training/test set
train.idx <- sample(nrow(all_article.df.ordered) , ceiling(nrow(all_article.df.ordered)* 0.7))
test.idx <- (1:nrow(all_article.df.ordered))[-train.idx]

all_article.df.train <- all_article.df.ordered[train.idx,]
all_article.df.test <- all_article.df.ordered[test.idx,]

library(e1071)
library(caret)

trainedModel <- naiveBayes(all_article.df.train[,c(2:ncol(all_article.df.ordered))],all_article.df.train[,c(1)], data = all_article.df.train)
predict.test <- predict(trainedModel, all_article.df.test)
confusionMatrix <- confusionMatrix(predict.test,all_article.df.test[,c(1)])
confusionMatrix


tdm.new <- lapply("new" , generateTDM , path = pathName)
DataFrame.new <- lapply(tdm.new, BindtargetToTDM)
predict_new <- predict(trainedModel, DataFrame_new)
as.character(BigData)


