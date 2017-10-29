library(tm)
library(textir)

#set working directory (modify path as needed)
setwd(“C:\\Users\\Kailash\\Documents\\TextMining”)
 
#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern=”*.txt”)
 

#read files into a character vector
files <- lapply(filenames,readLines)
 

#create corpus from vector
docs <- Corpus(VectorSource(files))
ardata.corpus<-Corpus(VectorSource(data))

# make each letter lowercase
ardata.corpus <- tm_map(ardata.corpus, tolower)

# remove punctuation
ardata.corpus <- tm_map(ardata.corpus, removePunctuation)


#remove numbers
myCorpus = tm_map(myCorpus, removeNumbers);

# remove generic and custom stopwords
myStopwords = c(stopwords('english'), "available", "via");
ardata.corpus <- tm_map(ardata.corpus, removeWords, stopword)

dictCorpus = myCorpus;
 
myCorpus = tm_map(myCorpus, stemDocument);
 
myCorpus = tm_map(myCorpus, stemCompletion, dictionary=dictCorpus);
 
myDtm = DocumentTermMatrix(myCorpus, control = list(minWordLength = 3));
mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.98)

k = 2;
SEED = 1234;
my_TM =
list(VEM = LDA(myDtm, k = k, control = list(seed = SEED)),
VEM_fixed = LDA(myDtm, k = k,
control = list(estimate.alpha = FALSE, seed = SEED)),
Gibbs = LDA(myDtm, k = k, method = "Gibbs",
https://www.r-bloggers.com/text-mining/
https://www.slideshare.net/MinhaHwang/introduction-to-text-mining-32058520
 
findFreqTerms(myDtm, lowfreq=50);
#find the probability a word is associated
findAssocs(myDtm, 'find_a_word', 0.5);