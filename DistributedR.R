#install.packages(c('RJSONIO','ggplot2','wordcloud','tm'))

#Loading Data in R --------------------------------------------------------------------------------------------------
setwd("Scrapped")
#load all the json files and list them into a list
library(RJSONIO)
blogs<-list.files(pattern="\\.json")
bloglists<-lapply(blogs,fromJSON)
allblogs<-unlist(bloglists,recursive=F)

#get the content to a json listand comment respone
comments.count<- as.numeric(sapply(allblogs,function(x) {if(length(x[[4]])==1 && x[[4]]=="" ) 0 else length(x[[4]])}))
content<- as.character( sapply(allblogs,function(x) paste(x[[5]],collapse=" ")))

#most repeated tags
head(sort(table(as.factor(unlist(sapply(allblogs, function(x){unlist(x[['Tags']])})))),decreasing = T),40)
library(ggplot2)
# time series analysis of blogs
qplot(as.Date(sapply(allblogs, function(x){unlist(x[['Date']])})))
#Time series analysis blog interactions
qplot(as.Date(sapply(allblogs, function(x){unlist(x[['Date']])}))[comments.count>0])
#un answered comments
qplot(as.Date(sapply(allblogs, function(x){unlist(x[['Date']])}))[comments.count=1])
#most tagged areas
head(sort(table(as.factor(unlist(sapply(allblogs, function(x){unlist(x[['Area']])})))),decreasing = T),40)
#top bloggers
head(sort(table(as.factor(unlist(sapply(allblogs, function(x){unlist(x[['Author']])})))),decreasing = T),50)
#trending the popular words over time
qplot(as.Date(sapply(allblogs, function(x){unlist(x[['Date']])}))[grepl('Nomad',content)])
qplot(as.Date(sapply(allblogs, function(x){unlist(x[['Date']])}))[grepl('Shopping',content)])
#ggplot2 and nlp of tm have conflicting issues
#detach("package:ggplot2", unload=TRUE)

# create a corpus and clean it up
library(tm)
blogcorp<-Corpus(VectorSource(content))
clean<-tm_map(blogcorp,tolower)
clean<-tm_map(blogcorp,removeNumbers)
clean<-tm_map(clean,removeWords,stopwords())
clean<-tm_map(clean,removePunctuation)
clean<-tm_map(clean,stripWhitespace)
clean <- tm_map(clean, PlainTextDocument)

#wordcloud of  most repeated words and tags
library(wordcloud)
wordcloud(clean, min.freq = 40, random.order = FALSE,colors=brewer.pal(8, "Dark2"))

#create a document term matrix and remove too obvious and unwanted words
dtm<-DocumentTermMatrix(clean)
onedict<-findFreqTerms(dtm,lowfreq=1, highfreq=10)
onetrain <- DocumentTermMatrix(clean,list(dictionary = onedict))

# convert to a markhov stochaistic matrix
bayesian.counts <- function(x) {x <- ifelse(x > 0, 1, 0); return(x)}
# a logical matrix to map all words to documents inverted indices
boolean.matrix <- apply(onetrain, MARGIN = 2, bayesian.counts)
# divide by column sum to convert the matrix into a markov like stohaistic matrix
stochaitic.matrix<-t(t(boolean.matrix)/colSums(boolean.matrix))

# unsupervised learning algorithm to find similar duplicates
find.duplicates <-function(test)
{
  #diiferential
  linear.difference<-t(t(boolean.matrix)-boolean.matrix[test,])
  #normalize
  normal.counts <- function(x) {x <- ifelse(x == 0, 0, 1); return(x)}
  normal.matrix <- apply(linear.difference, MARGIN = 2, normal.counts)
  #scalling
  linear.span<-t(t(normal.matrix)*stochaitic.matrix[test,])
  #find the most similar looking blogs
  nearestblogs<-unique(match(head(sort(rowSums(linear.span)),100),rowSums(linear.span)))
  #get the top two neighbours
  sapply(allblogs[nearestblogs], function(x){unlist(x[['URL']])})[1:2]
}

# find duplicates
lapply(c(143,56,37,745),find.duplicates)
