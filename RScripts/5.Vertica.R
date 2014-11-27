#install.packages(c('ggplot2','wordcloud','tm'))

articles<-list.files(pattern="\\.txt",recursive=T)
title<-NULL
date<-NULL
expire<-NULL
amount<-NULL
field<-NULL
invest<-NULL
Structured<- function(article)
{         
  lines<-readLines(article)
  amalgum<-paste(lines, collapse=" ")
  abstract<-gsub(".*Abstract    :"," ",amalgum)
  for(i in length(lines):1)
  {
    if(!grepl('.*:.*',lines[i]))
    {
      lines[i-1]=paste(lines[i-1],lines[i],sep=' ')
      lines[i]<-""
    }
  }
  
  lines<-lines[lines != ""]
  title<-c(title,gsub(".*Title       :",'',lines[grepl('Title',lines)]))
  date<-c(date,gsub(".*Start Date  :",'',lines[grepl('Start Date',lines)]))
  expire<-c(expire,gsub(".*Expires     :",'',lines[grepl('Expires',lines)]))
  amount<-gsub("[^0-9]", "",c(amount,gsub(".*Total Amt.  :",'',lines[grepl('Amt',lines)])))
  field<-c(field,gsub(".*Fld Applictn:",'',lines[grepl('Fld',lines)]))
  invest<-c(invest,gsub(".*Investigator:",'',lines[grepl('Investigator',lines)]))
  c(title[[1]],date[[1]],expire[[1]],amount[[1]],field[[1]],invest[[1]],abstract[[1]])
}
chunk<-lapply(articles[1:1000],function(x) Structured(x))
df<-data.frame(matrix(unlist(chunk,recursive=F),ncol=7,byrow=T), stringsAsFactors =F)
colnames(df)<-c("title","date","expire","amount","field","invest","abstract")
df<-transform(df,date=as.Date(date," %B %d, %Y"),expire=as.Date(date," %B %d, %Y"),amount=as.numeric(amount),field=as.factor(field),invest=as.factor(invest))



library(ggplot2)
qplot(date,data=df[df$date>"1998-01-01",],geom='bar')
qplot(date,amount,data=df[df$date>"1998-01-01",],geom='line')
qplot(field,data=df[df$date>"1998-01-01" & df$amount >2000000,],geom='bar',weight=amount)
head(sort(table(df$invest)))
head(sort(table(df$field)))
qplot(df$date[grepl('results',df$abstract)])
qplot(date,data=df[df$date>"1998-01-01" & df$amount >2000000,],fill=as.factor(amount))


library(tm)
blogcorp<-Corpus(VectorSource(df$abstract))
clean<-tm_map(blogcorp,tolower)
clean<-tm_map(blogcorp,removeNumbers)
clean<-tm_map(clean,removeWords,stopwords())
clean<-tm_map(clean,removePunctuation)
clean<-tm_map(clean,stripWhitespace)
clean <- tm_map(clean, PlainTextDocument)

#wordcloud of  most repeated words and tags
library(wordcloud)
# wordcloud(clean, min.freq = 40, random.order = FALSE,colors=brewer.pal(8, "Dark2"))

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
  df[nearestblogs,]$title[1:2]
}

# find duplicates
lapply(c(372,143,87,790,368,241),find.duplicates)
