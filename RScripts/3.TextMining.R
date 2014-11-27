library(tm)
blogcorp<-Corpus(VectorSource(df$abstract))
clean<-tm_map(blogcorp,tolower)
clean<-tm_map(blogcorp,removeNumbers)
clean<-tm_map(clean,removeWords,stopwords())
clean<-tm_map(clean,removePunctuation)
clean<-tm_map(clean,stripWhitespace)
clean <- tm_map(clean, PlainTextDocument)

#wordcloud of  most repeated words and tags
#library(wordcloud)
#wordcloud(clean, min.freq = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))

#create a document term matrix and remove too obvious and unwanted words
dtm<-DocumentTermMatrix(clean)
onedict<-findFreqTerms(dtm,lowfreq=1, highfreq=10)
onetrain <- DocumentTermMatrix(clean,list(dictionary = onedict))