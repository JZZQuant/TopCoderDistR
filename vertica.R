#setwd("Data")
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
chunk<-lapply(articles[1:5000],function(x) Structured(x))
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
