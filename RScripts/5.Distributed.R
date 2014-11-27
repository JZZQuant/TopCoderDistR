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
lapply(372,find.duplicates)
