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
