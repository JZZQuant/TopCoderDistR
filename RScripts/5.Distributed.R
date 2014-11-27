library(distributedR)
distributedR_start()

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

dmultiply<-function(matrix,array)
{
  da<-as.darray(rep.row(array,nrow(matrix)),dim(matrix))
  db<-as.darray(matrix,dim(matrix))
  result<-darray(dim=dim(matrix),dim(matrix))
  foreach(i, 1:npartitions(da),
          mul<-function(a = splits(da,i),
                        b = splits(db,i),
                        c = splits(result,i)){
            c <- a * b
            update(c)
          })
  getpartition(result)
}

dsub<-function(matrix,array)
{
  da<-as.darray(rep.row(array,nrow(matrix)),dim(matrix))
  db<-as.darray(matrix,dim(matrix))
  result<-darray(dim=dim(matrix),dim(matrix))
  foreach(i, 1:npartitions(da),
          sub<-function(a = splits(da,i),
                        b = splits(db,i),
                        c = splits(result,i)){
            c <- a - b
            update(c)
          })
  getpartition(result)
}
# unsupervised learning algorithm to find similar duplicates
find.duplicates <-function(test)
{
  #diiferential
  #linear.difference<-t(t(boolean.matrix)-boolean.matrix[test,])
  linear.difference<-dsub(boolean.matrix,boolean.matrix[test,])
  #normalize
  normal.counts <- function(x) {x <- ifelse(x == 0, 0, 1); return(x)}
  normal.matrix <- apply(linear.difference, MARGIN = 2, normal.counts)
  #scalling
  linear.span<-dmultiply(normal.matrix,stochaitic.matrix[test,])
  #find the most similar looking blogs
  nearestblogs<-unique(match(head(sort(rowSums(linear.span)),100),rowSums(linear.span)))
  #get the top two neighbours
  df[nearestblogs,]$title[1:2]
}

# find duplicates
# results<-lapply(c(372,143,87,790,368,241),find.duplicates)
lapply(372,find.duplicates)
distributedR_shutdown()

