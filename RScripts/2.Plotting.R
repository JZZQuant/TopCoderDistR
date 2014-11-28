
library(ggplot2)
qplot(date,data=df[df$date>"1998-01-01",],geom='bar',binwidth=40)
qplot(date,amount,data=df[df$date>"1998-01-01",],geom='line')
qplot(field,data=df[df$date>"1998-01-01" & df$amount >2000000,],geom='bar',weight=amount)
qplot(df$date[grepl('results',df$abstract)])
qplot(date,data=df[df$date>"1998-01-01" & df$amount >2000000,],fill=as.factor(amount),binwidth=55)
names(lapply(head(sort(table(df$invest))),function(x) gsub("s+"," ",x)))[-1]
names(lapply(head(sort(table(df$field))),function(x) gsub("s+"," ",x)))[-1]

