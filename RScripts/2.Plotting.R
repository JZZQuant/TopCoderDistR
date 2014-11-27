
library(ggplot2)
qplot(date,data=df[df$date>"1998-01-01",],geom='bar')
qplot(date,amount,data=df[df$date>"1998-01-01",],geom='line')
qplot(field,data=df[df$date>"1998-01-01" & df$amount >2000000,],geom='bar',weight=amount)
head(sort(table(df$invest)))
head(sort(table(df$field)))
qplot(df$date[grepl('results',df$abstract)])
qplot(date,data=df[df$date>"1998-01-01" & df$amount >2000000,],fill=as.factor(amount))
