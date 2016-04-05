library(ggplot2)
library(scales)
library (plyr)

aniversarios <- read.csv("aniversarios.csv")

aniversarios$start2 <- as.Date( substr(as.character(aniversarios$Start),1,10), "%m/%d/%Y")
aniversarios$month <- substr(as.character(aniversarios$Start),1,2)

qplot(data=aniversarios, x=aniversarios$start2, binwidth=1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_date(labels = date_format("%m-%d"), breaks=date_breaks("day"))

qplot(data=aniversarios, x=aniversarios$start2, binwidth=1 ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_date(labels = date_format("%m-%d"), breaks=date_breaks("day")) +
  facet_wrap(~month)
#Unmanagable graph. Let's find the month with the day with the biggest anniversary:

aniversarios_per_day = count(aniversarios, c('start2','month'))

aniversario_max_per_month = aggregate(freq ~ month, data = aniversarios_por_dia, FUN = max)

qplot(data=aniversario_max_per_month, 
      month) +
     geom_line(aes(y = freq)) 

ggplot(data=aniversario_max_per_month,
       aes(x=month, y=freq)) +
  geom_line()

qplot(month, freq, data = aniversario_max_per_month)

monthMax = aniversario_max_per_month[which.max(aniversario_max_per_month$freq),1]

#Month max = 5.

str(aniversarios)


aniversarios_05 = subset(aniversarios, format(aniversarios$start2,'%Y-%m')=='2016-05')


qplot(data=aniversarios_05, x=aniversarios_05$start2, binwidth=1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_date(labels = date_format("%m-%d"), breaks=date_breaks("day"))

# day_max = 12/may

#Anniversaries per month

qplot(data=aniversarios, x=aniversarios$month ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 