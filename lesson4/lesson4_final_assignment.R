library(ggplot2)
data("diamonds")

str(diamonds)

ggplot(aes(x=x,y=price), data=diamonds) +
  geom_point() +
  coord_cartesian(xlim=c(3,12))

cor (diamonds$x, diamonds$price)

cor (diamonds$y, diamonds$price)

cor (diamonds$z, diamonds$price)

ggplot(aes(x=depth,y=price), data=diamonds) +
  geom_point() 
  

ggplot(aes(x=depth,y=price), data=diamonds) +
  geom_point(alpha=1/100) +
  scale_x_continuous(breaks=seq(0,80,2))

cor(diamonds$depth, diamonds$price)

ggplot(aes(x=carat,y=price), data=diamonds) +
  geom_point()+
  xlim(0, quantile(diamonds$carat, 0.99))+
  ylim(0, quantile(diamonds$price, 0.99))

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x=volume,y=price), data=diamonds) +
  geom_point()

library(plyr)

count(diamonds$volume == 0)

diamonds_filter = subset(diamonds, volume>0 & volume<=800)

cor(diamonds_filter$volume, diamonds_filter$price)
  
ggplot(aes(x=volume,y=price), data=diamonds_filter) +
  geom_point(alpha=1/10)+ 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)

library(dplyr)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean (price),
            median_price = median (price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

p1 = ggplot(diamonds_mp_by_clarity, aes(x=clarity, y=mean_price)) + geom_bar(stat="identity")
p2 = ggplot(diamonds_mp_by_color, aes(x=color, y=mean_price)) + geom_bar(stat="identity")

library("gridExtra")

grid.arrange(p1,p2, ncol=1)

diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))

p3 = ggplot(diamonds_mp_by_cut, aes(x=cut, y=mean_price)) + geom_bar(stat="identity")