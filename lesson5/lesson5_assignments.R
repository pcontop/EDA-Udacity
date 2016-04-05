library(ggplot2)
data("diamonds")

ggplot(aes(x=price, fill=cut), data=diamonds) +
  geom_histogram() +
  facet_wrap(~color) +
  scale_fill_brewer(type = 'qual')

ggplot(aes(x=table, y=price), data=diamonds) +
  geom_point(aes(color=cut)) +
  scale_color_brewer(type = 'qual') +
  scale_x_continuous(breaks=seq(0,90,2))

ggplot(aes(x=x*y*z, y=price), 
       data=diamonds %>% filter(diamonds$price < quantile(diamonds$price, 0.99))) +
  geom_point(aes(color=clarity))  +
  scale_color_brewer(type = 'div')  +
  xlim(0, 400) + 
  xlab("Volume") +
  scale_y_log10()
  

pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, c(2004,2009,2011,2012,2014))

pf.friends_by_tenure <-  pf %>%
  filter(!is.na(prop_initiated)) %>%
  group_by(tenure, year_joined.bucket) %>%
  summarise(median_prop_initiated=median(prop_initiated)) %>%
  ungroup()

ggplot(aes(x=tenure,y=median_prop_initiated), data=pf.friends_by_tenure) +
  geom_line(aes(color=pf.friends_by_tenure$year_joined.bucket)) 
  

ggplot(aes(x=tenure,y=median_prop_initiated), data=pf.friends_by_tenure) +
  geom_smooth(aes(color=pf.friends_by_tenure$year_joined.bucket)) 

by(pf$prop_initiated, pf$year_joined.bucket, summary)

ggplot(aes(x=cut, y=price/carat), data=subset(diamonds,carat>0)) +
  facet_wrap(~clarity)+
  scale_color_brewer(type = 'div') +
  geom_jitter(aes(color=color))


