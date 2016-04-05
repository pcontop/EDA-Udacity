suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

pf <- read.delim('pseudo_facebook.tsv')


pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
group_by(gender, age) %>%
  summarise(mean_friend_count = mean (friend_count),
            median_friend_count = median (friend_count),
            n = n()) %>%
  ungroup() %>%
  arrange(gender, age)


ggplot(aes(x=age, y=median_friend_count), data=pf.fc_by_age_gender) + geom_line(aes(color=gender))

pf$year_joined <- floor(2014 - pf$tenure/365 )



