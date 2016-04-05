suppressMessages(library(ggplot2))
#install.packages('XLConnect', dependencies = T)
suppressMessages(library(XLConnect))
#install.packages('tidyr', dependencies = T)
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))


killedEpidemicRaw <- readWorksheet(loadWorkbook("indicator_epidemic killed.xlsx"),sheet=1)

colnames(killedEpidemicRaw)[1] <- "country"

killedCountryEpidemic = gather(killedEpidemicRaw, "year", "epidemic", 2:39)

killedCountryEpidemic$year <- as.numeric((substr(killedCountryEpidemic$year,2,5)))

unique(killedCountryEpidemic$year)

dim(killedCountryEpidemic)

killedFloodRaw <- readWorksheet(loadWorkbook("indicator_flood killed.xlsx"),sheet=1)

colnames(killedFloodRaw)[1] <- "country"

killedCountryFlood = gather(killedFloodRaw, "year", "flood", 2:40)

killedCountryFlood$year <- as.numeric((substr(killedCountryFlood$year,2,5)))

killedCountryFlood <- arrange(killedCountryFlood,country, year)

dim(killedCountryFlood)

killedCountry <- merge (killedCountryFlood, killedCountryEpidemic)

dim(killedCountry)

killedCountryBrazil <- killedCountry %>%
  filter(country=='Brazil') 
  
by (killedCountry$flood, killedCountry$year, summary)
by (killedCountry$epidemic, killedCountry$year, summary)

cor(killedCountry$flood, killedCountry$epidemic)

set.seed(9998)
sample.ids <- sample(killedCountry$country, 16)

ggplot(aes(x=year),
       data = subset(killedCountry, country %in% sample.ids)
       ) +
  facet_wrap(~ country) +
  ylab("Mortality") +
  geom_line(aes(y=flood,color="Flood")) +
  geom_line(aes(y=epidemic,color="Epidemic") )

ggplot(aes(x=year),
       data = subset(killedCountry, country %in% sample.ids)) +
  ylab("Mortality") +
  facet_wrap(~ country) +
  geom_smooth(aes(y=flood,color="Flood")) +
  geom_smooth(aes(y=epidemic,color="Epidemic") )

ggsave('lesson5_gapminder_flood_epidemic_year.png')

ggplot(aes(x=flood, y=epidemic),
       data = subset(killedCountry, country %in% sample.ids)) +
  facet_wrap(~ country) +
  geom_point()

ggsave('lesson5_gapminder_flood_x_epidemic.png')

sum(killedCountry$epidemic)/ sum(killedCountry$flood)

ggplot(aes(x=year, y=epidemic/flood),
       data = subset(killedCountry, flood>0)) +
  ylim(0,10) +
  geom_point(alpha=0.3)

