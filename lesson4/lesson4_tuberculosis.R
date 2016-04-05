library(ggplot2)
install.packages('XLConnect', dependencies = T)
library(XLConnect)
install.packages('tidyr', dependencies = T)
library(tidyr)
library(dplyr)


tubRaw <- readWorksheet(loadWorkbook("indicator_all tb deaths per 100000.xlsx"),sheet=1)

colnames(tubRaw)[1] <- "country"

tubCountry = gather(tubRaw, "year", "occurrences", 2:19)

tubCountry$year <- as.numeric((substr(tubCountry$year,2,5)))

ggplot(aes(x=year, y=occurrences, color = country), data=tubCountry) + 
  geom_line()

tbCountryPerYear <- tubCountry %>%
  group_by(country) %>%
  summarise(mean_mort = mean (occurrences),
            median_mort = median (occurrences),
            min_occurrences = min(occurrences),
            max_occurrences = max(occurrences),
            n = n()) %>%
  arrange(country)



tbCountryPerMortality = tbCountryPerYear %>% arrange(mean_mort)

countryMin = tbCountryPerMortality[1,1]
countryMin2 = tbCountryPerMortality[2,1]

countryMax = tbCountryPerYear[dim(tbCountryPerMortality)[1],1]
countryMax2 = tbCountryPerYear[dim(tbCountryPerMortality)[1]-1,1]

countryMedian = tbCountryPerMortality[dim(tbCountryPerMortality)[1]/2,1]
countryMedian2 = tbCountryPerMortality[dim(tbCountryPerMortality)[1]+1/2,1]

countriesExample = subset(tubCountry, 
                            country == countryMax[[1]] |
                            country == countryMax2[[1]] |
                            country==countryMin[[1]] |
                            country==countryMin2[[1]] | 
                            country==countryMedian[[1]]|
                            country==countryMedian2[[1]]
                          )

ggplot(aes(x=year, y=occurrences, color = country), data=countriesExample) + 
  geom_line()