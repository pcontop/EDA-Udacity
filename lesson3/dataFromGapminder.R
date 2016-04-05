library(ggplot2)
install.packages('XLConnect', dependencies = T)
library(XLConnect)
install.packages('tidyr', dependencies = T)
library(tidyr)

jobRaw <- readWorksheet(loadWorkbook("indicator_t above 15 employ.xlsx"),sheet=1)

colnames(jobRaw)[1] <- "country"

#Treating
job = gather(jobRaw, "year", "percentage", 2:18)

str(substr(job$year,2,5))

job$year <- as.numeric((substr(job$year,2,5)))

str(job$year)

str(job$percentage)

qplot(x=year, y=percentage, data=job, color=country, geom="line")

#That's a complete mess!

#Let's analyze this.

by(job$percent, job$country, summary)

#Country with highest job percentage.
countryHigh = job[which.max(job$percentage),1]

#Country with lowest job percentage.
countryLow = job[which.min(job$percentage),1]

#Reducing countries to a manageble number.
jobCountries = subset(job, country %in% c("Brazil","Argentina","Germany", countryHigh, countryLow))

#Line
qplot(x=year, y=percentage, data=jobCountries, color=country, geom="line")

ggsave('jobContriesLinePercentageYear.png')

#Frequency
qplot(x = percentage, 
      y = ..count.., 
      data = jobCountries,
      binwidth = 11,
      geom="freqpoly",
      color=country
) 
ggsave('jobContriesFreqPercentageContry.png')