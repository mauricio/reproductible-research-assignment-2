library(plyr)

filteredData <- data[data$year > 1992,]

consequences <- ddply(
  filteredData, 
  .(event), 
  summarise, 
  total_deaths=sum(FATALITIES),
  total_injuries=sum(INJURIES),
  total_health_events= sum(INJURIES) + sum(FATALITIES),
  total_damage=round(sum(CROPDMG) + sum(PROPDMG)) %/% 1000
  )
healthConsequences <- consequences[with(consequences, order(-total_health_events)),]
costConsequences <- consequences[with(consequences, order(-total_damage)),]

topHealthEvents <- head(healthConsequences$event, 5)
topCostEvents <- head(costConsequences$event, 5)

consequencesByYear <- ddply(
  filteredData, 
  .(event, year), 
  summarise, 
  total_deaths=sum(FATALITIES),
  total_injuries=sum(INJURIES),
  total_health_events= sum(INJURIES) + sum(FATALITIES),
  total_damage=round(sum(CROPDMG) + sum(PROPDMG)) %/% 1000
)