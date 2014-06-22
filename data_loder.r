zip <- bzfile("repdata-data-StormData.csv.bz2")
data <- read.csv(zip, 
                 strip.white=TRUE, 
                 colClasses=c("NULL","character","NULL","NULL","numeric","character","character","character", "NULL",
"NULL","NULL","character","NULL","NULL","NULL","NULL", "NULL","NULL","NULL","NULL","NULL","NULL","numeric","numeric",
"numeric","NULL", "numeric","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL", "NULL","NULL"))

normalizeEvent <- function (x) toupper(gsub("^\\s+|\\s+$", "", x))

data$EVTYPE <- normalizeEvent(data$EVTYPE)
replacements <- read.csv("replacements.csv", stringsAsFactors=FALSE)

eventFor <- function( evtype ) {
  replacements[replacements$event == evtype,]$actual
}

data$event <- mapply(eventFor, data$EVTYPE)
data$date <- as.Date(data$BGN_DATE, "%m/%d/%Y")
data$year <- as.POSIXlt(data$date)$year+1900
data$decade <- ((as.POSIXlt(data$date)$year+1900) %/% 10) * 10