
####### imputation in last section in sloppy... need to impose some logic to check if new activity.... perhaps subset then go off where elapsed seconds = 0??

## setup
library(XML)
library(lubridate)
my_path <- "C:/Users/Sugden Family/Documents/Garmin/data/"

import_list <- c(
  # ## cycling activities
  # "cycling_2016-04-01_01", "cycling_2016-04-01_02", "cycling_2016-04-04_01", 
  # "cycling_2016-04-04_02", "cycling_2016-04-05_01", "cycling_2016-04-05_02", 
  # "cycling_2016-04-05_03", "cycling_2016-05-10_01", "cycling_2016-05-10_02", 
  # "cycling_2016-06-24_01", "cycling_2016-06-24_02", 
    
  ## running activities
  "running_2015-04-11", "running_2015-04-18", "running_2015-05-09", 
  "running_2016-01-14", "running_2016-01-30", "running_2016-02-06", 
  "running_2016-02-13_01", "running_2016-02-13_02", "running_2016-02-15", 
  "running_2016-02-20", "running_2016-03-05", "running_2016-03-12", 
  "running_2016-03-16", "running_2016-03-19", "running_2016-03-26", 
  "running_2016-04-02", "running_2016-04-09", "running_2016-04-16", 
  "running_2016-04-23", "running_2016-04-30", "running_2016-05-07_01", 
  "running_2016-05-07_02", "running_2016-05-09", "running_2016-05-14", 
  "running_2016-05-28", "running_2016-06-02", "running_2016-06-11", 
  "running_2016-06-14", "running_2016-07-04", "running_2016-07-06", 
  "running_2016-07-07", "running_2016-07-09", "running_2016-07-12", 
  "running_2016-07-13", #"running_2016-07-16", 
  "running_2016-07-18", "running_2016-08-13", "running_2016-08-27", 
  "running_2016-09-03", "running_2016-09-06", "running_2016-09-10", 
  "running_2016-09-17")

## loop to import XML-formatted activities and then package results into data frame
for(i in 1:length(import_list)){
  ## import XML
  xml <- xmlParse(paste0(my_path, import_list[i], ".tcx"))
  
  ## parse XML
  parsed <- xmlToDataFrame(nodes <- getNodeSet(xml, "//ns:Trackpoint", "ns"), stringsAsFactors=F)
  
  ## clean up time stamp
  parsed$Date <- substr(parsed$Time,1,10)
  parsed$Time <- substr(parsed$Time,12,19)
  parsed$Elapsed_Seconds <- as.numeric(substr(parsed$Time,1,2)) * 60 * 60 + as.numeric(substr(parsed$Time,4,5)) * 60 + as.numeric(substr(parsed$Time,7,8))
  parsed$Elapsed_Seconds <- parsed$Elapsed_Seconds - min(parsed$Elapsed_Seconds) # normalize to zero
  ## quick hack to get around dashboard issues...
  parsed$Time <- gsub(" UTC", "", gsub("1970-01-01 ", "", as.character(as_date(hms(parsed$Time) - hms(parsed$Time[1])))))
  parsed$Time[1] <- "00:00:01"
  
  ## impute heart rate, if needed
  if(is.na(parsed$HeartRateBpm[1])) {parsed$HeartRateBpm[1] <- parsed$HeartRateBpm[!is.na(parsed$HeartRateBpm)][1]}
  for(j in 2:nrow(parsed)) {
    if(is.na(parsed$HeartRateBpm[j])) {parsed$HeartRateBpm[j] <- parsed$HeartRateBpm[j-1]}
  }; rm(j)
  
  ## generate new features
  parsed$Distance_Miles <- as.numeric(parsed$DistanceMeters) * 0.000621371  # 1 mile = 0.000621371 meters
  parsed$Delta_Miles <- c(0,diff(parsed$Distance_Miles))
  parsed$Delta_Time <- c(0,diff(parsed$Elapsed_Seconds / 3600))             # 3600 seconds = 1 hour
  parsed$MPH <- parsed$Delta_Miles/parsed$Delta_Time
  parsed$MPH[1] <- 0
  
  ## drop Extensions field (only in 'running' data sets; code of no effect to 'cycling' data sets)
  parsed$Extensions <- NULL
  
  ## label activity
  parsed$Activity <- import_list[i]
  
  ## initialize data frame
  if(i==1) {df <- parsed}
  
  ## update data frame with new XML data set
  else {df <- rbind(df, parsed)}
}

## split position field into latitude and longitude
lat_long <- data.frame(do.call('rbind', strsplit(as.character(df$Position),'-',fixed=TRUE)), stringsAsFactors=F)
names(lat_long) <- c("Latitude", "Longitude")

## add latitude and longitude back to data frame and remove redundant position field
df$Latitude <- as.numeric(lat_long$Latitude)
df$Longitude <- as.numeric(lat_long$Longitude)
df$Position <- NULL

## impute missing
if(is.na(df$AltitudeMeters[1])) {
  df$AltitudeMeters[1] <- df$AltitudeMeters[!is.na(df$AltitudeMeters)][1]
  df$Latitude[1] <- df$Latitude[!is.na(df$Latitude)][1]
  df$Longitude[1] <- df$Longitude[!is.na(df$Longitude)][1]
}
for(j in 2:nrow(df)) {
  if(is.na(parsed$AltitudeMeters[j])) {
    df$AltitudeMeters[j] <- df$AltitudeMeters[j-1]
    df$Latitude[j] <- df$Latitude[j-1]
    df$Longitude[j] <- df$Longitude[j-1]
  }
}; rm(j)

## slight re-arrange
df <- data.frame(Time=df$Time, Date=df$Date, df[,!(names(df) %in% c("Time", "Date"))])

## save out detailed forerunner log
write.csv(df, "detailed_forerunner_log.csv", row.names=F)
