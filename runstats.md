
## quick formulas; build a runstats package later

mph_to_pace <- function(mph){
  raw_pace <- 60/mph
  raw_min <- floor(raw_pace)
  raw_sec <- round((raw_pace-raw_min)*60)
  # error handling
  if(raw_sec>=60) {raw_min <- raw_min + 1}
  if(raw_sec>=60) {raw_sec <- raw_sec - 60}
  # more error handling
  buffer <- ""
  if(nchar(raw_sec)==1) buffer <- "0"
  pace <- paste0(raw_min, ":", buffer, raw_sec)
  return(pace)
}

pace_to_mph <- function(pace){
  raw_split <- strsplit(pace, ":")
  raw_min <- as.numeric(raw_split[[1]][1])
  raw_sec <- as.numeric(raw_split[[1]][2])
  raw_pace <- raw_min + raw_sec/60
  mph <- 60/raw_pace
  return(mph)
}
