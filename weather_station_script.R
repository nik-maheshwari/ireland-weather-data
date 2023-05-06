#-------------------------------------------------------------------------------
# Script to consolidate weather station data into one file.
# Objectives -
# 1. Create one unified source of all weather station data from 01/01/2014 - 31/12/2018
# 2. Make sure columns are in the right format
# 3. Each row has a date, time, weather station and weather data associated with it
#-------------------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(magrittr)


# get list of all weather data files in folder
allFiles <- list.files(path = "./", pattern = ".csv$")

# create an empty data.frame to store the consolidated weather station data
all_weather <- data.frame()

for(file in allFiles) {
  
  # read the weather file as lines
  lines <- readLines(paste0("./", file))
  
  # total number of lines
  len <- length(lines)
  
  # 1st line contains the station name
  station <- lines[1]
  
  # split the line on ': ', take the result ([[1]]), and take the 2nd value 
  station_name <- strsplit(station, ": ")[[1]][2]
  
  # some lines have multiple commas (,), so replace them with ""
  station_name <- gsub(",", "", station_name)
  
  # get the line # from which we start getting the actual data
  start <- grep("date,", lines)
  
  # weather data from 'start' position till the end
  weather <- lines[start:len]
  
  # split each line on ',', bind all split lines, convert to data.frame
  df <- data.frame(do.call(rbind, strsplit(weather, ",", fixed=TRUE)))
  
  # convert all factors to characters
  df <- df %>% mutate_if(is.factor, as.character)
  
  # take 1st row as column names
  colnames(df) <- as.character(df[1,])
  
  # delete 1st row
  df <- df[-1,]
  
  # remove any rows containing a missing value (highly advised to use, but can
  # be removed if necessary)
  df <- df[complete.cases(df),]
  
  # backup of df
  df2 <- df
  
  # very important: take the 1st row and 1st column value (date)
  # check the format (whether dd-mmm-yyyy or dd/mm/yyyy) and change to time 
  # format accordingly.
  # grepl checks for '-' in the first 'date' value
  if(grepl("-", df[1,1], fixed = TRUE)) {
    df2[[1]] <- as.POSIXct(strptime(df2[,1], format = "%d-%B-%Y %H:%M"), 
                           tz = "Europe/London")  # 24-hour
  } else {
    df2[[1]] <- as.POSIXct(strptime(df2[,1], format = "%d/%m/%Y %H:%M", 
                                    tz = "Europe/London")) # 24-hour
  }
  
  # remove duplicated columns (several columns called 'ind')
  df2 <- df2[, !duplicated(colnames(df2), fromLast = TRUE)]
  
  # remove remaining 'ind' column
  df2$ind <- NULL
  
  # convert character columns to numeric (everything apart from date column
  # contains numeric values)
  df2 <- df2 %>% mutate_if(is.character, as.numeric)
  
  # make a new column containing the station name
  df2$station <- station_name
  
  # only take rows between 2014 and 2018, inclusive. Change this for your needs
  sample <- df2[(df2$date >= "2014-01-01" & df2$date <= "2019-01-01"), ]
  
  # bind the values to a global data.frame
  all_weather <- bind_rows(all_weather, sample)
  
  # call garbage collector to refresh memory and not let your RStudio crash
  gc()
}

# remove rows where all columns have NA values
# all_weather <- final_df
all_weather <- all_weather[rowSums(is.na(all_weather)) != ncol(all_weather), ]
all_weather$station <- toupper(all_weather$station)
str(all_weather)
tail(all_weather)
head(all_weather)

sort(unique(all_weather$station)) # get unique name of stations
data.frame(table(all_weather$station)) # get number of rows per station
