#USING GTFS FEEDS TO FIND FREQUENCY OF SERVICE AT TRANSIT STOPS ---------------------------------
library(plyr)
library(dplyr)
library(lubridate)

#Import "stop_times.text" and "stops" from city specific GTFS folder
#Most GTFS feeds can be found: https://transitfeeds.com/
stop_times <- read.table("stop_times.txt", header = TRUE, sep = ",", dec = ".", fill = TRUE)
stops <- read.table("stops.txt", header = TRUE, sep = ",", dec = ".", fill = TRUE)
colnames1 <- colnames(stop_times)
colnames2 <- colnames(stops)

# ------------------------------ Stop Times -------------------------------------------------------
#Convert txt file to matrix
names(stop_times) <- as.matrix(stop_times[1, ])
stop_times <- stop_times[-1, ]
head(stop_times)
#Assign names to columns
colnames(stop_times) <- c(colnames1)
head(stop_times)

# ------------------------------ Stops ------------------------------------------------------------
#Convert txt file to matrix
names(stops) <- as.matrix(stops[1, ])
stops <- stops[-1, ]
head(stops)
#Assign names to columns -- note that there are variations in the data - always check the column names before assigning
colnames(stops) <- c(colnames2)
head(stops)

# ------------------------------ SPDF ------------------------------------------------------------
stops$stop_lon <- as.numeric(as.character(stops$stop_lon))
stops$stop_lat <- as.numeric(as.character(stops$stop_lat))
stops <- stops[complete.cases(stops$stop_lat, stops$stop_lon),]
xy <- c("stop_lon", "stop_lat")
xy <- stops[xy]
spdf <- SpatialPointsDataFrame(coords = xy, data = stops,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# -------------------------------------------------------------------------------------------------------------------
#Reset (%H:%M:%S) & store time in x & round time to nearest hour --- note that you can substitute "1 hour" for minutes 
x = as.POSIXlt(stop_times$arrival_time, format = "%H:%M:%S")
stop_times$roundtime <- round_date(x, "1 hour")
stop_times$time <- format(as.POSIXct(strptime(stop_times$`roundtime`,"%Y-%m-%d %H:%M:%S", tz="")), format = "%H:%M:%S")
stop_times$rounddate <- format(as.POSIXct(strptime(stop_times$roundtime,"%Y-%m-%d %H:%M:%S", tz="")), format = "%Y-%m-%d")
#DOW 1-7 ---- assign Day of week --- 1=Sunday, 2=Monday, 3=Tuesday, 4=Wednesday, 5=Thursday, 6=Friday & 7=Saturday
stop_times$DOW <- wday(stop_times$rounddate)
stop_times <- stop_times[, c("stop_id","DOW","time")]
rm(x)

# ------------------------------ Run Frequency Counts ------------------------------------------
#FREQUENCY COUNT FOR ROUNDED TIME AND STOP ID
Frequency_Hourly <- ddply(stop_times, .(stop_times$stop_id, stop_times$time), nrow)
names(Frequency_Hourly) <- c("stop_id", "rounded_time", "frequency")
#FREQUENCY COUNT FOR OVERALL
Frequency_Overall <- ddply(stop_times, .(stop_times$stop_id), nrow)
names(Frequency_Overall) <- c("stop_id", "frequency")
#FREQUENCY COUNT WEEKDAY
Weekday <- subset(stop_times, DOW >= 2 | DOW <= 6,
                  select=c(stop_id, DOW, time))
Frequency_Weekday <- ddply(stop_times, .(Weekday$stop_id, Weekday$time), nrow)
names(Frequency_Weekday) <- c("stop_id", "time", "frequency")
#FREQUENCY COUNT WEEKDAY COMMUTE HOURS --- note that you can select a range of times
Weekday_Commute <- subset(Weekday, time=="09:00:00",
                          select=c(stop_id, DOW, time))
Weekday_Commute <- ddply(Weekday_Commute, .(Weekday_Commute$stop_id, Weekday_Commute$time), nrow)
names(Weekday_Commute) <- c("stop_id", "time", "frequency")

#JOIN STOP IDS TO LONG, LAT & STATION NAME
#Merge two data frames by ID
Frequency_Hourly <- merge(Frequency_Hourly,stops,by="stop_id")
Frequency_Overall <- merge(Frequency_Overall,stops,by="stop_id")
Frequency_Weekday <- merge(Frequency_Weekday,stops,by="stop_id")
Weekday_Commute <- merge(Weekday_Commute,stops,by="stop_id")

#Write Final Files or make a quick visual -------------------------------------------------------
write.csv(Frequency_Hourly, "Frequency_Hourly.csv")
library(mapview)
xy <- Weekday_Commute[, c("stop_lat","stop_lon")]
xy <- xy[,c(2,1)]
Weekday_Commute <- SpatialPointsDataFrame(coords = xy, data = Weekday_Commute,
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
mapview(Weekday_Commute, cex = "frequency", zcol = "frequency", alpha = 0.2, alpha.regions = 0.3)