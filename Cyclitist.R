library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("C:/Users/drogi/Documents/R")

#1 collect data
m12_2023 <- read_csv("202312-divvy-tripdata.csv")
m11_2023 <- read_csv("202311-divvy-tripdata.csv")
m10_2023 <- read_csv("202310-divvy-tripdata.csv")
m9_2023 <- read_csv("202309-divvy-tripdata.csv")
m8_2023 <- read_csv("202308-divvy-tripdata.csv")
m7_2023 <- read_csv("202307-divvy-tripdata.csv")
m6_2023 <- read_csv("202306-divvy-tripdata.csv")
m5_2023 <- read_csv("202305-divvy-tripdata.csv")
m4_2023 <- read_csv("202304-divvy-tripdata.csv")
m3_2023 <- read_csv("202303-divvy-tripdata.csv")
m2_2023 <- read_csv("202302-divvy-tripdata.csv")
m1_2023 <- read_csv("202301-divvy-tripdata.csv")

#checking name of the columns for merging into 1
colnames(m1_2023)
colnames(m2_2023)
colnames(m3_2023)
colnames(m4_2023)
colnames(m5_2023)
colnames(m6_2023)
colnames(m7_2023)
colnames(m8_2023)
colnames(m9_2023)
colnames(m10_2023)
colnames(m11_2023)
colnames(m12_2023)

#2 combine data
#combine data
all_trips <- bind_rows(m1_2023,m2_2023,m3_2023,m4_2023,m5_2023,m6_2023,m7_2023,m8_2023,m9_2023,m10_2023,m11_2023,m12_2023)

#droping coordenates rows
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#3 clean data 
colnames(all_trips)#names of the columns
str(all_trips) #structure of the final file
nrow(all_trips)#number of rows
dim(all_trips)#dimension of the data
summary(all_trips)#resume of the data

table(all_trips$member_casual)

#dates in y/m/d and day of the week
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#ride length
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

#transforming the length from factor to numeric
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#removing data from the headquarters and ride equal to zero in another version of dataset
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
str(all_trips_v2)

#4 descriptive analysis
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

summary(all_trips_v2$ride_length)

#comparing members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#avg ride time each day
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#days in the correct order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"))

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creating the wday
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							#number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		#average duration
  arrange(member_casual, weekday)	

#visualization by raider type
all_trips_v2 %>% 
mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
summarise(number_of_rides = n()
,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")



