## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(readr)
library(knitr)
library(hms)


## ------------------------------------------------------------------------------------------
oct22_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202210-divvy-tripdata.csv")
nov22_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202211-divvy-tripdata.csv")
dec22_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202212-divvy-tripdata.csv")
jan23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202301-divvy-tripdata.csv")
feb23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202302-divvy-tripdata.csv")
mar23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202303-divvy-tripdata.csv")
apr23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202304-divvy-tripdata.csv")
may23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202305-divvy-tripdata.csv")
jun23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202306-divvy-tripdata.csv")
jul23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202307-divvy-tripdata.csv")
aug23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202308-divvy-tripdata.csv")
sep23_df <- read_csv("C:/Users/Asia/Documents/Project_cyclist/divvy-tripdata/202309-divvy-tripdata.csv")


## ------------------------------------------------------------------------------------------
tripdata_df <- bind_rows(oct22_df, nov22_df, dec22_df, jan23_df, feb23_df, mar23_df, apr23_df, may23_df, jun23_df, jul23_df, aug23_df, sep23_df)


## ------------------------------------------------------------------------------------------
head(tripdata_df)
colnames(tripdata_df)
str(tripdata_df)
skim_without_charts(tripdata_df)
summary(tripdata_df)


## ------------------------------------------------------------------------------------------
cyclistic_df <- tripdata_df


## ------------------------------------------------------------------------------------------
cyclistic_df$ride_length <- difftime(tripdata_df$ended_at, tripdata_df$started_at, units = "mins")
cyclistic_df <- cyclistic_df %>% 
  mutate(ride_length = as.numeric(ride_length))
is.numeric(cyclistic_df$ride_length) 


## ------------------------------------------------------------------------------------------
cyclistic_df$date <- as.Date(cyclistic_df$started_at) 
cyclistic_df$day_of_week_num <- wday(cyclistic_df$date, week_start = 1)
days_en <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
cyclistic_df$day_of_week <- days_en[cyclistic_df$day_of_week_num]
cyclistic_df$day_of_week_num <- NULL


## ------------------------------------------------------------------------------------------
cyclistic_df$month <- format(as.Date(cyclistic_df$date), "%m")


## ------------------------------------------------------------------------------------------
cyclistic_df$day <- format(as.Date(cyclistic_df$date), "%d") 


## ------------------------------------------------------------------------------------------
cyclistic_df$year <- format(as.Date(cyclistic_df$date), "%Y") 


## ------------------------------------------------------------------------------------------
cyclistic_df$time <- format(as.Date(cyclistic_df$date), "%H:%M:%S")
cyclistic_df$time <- as_hms((cyclistic_df$started_at))


## ------------------------------------------------------------------------------------------
cyclistic_df$hour <- hour(cyclistic_df$time)


## ------------------------------------------------------------------------------------------
cyclistic_df <- cyclistic_df %>% mutate(season=
                                                        case_when(month == "03"~ "Spring",
                                                                  month == "04"~ "Spring",
                                                                  month == "05"~ "Spring",
                                                                  month == "06"~ "Summer",
                                                                  month == "07"~ "Summer",
                                                                  month == "08"~ "Summer",
                                                                  month == "09"~ "Fall",
                                                                  month == "10"~ "Fall",
                                                                  month == "11"~ "Fall",
                                                                  month == "12"~ "Winter",
                                                                  month == "01"~ "Winter",
                                                                  month == "02"~ "Winter")
)


## ------------------------------------------------------------------------------------------
cyclistic_df <- cyclistic_df %>% mutate(time_of_day=
                                                        case_when(hour == "0" ~ "Night",
                                                                  hour == "1" ~ "Night",
                                                                  hour == "2" ~ "Night",
                                                                  hour == "3" ~ "Night",
                                                                  hour == "4" ~ "Night",
                                                                  hour == "5" ~ "Morning",
                                                                  hour == "6" ~ "Morning",
                                                                  hour == "7" ~ "Morning",
                                                                  hour == "8" ~ "Morning",
                                                                  hour == "9" ~ "Morning",
                                                                  hour == "10" ~ "Morning",
                                                                  hour == "11" ~ "Morning",
                                                                  hour == "12" ~ "Afternoon",
                                                                  hour == "13" ~ "Afternoon",
                                                                  hour == "14" ~ "Afternoon",
                                                                  hour == "15" ~ "Afternoon",
                                                                  hour == "16" ~ "Afternoon",
                                                                  hour == "17" ~ "Afternoon",
                                                                  hour == "18" ~ "Evening",
                                                                  hour == "19" ~ "Evening",
                                                                  hour == "20" ~ "Evening",
                                                                  hour == "21" ~ "Evening",
                                                                  hour == "22" ~ "Night",
                                                                  hour == "23" ~ "Night")
                                                      
)


## ------------------------------------------------------------------------------------------
cyclistic_df <- distinct(cyclistic_df) 


## ------------------------------------------------------------------------------------------
cyclistic_df <- na.omit(cyclistic_df)


## ------------------------------------------------------------------------------------------
cyclistic_df <- cyclistic_df %>%  
  select(-c(ride_id, start_station_id,end_station_id,start_lat,start_lng,end_lat,end_lng)) 


## ------------------------------------------------------------------------------------------
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <=0),]


## ------------------------------------------------------------------------------------------
head(cyclistic_df)
colnames(cyclistic_df)
str(cyclistic_df)
skim_without_charts(cyclistic_df)
summary(cyclistic_df)


## ------------------------------------------------------------------------------------------
nrow(cyclistic_df)


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  count(member_casual)


## ------------------------------------------------------------------------------------------

cyclistic_df %>%
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

cyclistic_df %>% 
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)


## ------------------------------------------------------------------------------------------
cyclistic_df %>%
  group_by(day_of_week) %>% 
  count(day_of_week)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  count(day_of_week)


## ------------------------------------------------------------------------------------------

cyclistic_df %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  count(time_of_day)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

cyclistic_df %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

cyclistic_df %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

cyclistic_df %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

cyclistic_df %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  count(hour) %>%
  print(n=24)

cyclistic_df %>% 
  group_by(member_casual) %>% 
  count(hour) %>%
  print(n=48)


## ------------------------------------------------------------------------------------------
cyclistic_df %>%
  count(month) 

cyclistic_df %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24)


## ------------------------------------------------------------------------------------------

cyclistic_df %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)


cyclistic_df %>%
  filter(season == "Spring") %>% 
  count(season)


cyclistic_df %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)


cyclistic_df %>%
  filter(season == "Summer") %>% 
  count(season)


cyclistic_df %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)


cyclistic_df %>%
  filter(season == "Fall") %>% 
  count(season)


cyclistic_df %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)


cyclistic_df %>%
  filter(season == "Winter") %>% 
  count(season)


cyclistic_df %>%
  group_by(season, member_casual) %>% 
  count(season)


cyclistic_df %>%
  group_by(season) %>% 
  count(season)



## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>%
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------

cyclistic_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  group_by(rideable_type) %>% 
  summarise(average_ride_length = mean(ride_length))



## ------------------------------------------------------------------------------------------
cyclistic_df %>%
group_by(hour, member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  print(n=48) 

cyclistic_df %>% 
  group_by(hour) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  print(n=24) 


## ------------------------------------------------------------------------------------------

cyclistic_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  group_by(day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  group_by(time_of_day) %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(time_of_day == "Morning") %>% 
  summarise(average_ride_length = mean(ride_length))



## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(time_of_day == "Evening") %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------

cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(time_of_day == "Night") %>% 
  summarise(average_ride_length = mean(ride_length))



## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(month, member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  print(n=24)  #lets you view entire tibble

cyclistic_df %>% 
  group_by(month) %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(season, member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  group_by(season) %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(season == "Spring") %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(season == "Summer") %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(season == "Fall") %>% 
  summarise(average_ride_length = mean(ride_length))


## ------------------------------------------------------------------------------------------
cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise(average_ride_length = mean(ride_length))

cyclistic_df %>% 
  filter(season == "Winter") %>% 
  summarise(average_ride_length = mean(ride_length))

