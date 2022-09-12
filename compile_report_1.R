##INSTALLING required R packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("janitor")
install.packages("skimr")
install.packages("dplyr")
install.packages("rmarkdown")
install.packages("readr")

##LOADING required R packages
library(tidyverse) #data analysis
library(lubridate) #date and time manipulation
library(ggplot2) #data visualization
library(janitor) #data cleaning
library(skimr) #data structure investigation
library(dplyr) #data manipulation
library(rmarkdown) #report
library(readr)

#As all 12 files where unable to load in R studio
# all 12 files have been merged before in 2 parts (AUG to DEC 2021) & (JAN to JUL 2022) AS ( all_trips.csv)

#Collecting Data of al 12 months aug_2021 to jul_2022.
all_trips <- read_csv("all_trips.csv")

# Inspecting the new dataset
skim_without_charts(all_trips) #checking the dataframe structure

#as we can see there are 2 useless column created while binding  all data, thus we will remove it
all_trips<-print(select(all_trips,-c(...1,...2)))

#Data Manipulation
#Renaming columns
all_trips <- rename(all_trips, "type_of_bike"= "rideable_type", "user_type"="member_casual")

#Adding columns
#Adding the ride_length column in secs format
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#converting started_at column from chr to posixlt(datetime) according to current location of company. adding timezone
all_trips$started_at <-print(strptime(all_trips$started_at, "%d-%m-%Y %R",tz="GMT"))
all_trips$ended_at <-print(strptime(all_trips$ended_at, "%d-%m-%Y %R",tz="GMT"))

#Adding columns for date, day, day_of_week, month and year
all_trips$date<- as.Date(all_trips$started_at) #adding trip start date column
all_trips$day <- format(as.Date(all_trips$date), "%d") #adding day column
all_trips$day_of_week <- format(as.Date(all_trips$date), "%a") #adding weekday column
all_trips$month <- format(as.Date(all_trips$date), "%b") #adding month column
all_trips$year <- format(as.Date(all_trips$date), "%Y") #adding year column

#Adding time_of_day column
all_trips$hour <- as.numeric(format(all_trips$started_at, "%H"))
all_trips <- all_trips %>% 
  mutate(time_of_day = case_when(
    hour>=5 & hour <12 ~ "Morning",
    hour>=12 & hour <17 ~ "Afternoon",
    hour>=17 & hour <21 ~ "Evening",
    hour<5 | hour>=21 ~ "Night"
  ))

#Inspect the dataframe
skim_without_charts(all_trips) #getting summary statistics

#Observations
#The minimum value of ride_length is negative, thus making it invalid
#The maximum value of ride_length is 3356649 seconds (932.4 hrs), which is an outlier
#We need to convert ride_length to numeric to perform calculations on it

#Data Transformation
#Converting ride_length to numeric data type
all_trips$ride_length <- as.numeric(all_trips$ride_length)
#Checking if ride_length is converted to numeric
is.numeric(all_trips$ride_length)

#3.4.1 Removing negative ride_length, ride_length less than 60 seconds and ride_length greater than 24 hrs
all_trips_v2 <- all_trips %>% 
  filter(!(ride_length<60)) %>% 
  filter(!(ride_length>86400))

# Checking minimum ride_length
min(all_trips_v2$ride_length)
#Checking maximum ride_length
max(all_trips_v2$ride_length)

#Removing rides with NA’s in end_lat or end_lng
all_trips_v3 <- all_trips_v2 %>% 
  filter(!(is.na(end_lng)) | !(is.na(end_lat)))

#Checking for assumption 3
temp1 <- all_trips_v3 %>% 
  filter(is.na(start_station_name) | is.na(end_station_name))
summary(temp1$ride_length)

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v3$ride_length)

#Comparing casual riders and annual members
#Descriptive analysis on ride_length
all_trips_v3 %>% 
  group_by(user_type) %>% 
  summarise(average_ride_length=mean(ride_length), median_ride_length=median(ride_length), min_ride_length=min(ride_length), max_ride_length=max(ride_length))
#Analyzing total no. of rides
  all_trips_v3 %>% 
  group_by(user_type) %>% 
  summarise(number_of_rides = n())
  
  #Total no. of monthly rides by each user_type
  all_trips_v3 <- all_trips_v3 %>%
    mutate(month =factor(month, levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul")))
  all_trips_v3 %>% 
    group_by(user_type, month) %>% 
    summarise(number_of_rides = n()) %>% 
    arrange(user_type)
  
  #Downloding csv files for backup
  write.csv(all_trips,"all_trips_5901463.csv")
  write.csv(all_trips_v3,"all_trips_v3_5832776.csv")
  #downloding csv file for further analysis in Tableau
  write.csv(temp1,"temp1_1244494.csv")
  
  # Project done by KEVAL KANANI
  #LinkedIn:-  www.linkedin.com/in/keval-kanani1/
  #Tableau_Public:- https://public.tableau.com/app/profile/keval.kanani
  