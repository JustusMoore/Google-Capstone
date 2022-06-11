#SCENARIO FOR PROJECT#

#You are an analyst working for a bike sharing company named Cyclistic.
#You are tasked with looking at how annual members(i.e. subscribers) and casual members (i.e.,Customers) use Cyclistic bikes differently. 
#Goal is to see how to convert customers into subscribers


#PREPARE PROCESS#

#Load necessary packages 
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

#Import data
Q1_2019 <- read_csv("Capstone/Divvy_Trips_2019_Q1/Divvy_Trips_2019_Q1.csv")
Q2_2019 <- read_csv("Capstone/Divvy_Trips_2019_Q2/Divvy_Trips_2019_Q2.csv")
Q3_2019 <- read_csv("Capstone/Divvy_Trips_2019_Q3/Divvy_Trips_2019_Q3.csv")
Q4_2019 <- read_csv("Capstone/Divvy_Trips_2019_Q4/Divvy_Trips_2019_Q4.csv")

#CLEANING PROCESS#

#View structure of data
str(Q1_2019)
str(Q2_2019)
str(Q3_2019)
str(Q4_2019)

#When briefly looking at the tables, I noticed that column names in Q2 data are different. Need to rename the columns in Q2 data set to match the others.
Q2_2019_Renamed <- rename(Q2_2019, 
                    trip_id=`01 - Rental Details Rental ID`,
                    start_time=`01 - Rental Details Local Start Time`,
                    end_time=`01 - Rental Details Local End Time`,
                    bikeid=`01 - Rental Details Bike ID`,
                    tripduration=`01 - Rental Details Duration In Seconds Uncapped`,
                    from_station_id=`03 - Rental Start Station ID`,
                    from_station_name=`03 - Rental Start Station Name`,
                    to_station_id=`02 - Rental End Station ID`,
                    to_station_name=`02 - Rental End Station Name`,
                    usertype=`User Type`,
                    gender=`Member Gender`,
                    birthyear=`05 - Member Details Member Birthday Year`)

#Confirm that the new table is consistent with others.
View(Q2_2019_Renamed)

#Combine all quarterly data into one data set
all_trip_data <- rbind(Q1_2019,Q2_2019_Renamed,Q3_2019,Q4_2019)
View(all_trip_data)

#Create columns that list date, month, year, and day of week. This will enable us to analyze trends for days of the week.
all_trip_data$date <- as.Date(all_trip_data$start_time)
all_trip_data$month <- format(as.Date(all_trip_data$date), "%m")
all_trip_data$day <- format(as.Date(all_trip_data$date), "%d")
all_trip_data$year <- format(as.Date(all_trip_data$date), "%Y")
all_trip_data$day_of_week <- format(as.Date(all_trip_data$date), "%A")

#Add a ride length column, in seconds, for analysis
all_trip_data$ride_length <- difftime(all_trip_data$end_time,all_trip_data$start_time, units = c("sec"))

#Convert ride length to numeric for cleaning. Have to convert from factor to character first and then from character to numeric
all_trip_data$ride_length <- as.numeric(as.character(all_trip_data$ride_length))

#Trips less than 0 are considered bad data and need to be removed from data frame.
sum(all_trip_data$ride_length<0)
all_trips_v2 <- all_trip_data[!(all_trip_data$ride_length<0),]
sum(all_trips_v2$ride_length<0)

#ANALYSIS PROCESS --> all calculations in seconds 

#Descriptive analysis of ride length for customers and subscribers
by(all_trips_v2$ride_length,list(all_trips_v2$usertype),mean)
by(all_trips_v2$ride_length,list(all_trips_v2$usertype),max)
by(all_trips_v2$ride_length,list(all_trips_v2$usertype),min)

#Ridership data by user type and weekday
usertype_and_weekday <- all_trips_v2 %>% 
       mutate(day_of_week = wday(start_time, label = TRUE)) %>%
       group_by(usertype, day_of_week) %>% 
       summarise(number_of_ride = n(), average_duration =  mean(ride_length)) %>%  
       arrange(usertype, day_of_week)

#VISUALIZATION PROCESS

#Visualize average duration for users throughout the week
ggplot(usertype_and_weekday,aes(x=day_of_week,y=average_duration,fill=usertype))+
  geom_col(position="dodge")+
  labs(title = "Average Ride Time",y="Ride Time (Sec)", x = "Day of Week")

#Visualize number of rides throughout week
ggplot(usertype_and_weekday,aes(x=day_of_week,y=number_of_ride,fill=usertype))+
  geom_col(position="dodge")+
  scale_y_continuous(limits=c(0,500000),breaks = seq(0,500000,25000))+
  labs(title = "Number of Rides", x="Day of Week", y="Number of Rides")

#CONCLUSION#

#How do the user types differ?
  #Based on the Average Ride Time visualization, it is clear that Customers have a much longer average ride time than subscribers, despite the time of week. More specifically, the average ride time for Customers tend to be between 50-60 minutes. On the other hand, the average ride time for subscribes is always under 17 minutes.
  #Based on the Number of Rides visualization, we can see that Subscribes make up a much larger portion of the number of rides. Taking a closer look at subscribers, we can see that they tend to take more rides during the weekday and see drops during the weekend.On the other hand, Customers tend to take most of their rides during the weekend.

#Recommendations to convert Customers to subscribers 
  # Since Customers tend to have a longer average ride time, creating Subscriber discounts for longer rides may encourage Customers to purchase a membership.
  # We also know that Customers tend to take most of their trips on the weekend. With that in mind, there could be discounted rates for members on the weekend.
  # Instead of discount rates on weekends, there could be a lower tier membership just for the weekends. This could possibly attract Customers to convert to a membership.