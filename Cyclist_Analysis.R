# This program is to complete case study #1 for 
# Coursera Google data analytic certificate.
# We will use data available by Motivate International Inc
# that operates Chicago Divvy bicycle sharing service.
# The objective is to answer the following business task:
# How do annual members and casual riders use Cyclistic bikes differently?


# First step:  Download and load necessary libraries 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("scales")



library(tidiverse)
library(lubridate)
library(hms)
library(dplyr)
library(ggplot2)
library(here)
library(skimr)
library(janitor)
library(scales)

# Second step : Read the .csv files that were downloaded from 
# the website of Motivate International Inc. There are 12 files ,
# one from each month since sep 2020 to aug 2020.
# One thing that i learned here is that you have to put "\\" instead of
# "\" in the path of the file.

# Read file 1 # You have to set your own path to the .csv files inside read.csv()
trip_sep2020 <- read.csv("\\Cyclistic_Data_CSV_FILES\\202009-divvy-tripdata.csv")
# We explore this file with the following commands( Its no necessary to
# use all them but i did to familiarize.

colnames(trip_sep2020)
head(trip_sep2020)
View(trip_sep2020)
glimpse(trip_sep2020)
str(trip_sep2020)
skim_without_charts(trip_sep2020)

# From this first step we see that the files have more than 500.000 rows and we have
# to work with 12 files with possible similar quantities of lines, so we will keep working 
# with R instead of a spreadsheet. Also we note that the names of the columns don not have
# spaces, looks consistent and clear.

# Read file 2
trip_oct2020 <- read.csv("\\Cyclistic_Data_CSV_FILES\\202010-divvy-tripdata.csv")
str(trip_oct2020)

# We will compare column names and data types with the data frame #1 to ensure
# all data frames have the same columns before joining them in only one big data frame.
compare_df_cols_same(trip_sep2020,trip_oct2020)
compare_df_cols(trip_sep2020,trip_oct2020)
# As we can see in the output, all the columns are the same in these 2 data frames.

# Read file 3
trip_nov2020<-read.csv("\\Cyclistic_Data_CSV_FILES\\202011-divvy-tripdata.csv")

# Read file 4
trip_dec2020<-read.csv("\\Cyclistic_Data_CSV_FILES\\202012-divvy-tripdata.csv")

#read file 5
trip_jan2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202101-divvy-tripdata.csv")

#read file 6
trip_feb2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202102-divvy-tripdata.csv")

#read file 7
trip_mar2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202103-divvy-tripdata.csv")

#read file 8
trip_apr2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202104-divvy-tripdata.csv")

#read file 9
trip_may2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202105-divvy-tripdata.csv")

#read file 10
trip_jun2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202106-divvy-tripdata.csv")

#read file 11
trip_jul2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202107-divvy-tripdata.csv")

#read file 12
trip_aug2021<-read.csv("\\Cyclistic_Data_CSV_FILES\\202108-divvy-tripdata.csv")



# Now Let's compare column names and data types between all data frames to ensure
# they have the same columns before joining them in only one big data frame.
# 
compare_df_cols_same(trip_aug2021,trip_jul2021,trip_jun2021,trip_may2021,
                     trip_apr2021,trip_mar2021,trip_feb2021,trip_jan2021,
                     trip_dec2020,trip_nov2020,trip_oct2020,trip_sep2020)

compare_df_cols(trip_aug2021,trip_jul2021,trip_jun2021,trip_may2021,
                     trip_apr2021,trip_mar2021,trip_feb2021,trip_jan2021,
                     trip_dec2020,trip_nov2020,trip_oct2020,trip_sep2020)

# From the output of this two commands we can see that the columns
# "start_station_id" and "end_station_id" are defined as "int"in data frames
# trip_sep2020,trip_oct2020 and trip_nov2020 while in the others are defined 
# as "character".


# Before combine all the datasets into one alone we are going to change
# the datatype of column "start_station_id" and "end_station_id" from "int" 
# to character in data frames trip_sep2020, trip_oct2020 and trip_nov2020.

trip_sep2020$start_station_id <- as.character(trip_sep2020$start_station_id)
trip_sep2020$end_station_id <- as.character(trip_sep2020$end_station_id)
str(trip_sep2020)

trip_oct2020$start_station_id <- as.character(trip_oct2020$start_station_id)
trip_oct2020$end_station_id <- as.character(trip_oct2020$end_station_id)
str(trip_oct2020)

trip_nov2020$start_station_id <- as.character(trip_nov2020$start_station_id)
trip_nov2020$end_station_id <- as.character(trip_nov2020$end_station_id)
str(trip_nov2020)

#We will Combine 12 data frame into 1 data frame.
trip_combined<-dplyr::bind_rows(trip_sep2020,trip_oct2020,trip_nov2020,trip_dec2020,
                                trip_jan2021,trip_feb2021,trip_mar2021,trip_apr2021,
                                trip_may2021,trip_jun2021,trip_jul2021,trip_aug2021)
str(trip_combined)
glimpse(trip_combined)

# STEP 3: We will clean our data
# We will check how many NA total and by column
sum(is.na(trip_combined))
colSums(is.na(trip_combined))

# We will eliminate duplicates rows
trip_combined <- distinct(trip_combined)

# We will eliminate rows that have duplicate data only in the column "ride_id"
trip_combined <- trip_combined[!duplicated(trip_combined$ride_id),]
glimpse(trip_combined)

# We will check if we have rows with ride_id ="" and verify that no row has value=""
trip_combined %>% filter(ride_id=="")
# All the rows have a value in ride_id so we will assume that is a valid ID
#generated by the system.



# FOURT STEP: Analyze

# We will convert the columns "started_at" and "ended at" from "chr" to date "POSIXct".
trip_combined$started_at<-ymd_hms(trip_combined$started_at)
trip_combined$ended_at<-ymd_hms(trip_combined$ended_at)

# We will calculate the duration trip in seconds and put in a new column.
trip_combined$ride_length <- trip_combined$ended_at-trip_combined$started_at

# We will calculate the year and create a new column
trip_combined$year<-format(trip_combined$started_at,format="%Y")

#we will calculate month
trip_combined$month<-format(trip_combined$started_at,format="%b")

# We will calculate the day of the week at each ride started an put in a new column
trip_combined$day_of_week<- wday(trip_combined$started_at,label=TRUE)

#We will calculate the day of the month
trip_combined$day_of_month<-format(trip_combined$started_at,format="%d")

#We will calculate Hour of trip
trip_combined$hour<-format(trip_combined$started_at,format="%H")


#we add a column with duration of trips in minutes
trip_combined$ride_length_min<-as.numeric(trip_combined$ride_length,units="mins")

#We will take a look to our data frame
str(trip_combined)
summary(trip_combined)
# From the output we can see than the minimum ride_length_min is negative,
# that's no good. We need to check.

# We will check the maximum ride length and average trip duration by 
# member_casual and day_of_the_week.

trip_combined %>% group_by(member_casual,day_of_week) %>% 
  summarize(maximun_trip=max(ride_length_min),avg_trip_dur=mean(ride_length_min))
# We see that there is an average trip duration very low on Tuesdays for
# the members, its necessary to go deep to check what happen.

# We check how many trips have a duration of zero or negative to remove 
# them for the file because have data not accurate.
colSums(trip_combined<=0)

# From the result we see that there are 5637 rows with negative value in 
# the column ride_length_min

# We'll check how may trips are below 60 seconds and remove them because
# according to the web page of the company these can be false starts or 
# problems when people are trying to lock the bikes to the dock stations.
sum(trip_combined$ride_length<60)

# Here we can see that there are 80306 trips below 60 second

# We'll remove the rows with ride_length <60 secs and put in a new dataset
trip_combined_clean <- trip_combined %>% filter(ride_length>=60)

# We'll remove data of trips taken by maintenance staff, so we check
#the names of the "start_station"/"end_station" and found some names like :
#"WATSON TESTING - DIVVY","DIVVY CASSETTE REPAIR MOBILE STATION",
# "WEST CHI-WATSON","HUBBARD ST BIKE CHECKING (LBS-WH-TEST)". 
# We assume that names are associated to maintenance.
unique(trip_combined_clean[c("start_station_name")])
unique(trip_combined_clean[c("end_station_name")])
str(trip_combined_clean)

#Here we filter out rows with data of trips taken by maintenance staff.
trip_combined_clean<- trip_combined_clean %>% filter(
                  (!start_station_name %in% c("WATSON TESTING - DIVVY",
                  "DIVVY CASSETTE REPAIR MOBILE STATION","WEST CHI-WATSON",
                  "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)"))&
                  (!end_station_name %in% c("WATSON TESTING - DIVVY",
                  "DIVVY CASSETTE REPAIR MOBILE STATION",
                  "WEST CHI-WATSON","HUBBARD ST BIKE CHECKING (LBS-WH-TEST)")))

summary(trip_combined_clean$ride_length_min)
sum(trip_combined_clean$ride_length<60)


# We'll calculate the average trip duration,maximum trip duration and number of
# trips by type of member
summ_trips_and_length <- trip_combined_clean %>% group_by(member_casual) %>% 
  summarise(maximun_trip=max(ride_length_min),
  avg_trip_dur=mean(ride_length_min),number_of_trips=n()) 

summ_trips_and_length
#Let's plot overall average trip duration by user type
ggplot(summ_trips_and_length)+geom_col(aes(x=member_casual,y=avg_trip_dur,
   fill=member_casual),position="dodge")+
   labs(title="Yearly average trip duration by user type (Mins)",
   x="Member type",y="Average trip duration (Mins)",fill="Member type",
   subtitle = "Data from September 2020 to August 2021")

#Let's plot overall number of trips by user type
ggplot(summ_trips_and_length)+ geom_col(aes(x=member_casual,y=number_of_trips,
  fill=member_casual),position="dodge")+
  labs(title="Yearly Number of trips by user type (Mins)",
  x="Member type",y="Yearly number of trips",fill="Member type",
  subtitle = "Data from September 2020 to August 2021")+
  scale_y_continuous(labels = comma)


  
#Lets calculate the % of difference between average trip duration by user type
overall_trip_dur_by_user <- summ_trips_and_length %>% pull(avg_trip_dur)
perc_diff <- (overall_trip_dur_by_user[1]-overall_trip_dur_by_user[2])/
  overall_trip_dur_by_user[2]*100
perc_diff     

# The results suggest  that the average trip duration for casual users is 
# greater than for member users by 136%. However the number of trips is 
# greater for member users than for casual users.





#Lets plot average trip duration Vs Day of the week for each user type.

trip_combined_clean %>% group_by(member_casual,day_of_week) %>% 
  summarize(maximun_trip=max(ride_length_min),
        avg_trip_dur=mean(ride_length_min),number_of_trips=n()) %>% 
        ggplot(.)+ geom_col(aes(x=day_of_week,y=avg_trip_dur,
        fill=member_casual),position="dodge")+
        labs(title="Average trip duration by weeday and user type (Mins)",
        x="Day of the Week",y="Average trip duration (Mins)",fill="Member type",
        subtitle = "Data from September 2020 to August 2021")
  

# From the chart we note that the average trip duration is 
# greater for casual users than for member users by more than 100% in any day, 
# and the longer duration for casual members are Saturdays and Sundays.

#Lets plot number of trips Vs Day of the week for each user type. 
trip_combined_clean %>% group_by(member_casual,day_of_week) %>% 
  summarize(maximun_trip=max(ride_length_min),
  avg_trip_dur=mean(ride_length_min),number_of_trips=n()) %>% 
  ggplot(.)+ geom_col(aes(x=day_of_week,y=number_of_trips,fill=member_casual),
  position="dodge")+ scale_y_continuous(labels = comma)+
  labs(title="Number of trips by weekday and user type",x="Day of the Week",
  y="Number of trips (Quantity)",fill="Member type",
  subtitle = "Data from September 2020 to August 2021")
                                        
# The chart suggests that the number of trips are relatively steady each day for 
# member user, but for casual members rise considerably during Fridays,
# Saturdays and Sundays.For member users the number of trips during 
# weekdays(Monday to Friday) is greater than for casual users.



#Lets plot the number of trips by weekday and user type but express as
# a percentage.

trip_combined_clean %>% group_by(member_casual,day_of_week) %>% 
  summarize(maximun_trip=max(ride_length_min),
  avg_trip_dur=mean(ride_length_min),number_of_trips=n()) %>% 
  ggplot(.)+ geom_bar(aes(x=day_of_week,y=number_of_trips,fill=member_casual),
  position="fill",stat="identity")+ 
  scale_y_continuous(labels = label_number(scale = 1e+02))+
  labs(title="Daily Percentage of trips by user type",x="Day of the Week",
  y="Percentage of trips (%)",fill="Member type",
  subtitle = "Data from September 2020 to August 2021")


#Lets plot number of trips by hour of the day
ggplot(trip_combined_clean)+geom_bar(aes(x=hour, fill=member_casual),
   position="dodge")+ labs(title="Total yearly number of trips by hour",
   x="hour",y="Number of trips (Quantity)",fill="Member type",
   subtitle = "Data from September 2020 to August 2021")+
   scale_y_continuous(labels = label_comma())

# The times in which the member users bike the most are 6 to 9 in the morning
# and 16 to 19 in the evenings; at 17 as the peak hour. On the other hand, 
# for casual members the number stay relatively low until 8 am, start to 
# grow until 17 and then decrease until 24.




#We will plot percentage of trips by month and user type

trip_combined_clean$month_factor <- factor(trip_combined_clean$month, 
   levels = month.abb)
trip_combined_clean %>% group_by(member_casual,year,month_factor) %>% 
summarise(number_of_trips=n()) %>% arrange(member_casual,year,month_factor) %>%
ggplot()+geom_bar(aes(x=month_factor,y=number_of_trips/sum(number_of_trips)*100,
fill=member_casual),stat="identity",position = "dodge")+
labs(title="Percentage of trips by month and user type",x="Month",
y="Percentage of trips (%)",fill="Member type",
subtitle = "Data from September 2020 to August 2021")

# The percentage of trips by casual members is greater than the one by member 
# users during the months of June-July-August. During the other months, 
# the tendency is contrary. The months with less percentage of trips for both
# user types are December, January and February.


 
# Finally, as the marketing campaign will target only casual members,let's plot
# numbers of trips started by station name for casual members
# to be prepare if some one ask this question during the presentation. 
# We plot the first 20 stations with more percentage of trips. 

trip_str_station <- trip_combined_clean %>%  
  group_by(member_casual,start_station_name) %>% 
  summarise(number_of_trips=n()) %>% arrange(member_casual,-number_of_trips) %>% 
  filter(start_station_name!=""& member_casual=="casual")

trip_str_station <- mutate(trip_str_station,
  perc_trip_station= number_of_trips/sum(trip_str_station$number_of_trips)*100)
#Lets check the percentage represented by the firs 20 stations
sum(trip_str_station[1:20,]$perc_trip_station)
#We can see that they represent 19.78 % of the trips.

# Let's plot the 20 start stations most used by casual members.
twenty_stations <- trip_str_station[1:20,]
twenty_stations <- twenty_stations%>% arrange(twenty_stations$perc_trip_station)
twenty_stations$start_station_name <- factor(twenty_stations$start_station_name,
                                    levels = twenty_stations$start_station_name)

ggplot(twenty_stations)+geom_bar(aes(x=start_station_name, y=perc_trip_station),
  position="dodge",stat="identity",fill = "#FF6666")+coord_flip()+
  labs(title="The twenty station most used by casual members",
  y="Percentage (%)",x="Name of start station",
  subtitle = "Percentage of trips started at each station")


# Based on this work, I prepared a slide presentations with the charts and 
# insights to share my work with the stake holders.
# Thank you for taking time to check this work.





