# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  



library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("\\\\appserver/UsersDir/User Profiles/rboateng/Documents/Citi Bikes") #sets your working directory to simplify calls to data 

--------------------------------------------------------------------------------
  
  #=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy data sets (csv files) here

Jan_2021 <- read_csv("01.csv")
Feb_2021 <- read_csv("02.csv")
March_2021 <- read_csv("03.csv")
April_2021 <- read_csv("04 new.csv")
May_2021 <- read_csv("05.csv")
June_2021 <- read_csv("06.csv")
July_2021 <- read_csv("07.csv")
Aug_2021 <- read_csv("08.csv")
Sept_2021 <- read_csv("09.csv")
Oct_2021 <- read_csv("10.csv")

--------------------------------------------------------------------------------
  
  #====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, 
# they DO need to match perfectly before we can use a command to join them into one file.

colnames(Jan_2021)
colnames(Feb_2021)
colnames(March_2021)
colnames(April_2021)
colnames(May_2021)
colnames(June_2021)
colnames(July_2021)
colnames(Aug_2021)
colnames(Sept_2021)
colnames(Oct_2021)

# Inspect the data-frames and look for incongruencies

str(Jan_2021)
str(Feb_2021)
str(March_2021)
str(April_2021)
str(May_2021)
str(June_2021)
str(July_2021)
str(Aug_2021)
str(Sept_2021)
str(Oct_2021)

# Convert ride_id and rideable_type to character so that they can stack correctly


Jan_2021 <-  mutate(Jan_2021, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)) 
Feb_2021 <-  mutate(Feb_2021, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type))   
March_2021 <-  mutate(March_2021, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
April_2021 <-  mutate(April_2021, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
May_2021 <-  mutate(May_2021, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)) 
June_2021 <-  mutate(June_2021, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
July_2021 <-  mutate(July_2021, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type))   
Aug_2021 <-  mutate(Aug_2021, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)) 
Sept_2021 <-  mutate(Sept_2021, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type)) 
Oct_2021 <-  mutate(Oct_2021, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame

all_trips <- bind_rows(Jan_2021, Feb_2021, March_2021, April_2021, May_2021,
                       June_2021, July_2021, Aug_2021, Sept_2021, Oct_2021)

# Remove lat, long as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
--------------------------------------------------------------------------------
  #======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created



colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# There are a few problems we will need to fix:

# (1) The data can only be aggregated at the ride-level, which is too granular. 
# We will want to add some additional columns of data -- such as day, month, year --
# that provide additional opportunities to aggregate the data.

# (2) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column.
# We will add "ride_length" to the entire dataframe for consistency.
# (3) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.



# Add columns that list the date,
all_trips$date <- as.Date(all_trips$started_at, format = "%m/%d/%Y") #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m") #month
all_trips$day <- format(as.Date(all_trips$date), "%d") #day
all_trips$year <- format(as.Date(all_trips$date), "%Y") #Year
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") #Week

# This will allow us to aggregate ride data for each month, day, or year ...
#before completing these operations we could only aggregate at the ride level

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips$ride_length) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips$ride_length)

# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users

aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips$day_of_week <-
  ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
-----------------------------------------------------------------------------------------------
  # analyze ridership data by type and weekday
  
  all_trips %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)				       


# Let's visualize the number of rides by rider type
all_trips %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)	 %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)	 %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, 
counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual +
                      all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'C:\\Users\\rboateng\\Documents\\avg_ride_length.csv')