## Loading packages
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(scales)
library(ggpubr)


## reading the CSV files and creating the data set
##########################################################

df1 <- read.csv("./Data/202106-divvy-tripdata.csv")
df2 <- read.csv("./Data/202107-divvy-tripdata.csv")
df3 <- read.csv("./Data/202108-divvy-tripdata.csv")
df4 <- read.csv("./Data/202109-divvy-tripdata.csv")
df5 <- read.csv("./Data/202110-divvy-tripdata.csv")
df6 <- read.csv("./Data/202111-divvy-tripdata.csv")
df7 <- read.csv("./Data/202112-divvy-tripdata.csv")
df8 <- read.csv("./Data/202201-divvy-tripdata.csv")
df9 <- read.csv("./Data/202202-divvy-tripdata.csv")
df10 <- read.csv("./Data/202203-divvy-tripdata.csv")
df11 <- read.csv("./Data/202204-divvy-tripdata.csv")
df12 <- read.csv("./Data/202205-divvy-tripdata.csv")
rides <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
rm(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)


## Checking thestructure of the data
##########################################################

head(rides)
str(rides)
colnames(rides)


## proportion of annual member rides and casual rides
##########################################################

g1 <- rides %>% count(member_casual) %>%
  group_by(member_casual)
g1

g1 %>% ggplot(aes(y=n, x="", fill = member_casual)) + geom_col() + coord_polar(theta = "y") +
  labs(title = "Cyclistic users' memberships", subtitle = "Number of rides in millions", x = "") +
  scale_y_continuous(labels = comma)

# round(g1$n[2]/(g1$n[1]+g1$n[2])*100,digits = 1)



# different rideable bike presented in the data
rides %>% count(rideable_type)
# rideable type of the bike rides distribution
rides %>% count(rideable_type, member_casual)     #all docked_bike records are for casual riders




## converting data formats
##########################################################

rides$started_at <- ymd_hms(rides$started_at)
rides$ended_at <- ymd_hms(rides$ended_at)


## mutating new columns to data
##########################################################

# mutating dates and hour of the day
new_rides <- rides %>%
  mutate(start_date = as.Date(rides$started_at),
         end_date = as.Date(rides$ended_at),
         start_hour = hour(rides$started_at),
         end_hour = hour(rides$ended_at))

# calculating the trip duration
new_rides <- new_rides %>%
  mutate(ride_duration = difftime(ended_at, started_at, units = c("mins")))

  # Changing ride_duration data type to numeric
new_rides$ride_duration <- as.numeric(new_rides$ride_duration)

# calculating the trip weekday and month
new_rides <- new_rides %>%
  mutate(start_weekday = weekdays(started_at),
         month = month(started_at))
#new_rides %>% count(month)

  # Reordering the Weekdays
new_rides$start_weekday <- as.factor(new_rides$start_weekday)
summary(new_rides$start_weekday)
new_rides$start_weekday <- factor(new_rides$start_weekday,
                                  levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

  # Reordering the months
str(new_rides$month)
new_rides$month <- as.factor(new_rides$month)

levels(new_rides$month) <- list(January = "1", February = "2", March = "3", April = "4",
                                May = "5", June = "6", July = "7", August = "8", September = "9",
                                October = "10", November = "11", December = "12")
summary(new_rides$month)











## Cleaning data
##########################################################

# Removing empty rows and columns
dim(rides)
rides <- remove_empty(rides, which = c("rows", "cols"))

# Checking for data consistency
new_rides %>% count(member_casual)    # There are only two types of membership in the data as expected

new_rides %>%
  filter(end_date < start_date) %>%
  select(ride_id)                     # No start date is after the end date, as expected

new_rides %>%
  filter(ride_duration < 0) %>%
  select(ride_id)                     # There are 139 rides having a negative ride_duration

#Removing incorrect observations from data
new_rides <- new_rides[!(new_rides$ride_duration < 0), ]

new_rides %>%
  filter(ride_duration < 0) %>%
  select(ride_id) %>% dim()           # Data is now cleaned from rides with negative duration


# Exporting the cleaned data as a CSV file for further use:

write.csv(new_rides,"./New_rides.csv", row.names = TRUE)

## Graphs and data insights
##########################################################

# average ride duration by members and casual riders
new_rides %>%
  select(member_casual,ride_duration) %>%
  group_by(member_casual) %>%
  summarize(Average_ride_duration = mean(ride_duration))

# rides based on the hour in each day
new_rides %>% count(start_hour) %>%
  ggplot(aes(start_hour,n/365 )) +
  geom_line(color = "purple", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1:23)) +
  labs(title = 'Average number of rides in each hour in 24 hours') +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=13, face="bold"),
        axis.title.y = element_text(color="black", size=13, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  xlab("Hour") + ylab("Ride count per day")

# rides based on the weekday
new_rides %>%
  ggplot(aes(start_weekday, fill = member_casual)) +
  geom_bar(position = "dodge2") +
  scale_y_continuous(labels = comma) +
  labs(title = 'Total number of rides in each weekday') +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=13, face="bold"),
        axis.title.y = element_text(color="black", size=13, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  xlab("WeekDay") + ylab("Total ride Count")

# rides in each month
new_rides %>%
  ggplot(aes(month, fill = member_casual)) +
  geom_bar(position = "dodge2") +
  labs(title = "Number of rides in each month") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=13, face="bold"),
        axis.title.y = element_text(color="black", size=13, face="bold"),
        axis.text.x = element_text(angle = 90, size=10),
        axis.text.y = element_text(size=10)) +
  xlab("Month") + ylab("Ride Count")





# Busiest stations:

new_rides[c(new_rides$start_station_name != ""),] %>%
  count(start_station_name) %>% arrange(desc(n)) %>% head(10)

new_rides[c(new_rides$end_station_name != ""),] %>%
  count(end_station_name) %>% arrange(desc(n)) %>% head(10)

p1 <- new_rides[c(new_rides$start_station_name != ""),] %>%
  count(start_station_name) %>% arrange(desc(n)) %>% filter(n>40000) %>%
  ggplot(aes(start_station_name, n, fill=start_station_name)) + geom_col() +
  labs(title = 'Busiest start station') +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold"),
      axis.text.x = element_text(angle = 90, size=10, hjust = 1),
      axis.text.y = element_text(size=10)) +
  rremove("x.text")

p2 <- new_rides[c(new_rides$end_station_name != ""),] %>%
  count(end_station_name) %>% arrange(desc(n)) %>% filter(n>40000) %>%
  ggplot(aes(end_station_name, n, fill = end_station_name)) + geom_col() +
  labs(title = 'Busiest end station') +
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=13, face="bold"),
        axis.title.y = element_text(color="black", size=13, face="bold"),
        axis.text.x = element_text(angle = 90, size=10, hjust = 1),
        axis.text.y = element_text(size=10)) +
  rremove("x.text")

ggarrange(p1,p2, common.legend = TRUE, ncol = 1)

new_rides[c(new_rides$start_station_name != ""),] %>%
  count(start_station_name) %>% arrange(desc(n)) %>% filter(n>40000)







