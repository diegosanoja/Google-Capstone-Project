may<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202105_divvy_tripdata_cleaned.xlsx', sheet = '202105-divvy-tripdata')

str(may)

june<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202106_divvy_tripdata_cleaned.xlsx', sheet = '202106-divvy-tripdata')

july<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202107_divvy_tripdata_cleaned.xlsx', sheet = '202107-divvy-tripdata')

august<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202108_divvy_tripdata_cleaned.xlsx', sheet = '202108-divvy-tripdata')

september<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202109_divvy_tripdata_cleaned.xlsx', sheet = '202109-divvy-tripdata')

october<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202110_divvy_tripdata_cleaned.xlsx', sheet = '202110-divvy-tripdata')

november<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202111_divvy_tripdata_cleaned.xlsx', sheet = '202111-divvy-tripdata')

december<- read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202112_divvy_tripdata_cleaned.xlsx', sheet = '202112-divvy-tripdata')

january<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202201_divvy_tripdata_cleaned.xlsx', sheet = '202201-divvy-tripdata')

february<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202202_divvy_tripdata_cleaned.xlsx', sheet = '202202-divvy-tripdata')

march<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202203_divvy_tripdata_cleaned.xlsx', sheet = '202203-divvy-tripdata')

april<-read_excel('C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned_files/202204_divvy_tripdata_cleaned.xlsx', sheet = '202204-divvy-tripdata')

full_year_frame<-rbind(may,june, july, august, september, october, november, december, january, february, march, april)

str(full_year_frame)

numbers_of_days<- full_year_frame$day_of_week

days_vector<-numeric(length(numbers_of_days))


for (i in 1:length(days_vector)) {
  if (numbers_of_days[i] == 1) { days_vector[i]<- "Sunday"
  } else if (numbers_of_days[i] == 2) { days_vector[i]<- "Monday"
  } else if (numbers_of_days[i] == 3) { days_vector[i]<- "Tuesday"
  } else if (numbers_of_days[i] == 4) { days_vector[i]<- "Wednesday"
  } else if (numbers_of_days[i] == 5) { days_vector[i]<- "Thursday"
  } else if (numbers_of_days[i] == 6) { days_vector[i]<- "Friday"
  } else { days_vector[i]<- "Saturday"
  }
}

year_number<-numeric(length(numbers_of_days))

months_names<- full_year_frame$Month

months_names

for (i in 1:length(months_names)) {
  if (months_names[i] == "May") {year_number[i]<- "2021"
  } else if (months_names[i] == "June") {year_number[i]<- "2021"
  } else if (months_names[i] == "July") {year_number[i]<- "2021"
  } else if (months_names[i] == "August") {year_number[i]<- "2021"
  } else if (months_names[i] == "September") {year_number[i]<- "2021"
  } else if (months_names[i] == "October") {year_number[i]<- "2021"
  } else if (months_names[i] == "November") {year_number[i]<- "2021"
  } else if (months_names[i] == "December") {year_number[i]<- "2021"
  } else if (months_names[i] == "January") {year_number[i]<- "2022"
  } else if (months_names[i] == "February") {year_number[i]<- "2022"
  } else if (months_names[i] == "March") {year_number[i]<- "2022"
  } else {year_number[i]<- "2022"
  }
}



full_year_frame %>% group_by(Month) %>% summarize(min_ride = min(`ride_length(minutes)`))

year_number

full_year_frame_v2<- mutate(full_year_frame, Day = days_vector, Year = year_number)
str(full_year_frame_v2)

full_year_frame_v3<- full_year_frame_v2[!(is.na(full_year_frame_v2$start_station_name)),]

full_year_frame_v3<- full_year_frame_v3[!(is.na(full_year_frame_v3$end_station_name)),]

View(full_year_frame_v3)
colnames(full_year_frame_v3)

full_year_frame_v3$Day<- factor(full_year_frame_v3$Day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

full_year_frame_v3$Month<- factor(full_year_frame_v3$Month, levels = c('May', 'June', 'July', 'August', 'September', 'October', 'November', 'December', 'January', 'February', 'March', 'April'))

colnames(full_year_frame_v3)

minutes<-full_year_frame_v3$`ride_length(minutes)`

full_year_frame_v4 <-full_year_frame_v3 %>% select(-`ride_length(minutes)`) %>% mutate(trip_length_in_minutes = minutes)

type<-full_year_frame_v4$type_of_customer

member<-numeric(length(type))

for (i in 1:length(type)) {
  if (type[i] == "member") {member[i] <- "Member"
  } else {member[i] <- "Casual"
  }
}

member

rideable<- full_year_frame_v4$rideable_type

bike_type<-numeric(length(type))

for (i in 1:length(type)) {
  if (rideable[i] == "classic_bike") {bike_type[i] <- "Classic"
  } else if (rideable[i] == "docked_bike") {bike_type[i] <- "Docked"
  } else {bike_type[i] <- "Electric"
  }
}

full_year_frame_v5 <-full_year_frame_v4 %>% select(-`rideable_type`) %>% select(-`type_of_customer`) %>% mutate(type_of_bike_used = bike_type, type_of_customer = member)

View(full_year_frame_v5)

ggplot(full_year_frame_v5) +
  geom_bar(aes(x = Month, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 30), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Number of uses vs month", x = "Month", y = "Number of uses")

ggplot(full_year_frame_v5) +
  geom_bar(aes(x = Day, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Number of uses vs day of the week", x = "Day of the week", y = "Number of uses")

ggplot(full_year_frame_v5) +
  geom_bar(aes(x = type_of_bike_used, fill = type_of_customer), position = "dodge") +
  labs(title = "Number of uses vs type of bike used", x = "Type of bike", y = "Number of uses") +
  theme(plot.title = element_text(hjust = 0.5))

docked_bikes_frames<-full_year_frame_v5[full_year_frame_v5$type_of_bike_used == 'Docked',]

docked_bikes_annual<-docked_bikes_frames[docked_bikes_frames$type_of_customer == 'Annual',]

docked<- docked_bikes_frames$type_of_customer

sum(docked == 'Annual')

mean_median_frame<-full_year_frame_v5 %>% group_by(Month, type_of_customer)  %>% summarize(mean_ride = mean(trip_length_in_minutes), median_ride = median(trip_length_in_minutes))

colnames(mean_median_frame)

View(mean_median_frame)

ggplot(mean_median_frame) +
  geom_col(aes(x = Month, y = mean_ride, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Average bike use duration per month", x = "Month", y = "Time (minutes)")



max_frame<-full_year_frame_v5 %>% group_by(Month, type_of_customer)  %>% summarize(max_ride = max(trip_length_in_minutes)/60)

max_frame

ggplot(max_frame) +
  geom_col(aes(x = Month, y = max_ride, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Longest bike rent per month", x = "Month", y = "Time (hours)")

colnames(full_year_frame_v5)

short_trips<-full_year_frame_v5[(full_year_frame_v5$trip_length_in_minutes > 0 & full_year_frame_v5$trip_length_in_minutes <= 60),]

long_trips<-full_year_frame_v5[full_year_frame_v5$trip_length_in_minutes > 60,]

mean_median_frame_short<-short_trips %>% group_by(Month, type_of_customer)  %>% summarize(mean_ride = mean(trip_length_in_minutes), median_ride = median(trip_length_in_minutes))

View(mean_median_frame_short)

ggplot(short_trips) +
  geom_bar(aes(x = Month, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Short bike trips per month", subtitle = "Trips lower or equal to 60 minutes", x = "Month", y = "Number of trips")

ggplot(mean_median_frame_short) +
  geom_col(aes(x = Month, y = mean_ride, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Average time of short bike trips per month", subtitle = "Trips lower or equal to 60 minutes", x = "Month", y = "Time (minutes)")

View(short_trips)

ggplot(long_trips) +
  geom_bar(aes(x = Month, fill = type_of_customer), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Long bike trips per month", subtitle = "Trips longer than 60 minutes", x = "Month", y = "Number of trips")

aggregate(full_year_frame_v4$trip_length_in_minutes ~ full_year_frame_v4$type_of_customer, FUN = mean)
