library(tidyverse)

path_name <- 'C:/Users/Diego/Documents/Google_Certificate/Capstone_project/cleaned/' #this saves the string under the given variable name

file_names = c('202110-divvy-tripdata-cleaned.csv',
               '202111-divvy-tripdata-cleaned.csv',
               '202112-divvy-tripdata-cleaned.csv',
               '202201-divvy-tripdata-cleaned.csv',
               '202202-divvy-tripdata-cleaned.csv',
               '202203-divvy-tripdata-cleaned.csv',
               '202204-divvy-tripdata-cleaned.csv',
               '202205-divvy-tripdata-cleaned.csv',
               '202206-divvy-tripdata-cleaned.csv',
               '202207-divvy-tripdata-cleaned.csv',
               '202208-divvy-tripdata-cleaned.csv',
               '202209-divvy-publictripdata-cleaned.csv') #this saves a string of vector under the given variable

full_year_frame<-read_csv(paste(path_name,file_names[1], sep="")) #this loads the first csv file of the project

for (i in 2:length(file_names)) {
  full_year_frame<- rbind(full_year_frame,read_csv(paste(path_name,file_names[i], sep="")))
} #this loop loads the other 11 csv files and appends them to the full_year_frame

str(full_year_frame) #this shows the structure of the full_year_frame

full_year_frame<- full_year_frame[!(is.na(full_year_frame$ride_id)),] #this removes the rows where the ride_id is null/blank

full_year_frame<- full_year_frame[full_year_frame$Rent_time>1.5,] #this removes the rows where the Rent_time value is less than 1.5 minutes

week_days <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')

for (i in 1:7) {
  full_year_frame$Day[full_year_frame$Day == i] <- week_days[i]
} #this loop uses the vector above to change the entries of the Day column in the full_year_frame

year_vector<-numeric(length(full_year_frame$Day)) #this creates an empty vector of length equal to the number of values of the Day column

months_names <- full_year_frame$Month

for (i in 1:length(months_names)) {
  if (months_names[i] %in% c("October","November", "December")) {year_vector[i]<- "2021"
  } else {year_vector[i]<- "2022"
  }
} #this loops adds the year when the bike was rented based on the strings in the Month column

year_vector

hours<- full_year_frame$Hour

period_vector<- numeric(length(hours))

for (i in 1:length(period_vector)) {
  if (hours[i] %in% 5:11) { period_vector[i]<- "Morning"
  } else if (hours[i] %in% 12:16) { period_vector[i]<- "Afternoon"
  } else if (hours[i] %in% 17:20) { period_vector[i]<- "Evening"
  } else { period_vector[i]<- "Night"
  }
} #this loops adds the day period when the bike was rented based on the strings in the Hour column

full_year_frame<- mutate(full_year_frame,
                         Year = year_vector,
                         Day_period = period_vector) #this code adds the year and period vector to the data frame

str(full_year_frame)

View(full_year_frame) #this opens a window that displays the data frame enclosed in brackets

colnames(full_year_frame) #this displays the column names of the data frames

full_year_frame$Day<- factor(full_year_frame$Day,
                             levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) #this turns the Day column into a factor(data objects which are used to categorize the data and store it as levels)

full_year_frame$Day

full_year_frame$Month<- factor(full_year_frame$Month,
                               levels = c('October', 'November', 'December', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September'))

full_year_frame$Month

colnames(full_year_frame)

full_year_frame <-full_year_frame %>%
  rename(Bike_used = rideable_type,
         start_time = started_at,
         end_time = ended_at,
         Customer_type = Member_Casual) #this code renames the columns of the frame

colnames(full_year_frame)

View(full_year_frame)

aggregated_frame_month<-full_year_frame %>%
  group_by(Month, Customer_type)  %>%
  summarize(mean_rent = mean(Rent_time), bikes_used = n()/1000)
#this is a pipe where a sub data frame is created by grouping the data by Month and Customer type and then added to compute the mean rent time and the number of bikes used.

View(aggregated_frame_month)

ggplot(aggregated_frame_month) +
  geom_line(aes(x = Month, y = bikes_used, group = Customer_type, color = Customer_type)) +
  geom_point(aes(x = Month, y = bikes_used, group = Customer_type, color = Customer_type)) +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Number of Rents per Month", x = "Month", y = "Number of Rents (thousands)")
#this creates a line chart with dots using the given columns of the data frame in the first line. Also the last line sets up the title and axis of the chart.


ggplot(aggregated_frame_month) +
  geom_line(aes(x = Month, y = mean_rent, group = Customer_type, color = Customer_type)) +
  geom_point(aes(x = Month, y = mean_rent, group = Customer_type, color = Customer_type)) +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Average Rent Time per Month", x = "Month", y = "Time (minutes)") +
  ylim(0, 40) +
  scale_color_manual(values = c("#009900","#993366"))
#the last 2 lines of code above control the range of the y-axis and the color of the lines and dots respectively

aggregated_frame_day<-full_year_frame %>%
  group_by(Day, Customer_type)  %>%
  summarize(mean_rent = mean(Rent_time), bikes_used = n()/1000) 

View(aggregated_frame_day)

ggplot(aggregated_frame_day) +
  geom_col(aes(x = Day, y = bikes_used, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Number of Rents per Day", x = "Day", y = "Number of Rents (thousands)")
#this creates a bar chart using the given columns of the data frame in the first line.

ggplot(aggregated_frame_day) +
  geom_col(aes(x = Day, y = mean_rent, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Average Rent Time per Day", x = "Day", y = "Time (minutes)")

aggregated_frame_period<-full_year_frame %>% group_by(Day_period, Customer_type)  %>% summarize(bikes_used = n())

View(aggregated_frame_period)

period_matrix<- matrix(c(aggregated_frame_period$bikes_used[seq(1,7,2)],
                         aggregated_frame_period$bikes_used[seq(2,8,2)]),
                       nrow = 2, ncol = 4, byrow = TRUE,
                       dimnames = list(c("Casual", "Member"),
                                       aggregated_frame_period$Day_period[seq(1,7,2)]))
#the code above creates a matrix using the customer type as rows and day periods as columns

period_matrix

ggplot(aggregated_frame_period) +
  geom_col(aes(x = Day_period, y = bikes_used, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Number of Rents per Day Period", x = "Day Period", y = "Number of Rents (thousands)")

aggregated_frame_bike_type<-full_year_frame %>%
  group_by(Bike_used, Customer_type) %>% 
  ummarize(bikes_used = n()/1000)

View(aggregated_frame_bike_type)

aggregated_frame_bike_type[6,] <- list('Docked', 'Member', 0) #this adds a row at the end of the frame, the row is the given list

View(aggregated_frame_bike_type)

ggplot(aggregated_frame_bike_type) +
  geom_col(aes(x = Bike_used, y = bikes_used, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Number of Rents per Type of Bike", x = "Type of Bike", y = "Number of Rents (thousands)") +
  scale_fill_manual(values = c("#cc00cc","#006666"))
#the last line of code above controls the color of the bars 


max_frame_month<-full_year_frame %>% group_by(Month, Customer_type)  %>% summarize(max_ride = max(Rent_time)/60)

view(max_frame_month)

ggplot(max_frame_month) +
  geom_point(aes(x = Month, y = max_ride, group = Customer_type, color = Customer_type), size = 2.5) +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Longest bike rent per month", x = "Month", y = "Time (hours)")

max_frame_day<-full_year_frame %>% group_by(Day, Customer_type)  %>% summarize(max_ride = max(Rent_time)/60)

view(max_frame_day)

ggplot(max_frame_day) +
  geom_col(aes(x = Day, y = max_ride, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Longest bike rent per Day", x = "Day", y = "Time (hours)")

long_rents_frame_month <- full_year_frame[full_year_frame$Rent_time > 60,] %>%
  group_by(Month, Customer_type) %>%
  summarize(bikes_used = n()/1000)

View(long_rents_frame_month)

ggplot(long_rents_frame_month) +
  geom_line(aes(x = Month, y = bikes_used, group = Customer_type, color = Customer_type)) +
  geom_point(aes(x = Month, y = bikes_used, group = Customer_type, color = Customer_type)) +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Bikes Rented per Month", subtitle = "Rents Longer Than 60 Minutes", x = "Month", y = "Bikes Rented (thousands)")  +
  scale_color_manual(values = c("#009900","#993366"))

long_rents_frame_day <- full_year_frame[full_year_frame$Rent_time > 60,] %>%
  group_by(Day, Customer_type) %>%
  summarize(bikes_used = n()/1000)

View(long_rents_frame_day)

ggplot(long_rents_frame_day) +
  geom_col(aes(x = Day, y = bikes_used, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Bikes Rented per Day", subtitle = "Rents Longer Than 60 Minutes", x = "Day", y = "Bikes Rented (thousands)")  +
  scale_fill_manual(values = c("#009900","#993366"))

long_rents_frame_period <- full_year_frame[full_year_frame$Rent_time > 60,] %>%
  group_by(Day_period, Customer_type)  %>% summarize(bikes_used = n())

View(long_rents_frame_period)

long_period_matrix<- matrix(c(long_rents_frame_period$bikes_used[seq(1,7,2)],
                         long_rents_frame_period$bikes_used[seq(2,8,2)]),
                       nrow = 2, ncol = 4, byrow = TRUE,
                       dimnames = list(c("Casual", "Member"),
                                       long_rents_frame_period$Day_period[seq(1,7,2)]))

long_period_matrix

short_mean_frame_month<- full_year_frame[full_year_frame$Rent_time <= 60,] %>%
  group_by(Month, Customer_type) %>%
  summarize(mean_rent = mean(Rent_time))

View(short_mean_frame_month)

ggplot(short_mean_frame_month) +
  geom_line(aes(x = Month, y = mean_rent, group = Customer_type, color = Customer_type)) +
  geom_point(aes(x = Month, y = mean_rent, group = Customer_type, color = Customer_type)) +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Average Rent Time of per Month", subtitle = "Rents Shorter or Equal to 60 Minutes", x = "Month", y = "Time (minutes)")  +
  ylim(0, 20) +
  scale_color_manual(values = c("#009900","#993366"))

short_mean_frame_day<- full_year_frame[full_year_frame$Rent_time <= 60,] %>%
  group_by(Day, Customer_type) %>%
  summarize(mean_rent = mean(Rent_time))

View(short_mean_frame_day)

ggplot(short_mean_frame_day) +
  geom_col(aes(x = Day, y = mean_rent, fill = Customer_type), position = "dodge") +
  theme(axis.text.x = element_text(angle = 25), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Average Rent Time of per Day", subtitle = "Rents Shorter or Equal to 60 Minutes", x = "Day", y = "Time (minutes)") +
  scale_fill_manual(values = c("#996600","#ff3300"))
