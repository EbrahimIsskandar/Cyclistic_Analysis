library(tidyverse)
library(googledrive)
library(lubridate)

# Google Drive file IDs
file_ids <- c(
  "1wDWoOtOS-7H9KjwpkczVOrMiJf8kAvBA",
  "17Ig2DZWLw5hVa3xIbOdVfYUhPZ1I9K1A",
  "11K3HQSRQMpeNc_maIDFU3vUUPxOkd5qy",
  "1Rqi4Eers94NxkAxglexNTGslaEmDY43b"
)

# Authenticate Google Drive (opens browser window for sign-in)
drive_auth()

# Download the files
for(i in seq_along(file_ids)){
  drive_download(as_id(file_ids[i]), path = paste0("q", i, ".csv"), overwrite = TRUE)
}

# Read the data
q1 <- read_csv("q1.csv")
q2 <- read_csv("q2.csv")
q3 <- read_csv("q3.csv")
q4 <- read_csv("q4.csv")
view(q2)

# Standardize column names for quarter 2 to match q1
colnames(q2) <- colnames(q1)
colnames(q3) <- colnames(q1)
colnames(q4) <- colnames(q1)

# Combine all quarters into one data frame
all_data <- bind_rows(q1, q2, q3, q4)
View(all_data)

# Print column names to verify
print(colnames(all_data))

# Convert columns to datetime and calculate ride length
all_data <- all_data %>%
  mutate(
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    ride_length = as.numeric(difftime(end_time, start_time, units = "mins"))
  ) %>%
  filter(ride_length > 0)
View(all_data)
# Quick overview of the data
glimpse(all_data)

# 1. Show statistical summary of ride lengths
format(summary(all_data$ride_length), scientific = FALSE)

# 2. Plot number of rides by user type (Subscriber or Customer)
all_data %>%
  count(usertype) %>%
  ggplot(aes(x = usertype, y = n, fill = usertype)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Rides by User Type", x = "User Type", y = "Number of Rides") +
  theme_minimal()

# 3. Calculate and plot average ride length by user type
all_data %>%
  group_by(usertype) %>%
  summarise(avg_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = usertype, y = avg_ride_length, fill = usertype)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Ride Length by User Type", x = "User Type", y = "Ride Duration (minutes)") +
  theme_minimal()

# 4. Extract day of week from start_time as a factor with day labels
all_data <- all_data %>%
  mutate(day_of_week = wday(start_time, label = TRUE))

# Verify number of rides per day of week
table(all_data$day_of_week)

# 5. Plot number of rides per day of week by user type with dodged bars
all_data %>%
  group_by(usertype, day_of_week) %>%
  summarise(trip_count = n(), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = trip_count, fill = usertype)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides per Day of Week by User Type", x = "Day", y = "Number of Rides") +
  theme_minimal()


# 6. Boxplot comparing ride length by user type with y-axis limit to avoid outliers

all_data %>%
  ggplot(aes(x = usertype, y = ride_length, fill = usertype)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 100)) +  # Limit y-axis to avoid extreme values affecting the plot
  labs(title = "Ride Length Comparison by User Type", x = "User Type", y = "Ride Duration (minutes)") +
  theme_minimal()
