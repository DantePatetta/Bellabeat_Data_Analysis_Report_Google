# Installing packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("here")
install.packages("rmarkdown")
install.packages("skimr")
install.packages("readr")
install.packages("lubridate")

---
  
# Loading packages
library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)
library(readr)
library(rmarkdown)
library(here)
library(dplyr)
library(lubridate)

---

# Importing data frame into RStudio. Each table will be stored in an independent variable
daily_activity <- read_csv(here("Fitabase_Data", "dailyActivity_merged.csv"))
sleep_activity <- read_csv(here("Fitabase_Data", "sleepDay_merged.csv"))
head(sleep_activity)
---

# Exploring data
View(daily_activity)
View(sleep_activity)
summary(daily_activity)
summary(sleep_activity)
skim(daily_activity)
skim(sleep_activity)
colnames(daily_activity)
colnames(sleep_activity)

---
  
# Cleaning data
## Changing 'char' data type to 'date' and cleaning all column names
  
dly_act_cleaned <- daily_activity %>% 
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y")) %>% # Changing ActivityDate from char to date format
  clean_names() %>% # Clean all names
  rename(activity_day = activity_date) # Change activity_date to activity_day name

slp_act_cleaned <- sleep_activity %>%
  mutate(SleepDay = as.Date(SleepDay, format = "%m/%d/%Y")) %>% # Changing SleepDay from char format to Date format
  clean_names() # Clean all names

## Joining tables. Left join dly_act_cleaned and slp_act_cleaned (dplyr)
dly_slp_merged <- dly_act_cleaned %>% 
  left_join(slp_act_cleaned, by = c('id'='id', 'activity_day'='sleep_day'))

## Find and remove duplicate records; and drop subset
duplicated(dly_slp_merged)
dly_slp_merged <- dly_slp_merged[!duplicated(dly_slp_merged),]

# Creat a new .csv file with clean table
write_csv(dly_act_cleaned, "daily_activity_cleaned.csv")
write_csv(slp_act_cleaned, "sleep_activity_cleaned.csv")

# Delete former tables
rm(daily_activity)
rm(sleep_activity)
rm(dly_act_cleaned)
rm(slp_act_cleaned)

---
  
# Analyze
## Filtering and summarizing

### Adding weekday column to data frame
dly_slp_merged$weekday <- weekdays(dly_slp_merged$activity_day)

### Create .csv file with joined data frames
write_csv(dly_slp_merged, "daily_sleep_merged")

### Total unique users and unique date activities
nrow(distinct(dly_slp_merged, id)) # Total unique users
nrow(distinct(dly_slp_merged, activity_day))# Total unique dates

### Summarizing min, max, avg using "dly_slp_merged" df
distance_summarize <- dly_slp_merged %>%
  group_by(id) %>%
  summarise(min_distance = min(total_distance), max_distance = max(total_distance), mean_distance = mean(total_distance), mean_calories = mean(calories)) %>%
  arrange(desc(mean_calories))

date_summarize <- dly_slp_merged %>%
  group_by(activity_day) %>%
  summarise(min_distance = min(total_distance), max_distance = max(total_distance), mean_distance = mean(total_distance)) %>%
  arrange(activity_day)

distance_detail_wday <- dly_slp_merged %>%
  group_by(weekday) %>%
  summarise(total_very_active = sum(very_active_distance), total_mod_active = sum(moderately_active_distance), total_light_active = sum(light_active_distance), total_sedentary_active = sum(sedentary_active_distance))
#, total_distance = sum(total_distance), total_calories = sum(calories))

sleep_logged_record <- dly_slp_merged %>% 
  mutate(sleep_record = case_when(total_sleep_records >= 1 ~ "recorded", TRUE ~ "not_recorded")) %>%
  count(sleep_record)

### Total number of users that really logged their training
user_activity_logged <- dly_slp_merged %>% 
  mutate(activities = case_when(logged_activities_distance == 0 ~ "not_logged", TRUE ~ "logged")) %>%
  count(activities) %>% 
  rename(count_logged_activities = n)

### When have users logged their activities
logged_act_per_wday <- dly_slp_merged %>% 
  filter(logged_activities_distance > 0) %>% 
  group_by(weekday) %>% 
  summarise(count_logged_activities = length(weekday)) %>% 
  arrange(desc(count_logged_activities))

### Total sleep recorded days per sleep status (deficient, regular, good, optimum and lapsed)
count_sleep_status <- dly_slp_merged %>% 
  mutate(sleep_range = case_when(total_minutes_asleep < 239 ~ "< 4 hr", total_minutes_asleep >= 240 & total_minutes_asleep < 300 ~ "4-5 hr", total_minutes_asleep >= 300 & total_minutes_asleep < 420 ~ "5-7 hs", total_minutes_asleep >= 420 & total_minutes_asleep <= 540 ~ "7-9 hs", TRUE ~ ">9 hs")) %>% 
  mutate(sleep_status = case_when(total_minutes_asleep < 239 ~ "deficient", total_minutes_asleep >= 240 & total_minutes_asleep < 300 ~ "regular", total_minutes_asleep >= 300 & total_minutes_asleep < 420 ~ "good", total_minutes_asleep >= 420 & total_minutes_asleep <= 540 ~ "optimum", TRUE ~ "lapsed")) %>% 
  filter(total_sleep_records >= 1) %>% 
  group_by(sleep_range) %>% 
  count(sleep_status) %>% 
  rename(total_recorded_days = n)

### Relation between sleep status and average distance covered
sleep_status_distance <- dly_slp_merged %>%
  mutate(sleep_status = case_when(total_minutes_asleep < 239 ~ "deficient", total_minutes_asleep >= 240 & total_minutes_asleep < 300 ~ "regular", total_minutes_asleep >= 300 & total_minutes_asleep < 420 ~ "good", total_minutes_asleep >= 420 & total_minutes_asleep <= 540 ~ "optimum", TRUE ~ "lapsed")) %>% 
  filter(total_sleep_records >= 1) %>% 
  group_by(sleep_status)  %>% 
  summarise(mean_total_distance = mean(total_distance))

### Join data frames count_sleep_status with sleep_status_distance
sleep_status_mean_distance <- count_sleep_status %>% 
  inner_join(sleep_status_distance, by = "sleep_status")

# Delete former tables
rm(count_sleep_status)
rm(sleep_status_distance)

---

# Share (use ggplot2)
library(ggplot2)

## Display the % of logged activity by users
ggplot(user_activity_logged, aes(x="", y= count_logged_activities, fill=activities, label = count_logged_activities)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  theme_void() +
  geom_text(size = 3.2, hjust = 1.9) +
  ggtitle("Percentage of logged activity by users")

## Display logged activities per weekday
ggplot(logged_act_per_wday, aes(x = weekday, y = count_logged_activities)) +
  geom_bar(stat = "identity", fill = "#9ecae1") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme(axis.text.x = element_text(size = 7)) +
  geom_text(aes(label = count_logged_activities), vjust = -1, colour = "black") +
  ylim(c(0, 10)) +
  ggtitle("Logged activities per weekday")

## Display total distance per weekday
distance_detail_wday_transposed <- read_delim("distance_detail_wday_transposed.csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(distance_detail_wday_transposed, aes(km, weekday, label = km)) +
  geom_line(aes(group = weekday)) +
  geom_point(aes(color = detail)) +
  xlim(0, 600) +
  geom_text(aes(color = detail), size = 3.2, vjust = -1) +
  ggtitle("Total distance per weekday")

## Portion of recorded sleep
ggplot(sleep_logged_record, aes(x="", y= n, fill=sleep_record, label = n)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  theme_void() +
  geom_text(size = 3.2, vjust = -5.5) +
  ggtitle("Percentage of recorded sleep activity by user")

## Show the sleep status from the recorded sleep
ggplot(sleep_status_mean_distance, aes(fill=sleep_status, y=total_recorded_days, x=sleep_range)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer() +
  geom_text(aes(label = total_recorded_days), vjust = -0.5, colour = "black") +
  ggtitle("Sleep status acording to user log")