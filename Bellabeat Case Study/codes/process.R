daily_activity <- read_csv("dailyActivity_merged.csv")
# No need to import the dailyCalories, dailyIntensities, dailySteps since 
# dailyActivity is already includes them
daily_sleep <- read_csv("sleepDay_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_intensities <- read_csv("hourlyIntensities_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
# There is no data data like hourlyActivity, so we should use hourlyCalories, 
# hourlyIntensities, hourlySteps

# Also, there is data of calories, intensities and steps for each minute but
# it would be too specific for my analysis, so I don't use it.
weight_log_info <- read_csv("weightLogInfo_merged.csv")
# Weight data of users may also be useful

glimpse(daily_activity)
head(daily_activity)
glimpse(daily_sleep)
head(daily_sleep)
glimpse(hourly_calories)
head(hourly_calories)
glimpse(hourly_intensities)
head(hourly_intensities)
glimpse(hourly_steps)
head(hourly_steps)
glimpse(weight_log_info)
head(weight_log_info)

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
hourly_calories <- clean_names(hourly_calories)
hourly_intensities <- clean_names(hourly_intensities)
hourly_steps <- clean_names(hourly_steps)
weight_log_info <- clean_names(weight_log_info)

daily_activity$activity_date = as.Date(daily_activity$activity_date, format="%m/%d/%Y", tz=Sys.timezone())
daily_sleep$sleep_day = as.Date(daily_sleep$sleep_day, format="%m/%d/%Y", tz=Sys.timezone())
weight_log_info$date = as.Date(weight_log_info$date, format="%m/%d/%Y", tz=Sys.timezone())

# Remove AM & PM from dates & times
hourly_calories$activity_hour <- parse_date_time(hourly_calories$activity_hour,
                                                  "%m/%d/%y %I:%M:%S %p")
hourly_intensities$activity_hour <- parse_date_time(hourly_intensities$activity_hour,
                                                    "%m/%d/%y %I:%M:%S %p")
hourly_steps$activity_hour <- parse_date_time(hourly_steps$activity_hour,
                                              "%m/%d/%y %I:%M:%S %p")

n_unique(daily_activity$id)
n_unique(daily_sleep$id)
n_unique(weight_log_info$id)
n_unique(hourly_calories$id)
n_unique(hourly_intensities$id)
n_unique(hourly_steps$id)

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))

daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

hourly_calories <- hourly_calories %>%
  distinct() %>%
  drop_na()

hourly_intensities <- hourly_intensities %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

hourly_activity <- merge(hourly_calories,hourly_steps,by=c("id","activity_hour"))
hourly_activity <- merge(hourly_activity,hourly_intensities,by=c("id","activity_hour"))
# Before we merge daily_activity and daily_sleep we need to change the name of 
# sleep_day column to activity_date so, they match in both data.
daily_sleep <- rename(daily_sleep, activity_date = sleep_day)
daily_merged <- merge(daily_activity,daily_sleep,by=c("id","activity_date"))


daily_activity %>% 
  select(total_steps, very_active_minutes, lightly_active_minutes, sedentary_minutes, calories, total_distance) %>%
  summary()

daily_sleep %>%
  select(total_minutes_asleep, total_time_in_bed) %>%
  summary()

ggplot(data=daily_activity, aes(x=total_steps, y = calories))+ 
  geom_point()+ 
  stat_smooth(method=lm)+
  labs(title="Total Steps vs. Calories Burned")

daily_activity %>%
  summarize(cor(total_steps,calories))


users_daily_activity <- daily_activity %>%
  group_by(id) %>%
  summarize(avg_total_steps = mean(total_steps), 
            avg_sedentary_minutes = mean(sedentary_minutes), 
            avg_calories = mean(calories), 
            corr_steps_calories = cor(total_steps,calories))

#Classify them according to activity type
users_daily_activity <- users_daily_activity %>%
  mutate(activity = case_when(
    avg_total_steps < 5000 ~ "sedentary",
    avg_total_steps >= 5000 & avg_total_steps < 7500 ~ "low active", 
    avg_total_steps >= 7500 & avg_total_steps < 10000 ~ "somewhat active", 
    avg_total_steps >= 10000 & avg_total_steps < 12500 ~ "active",
    avg_total_steps >= 12500 ~ "highly active"
  ))


nrow(users_daily_activity[users_daily_activity$avg_sedentary_minutes >= 1200, ])

mean(users_daily_activity$corr_steps_calories)














