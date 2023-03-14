glimpse(hourly_calories)
str(hourly_calories)
skim_without_charts(hourly_calories)
colnames(hourly_calories)
head(daily_activity)

hourly_calories$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")


dailyActivity<-daily_activity[order(as.Date(daily_activity$ActivityDate, format="%m/%d/%Y")),]

str(dailyActivity)

n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(hourly_steps$id)
n_unique(weight_log_info$Id)

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))

daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

View(clean_names(sleep_day))

sleep_day <- clean_names(sleep_day)

daily_activity <- clean_names(daily_activity)
hourly_calories <- clean_names(hourly_calories)
hourly_intensities <- clean_names(hourly_intensities)
hourly_steps1 <- clean_names(hourly_steps)


str(daily_activity)
daily_activity$activity_date = as.Date(daily_activity$activity_date, format="%m/%d/%Y", tz=Sys.timezone())
daily_sleep1$sleep_day = as.Date(daily_sleep1$sleep_day, format="%m/%d/%Y", tz=Sys.timezone())



str(hourly_calories1)
hourly_calories1$activity_hour = as.POSIXct(hourly_calories1$activity_hour, format="%m/%d/%Y %H:%M:%S", tz=Sys.timezone())

str(hourly_steps)
hourly_steps$activity_hour = as.POSIXct(hourly_steps$activity_hour, format="%m/%d/%Y %H:%M:%S", tz=Sys.timezone())

# Remove AM & PM from dates & times
hourly_calories1$activity_hour <- parse_date_time(hourly_calories1$activity_hour,
                                                 "%m/%d/%y %I:%M:%S %p")

hourly_steps$activity_hour <- parse_date_time(hourly_steps$activity_hour,
                                                 "%m/%d/%y %I:%M:%S %p")

hourly_intensities$activity_hour <- parse_date_time(hourly_intensities$activity_hour,
                                              "%m/%d/%y %I:%M:%S %p")


hourly_activity <- merge(hourly_calories,hourly_steps,by=c("id","activity_hour"))
hourly_activity <- merge(hourly_activity,hourly_intensities,by=c("id","activity_hour"))

str(hourly_activity)

daily_sleep <- rename(daily_sleep, activity_date = sleep_day)

daily_merged <- merge(daily_activity,daily_sleep,by=c("id","activity_date"))


users_daily <- daily_activity %>%
  group_by(id) %>%
  summarize(mean(total_steps), mean(total_distance),mean(sedentary_minutes), mean(calories), cor(total_steps,calories))



users_daily_activity <- daily_activity %>%
  group_by(id) %>%
  summarize(avg_total_steps = mean(total_steps), 
            avg_total_distance = mean(total_distance),
            avg_sedentary_minutes = mean(sedentary_minutes), 
            avg_calories = mean(calories), 
            corr_steps_calories = cor(total_steps,calories))

mean(users_daily_activity$corr_steps_calories)

users_daily_activity %>%
  summary()
  #summarize(mean(avg_total_steps), mean(avg_calories),mean(avg_total_distance),
  #          mean(avg_sedentary_minutes), mean(corr_steps_calories))

users_daily_activity <- users_daily_activity %>%
  mutate(activity = case_when(
    avg_total_steps < 5000 ~ "sedentary",
    avg_total_steps >= 5000 & avg_total_steps < 7500 ~ "low active", 
    avg_total_steps >= 7500 & avg_total_steps < 10000 ~ "somewhat active", 
    avg_total_steps >= 10000 & avg_total_steps < 12500 ~ "active",
    avg_total_steps >= 12500 ~ "highly active"
  ))


nrow(users_daily_activity[users_daily_activity$avg_sedentary_minutes >= 1200, ])

glimpse(users_daily_activity)


ggplot(daily_activity) + 
  geom_point(aes(x=total_steps, y=calories)) +
  geom_smooth(mapping = aes(x = total_steps, y = calories))


ggplot(data=daily_activity, aes(x=total_steps, y = calories))+ 
  geom_point()+ 
  stat_smooth(method=lm)+
  labs(title="Total Steps vs. Calories Burned")

daily_activity %>%
  summarize(cor(total_steps,calories))

ggplot(data=daily_merged, aes(x=sedentary_minutes, y = total_minutes_asleep))+ 
  geom_point()+ 
  stat_smooth(method=lm)+
  labs(title="Total Steps vs. Calories Burned")

daily_merged %>%
  summarize(cor(sedentary_minutes,total_minutes_asleep))


write_csv(daily_activity, path = "New Data/daily_activity.csv")
write_csv(daily_merged, path = "New Data/daily_merged.csv")
write_csv(daily_sleep, path = "New Data/daily_sleep.csv")
write_csv(hourly_activity, path = "New Data/hourly_activity.csv")
write_csv(users_daily, path = "New Data/users_daily.csv")
write_csv(users_daily_activity, path = "New Data/users_daily_activity.csv")

* Users takes 7638 steps per day on average. So, we can consider them somewhat active. However, we should courage them to take at least 10000 steps daily for an active and healthier lifestyle.

* They also burn 2304 calories on average per day, which could be considered normal.

* On average, they are very active for 21 minutes, lightly active for 192 minutes and sedentary for 16,5 hours per day. 

* They travel 5.49 kilometers per day on average.

* They sleep 7 hours per day on average. For adults, 7-9 hours of sleep per day is considered ideal and healthy. So, they fit on that but they are on the boundary.

* There is a positive correlation between daily steps taken and daily calories burned.

* Only %15 of users are active and %6 of users are highly active. %24 of the users considered as sedentary which is a quite dangereous amount.

* %27 of all users spend more than 20 hours of the day by sitting or lying down.


