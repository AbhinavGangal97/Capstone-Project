install.packages('tidyverse')
install.packages('here')
install.packages('readr')
install.packages('tidyr')
install.packages('dplyr')
install.packages('skimr')
install.packages('janitor')

library(tidyverse)
library(here)
library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(janitor)

daily_steps <- read_csv("dailySteps_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_intensity <- read_csv("dailyIntensities_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")
hourly_intensity <- read_csv("hourlyIntensities_merged.csv")

head(daily_steps)
head(hourly_calories)
head(daily_activity)
head(daily_calories)
head(daily_intensity)
head(hourly_steps)
head(sleep_day)

str(daily_steps)
str(hourly_calories)
str(daily_activity)
str(daily_calories)
str(daily_intensity)
str(hourly_steps)
str(sleep_day)

n_unique(daily_steps$Id)
n_unique(hourly_calories$Id)
n_unique(daily_activity$Id)
n_unique(daily_calories$Id)
n_unique(daily_intensity$Id)
n_unique(hourly_steps$Id)
n_unique(sleep_day$Id)

n_unique(daily_steps$ActivityDay)
n_unique(hourly_calories$ActivityHour)
n_unique(daily_activity$ActivityDate)
n_unique(daily_calories$ActivityDay)
n_unique(daily_intensity$ActivityDay)
n_unique(hourly_steps$ActivityHour)
n_unique(sleep_day$SleepDay)

sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_steps))
sum(duplicated(hourly_calories))
sum(duplicated(daily_intensity))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_day))

daily_activity <- daily_activity %>% 
  distinct() %>% 
  drop_na()
daily_calories <- daily_calories %>% 
  distinct() %>%
  drop_na()
daily_steps <- daily_steps %>% 
  distinct() %>%
  drop_na()
hourly_calories <- hourly_calories %>%
  distinct() %>%
  drop_na()
daily_intensity <- daily_intensity %>% 
  distinct() %>%
  drop_na()
sleep_day <- sleep_day %>% 
  distinct() %>% 
  drop_na()
hourly_intensity <- hourly_intensity %>%
  distinct() %>% 
  drop_na()
hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()

sum(duplicated(sleep_day))

clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)

clean_names(daily_calories)
daily_calories <- rename_with(daily_calories, tolower)

clean_names(daily_intensity)
daily_intensity <- rename_with(daily_intensity, tolower)

clean_names(daily_steps)
daily_steps <- rename_with(daily_steps, tolower)

clean_names(hourly_calories)
hourly_calories <- rename_with(hourly_calories, tolower)

clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

clean_names(sleep_day)
sleep_day <- rename_with(sleep_day, tolower)

clean_names(hourly_intensity)
hourly_intensity <- rename_with(hourly_intensity, tolower)

daily_activity <- daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_calories <- daily_calories %>% 
  rename(date = activityday) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_intensity <- daily_intensity %>% 
  rename(date = activityday) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_steps <- daily_steps %>% 
  rename(date = activityday) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

hourly_steps <- hourly_steps %>%
  rename(date_time = activityhour) %>%
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

hourly_calories <- hourly_calories %>% 
  rename(date_time = activityhour) %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

hourly_intensity <- hourly_intensity %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

sleep_day <- sleep_day %>%
  rename(date_time = sleepday) %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

hourly_data <- merge(hourly_calories, hourly_intensity, by=c ("id", "date_time"))
hourly_data <- merge(hourly_data, hourly_steps, by = c("id", "date_time"))
summary(hourly_data)

daily_activity_sleep <- merge(daily_activity, sleep_day, by=c("id"))

daily_activity %>% 
  select(totalsteps, totaldistance, sedentaryminutes, calories) %>% 
  summary()

daily_calories %>% 
  select(calories) %>% 
  summary()

daily_intensity %>% 
  select(sedentaryminutes, lightlyactiveminutes, fairlyactiveminutes, veryactiveminutes) %>% 
  summary()

daily_steps %>% 
  select(steptotal) %>% 
  summary()

sleep_day %>%
  select(totalsleeprecords, totalminutesasleep, totaltimeinbed) %>% 
  summary()

hourly_data %>% 
  select(calories, steptotal, totalintensity) %>% 
  summary()

##Correlation between total steps and calories burned

ggplot(data=daily_activity, aes(x = totalsteps, y = calories)) +
  geom_point()+
  geom_smooth() +
  stat_cor(method = "pearson", label.x = 20000, label.y = 1800)+
  labs(title = "Total Steps vs Calories Burned")

ggplot(data=daily_activity, aes(x = totaldistance, y = calories)) +
  geom_point()+
  geom_smooth() +
  stat_cor(method = "pearson", label.x = 20, label.y = 1000)+
  labs(title = "Total Distance vs Calories Burned")

weekday_steps <- daily_activity %>% 
  mutate(weekday_steps, weekday = weekdays(date))
weekday_steps$weekday <- ordered(weekday_steps$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_steps <- weekday_steps %>% 
  group_by(weekday) %>% 
  summarise(daily_steps = mean(totalsteps))

ggplot(data = weekday_steps) + 
  geom_col(mapping = aes(x=weekday, y=daily_steps), fill = 'purple',width = .6) +
  geom_hline(yintercept = 7638) +
  labs(title = "Total Number of Steps Per Day")

sleep_per_day <- sleep_day %>% 
  mutate(sleep_day, weekday = weekdays(date_time))
sleep_per_day$weekday <- ordered(sleep_per_day$weekday, levels=c("Monday", "Tuesday", "Wednesday", 
                                                                 "Thursday", "Friday", "Saturday", "Sunday"))

sleep_per_day <- sleep_per_day %>% 
  group_by(weekday) %>% 
  summarise(sleep_per_day = mean(totalminutesasleep))

head(sleep_per_day)

ggplot(data = sleep_per_day) + 
  geom_col(mapping = aes(x=weekday, y=sleep_per_day), fill = 'Green',width = .6) +
  geom_hline(yintercept = 420) +
  labs(title = "Total Hours of Sleep Per Day")

hourly_intensity <- hourly_intensity %>%
  separate(date_time, into = c("date", "time"), sep= " ")

hourly_intensity <- hourly_intensity %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(mean_total_intensity = mean(totalintensity))

ggplot(data=hourly_intensity) + 
  aes(x=time, y=mean_total_intensity)+
  geom_histogram(stat = "identity", colour="white", fill="blue", width = 0.8)+
  labs(x = "Time", y = "mean_total_intensity", title = "Average Total Intensity vs Time")+
  theme(axis.text.x = element_text(angle = 90))

hourly_steps <- hourly_steps %>% 
  separate(date_time, into = c("date", "time"), sep = " ")

hourly_steps %>%
  group_by(time) %>%
  summarize(average_steps = mean(steptotal)) %>%
  ggplot() +
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "red", high = "blue")+
  theme(axis.text.x = element_text(angle = 90))

daily_average_steps <- daily_activity %>% 
  group_by(id) %>% 
  summarise(avg_daily_steps = mean(totalsteps)) %>% 
  mutate(user_type = case_when(
    avg_daily_steps <= 5000 ~ "Sedentary",
    avg_daily_steps > 5000 & avg_daily_steps <= 9000 ~ "Fairly Active",
    avg_daily_steps > 9000 ~ "Very Active"
  ))

head(daily_average_steps)

user_type_sum <- daily_average_steps %>% 
  group_by(user_type) %>% 
  summarise(total = n()) %>% 
  mutate(total_percent = scales::percent(total/sum(total)))

head(user_type_sum)

user_type_sum %>% 
  ggplot(aes(x=" ", y = total_percent, fill=user_type))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  geom_text(aes(label = total_percent), position = position_stack(vjust = 0.5, reverse = FALSE))+
  theme_void()
