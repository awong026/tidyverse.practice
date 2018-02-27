#Data Transformation 
library(tidyverse)
library(nycflights13)

#Question 1:	Find all flights that
#a)	Had an arrival delay of two or more hours
#b)	Flew to Houston (IAH or HOU)
#c)	Were operated by United, American, or Delta
#d)	Departed in summer (July, August, and September)
#e)	Arrived more than two hours late, but didn't leave late
#f)	Were delayed by at least an hour, but made up over 30 minutes in flight
#g)	Departed between midnight and 6am (inclusive)

#1a)

filter(flights, arr_delay>120)


#1b)

filter(flights, dest %in% c("IAH", "HOU"))


#1c)

filter(flights, carrier %in% c("AA", "DL", "UA"))


#1d)

filter(flights, between(month, 7,9))


#1e)
filter(flights, !is.na(dep_delay), dep_delay <= 0, arr_delay >120)


#1f)
filter(flights, !is.na(dep_delay), dep_delay >= 60, dep_delay-arr_delay > 30)


#1g)
filter(flights, dep_time <=600 | dep_time == 2400)



#Question 3: Find the 10 most delayed flights using a ranking function. 

mutate(flights,
       dep_delay_r = min_rank(-dep_delay)) %>%
  arrange(dep_delay_r) %>% 
  filter(dep_delay_r <= 10)


#Question 5: Look at the number of cancelled flights per day
canceled_delayed <- 
  flights %>%
  mutate(canceled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(prop_canceled = mean(canceled),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE)) ##From class notes

ggplot(canceled_delayed, aes(x = avg_dep_delay, prop_canceled)) +
  geom_point() +
  geom_smooth()


#Question 6: Which carrier has the worst delays?

flights %>%
  group_by(carrier) %>%
  summarise(arr_delaynew = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delaynew))

