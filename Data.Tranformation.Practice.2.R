#0: Look at airports, flights, and airlines data
 library(nycflights13)
 library(tidyverse)
 library(ggplot2)
 
 #flights
 str(flights) #19 variables with 336776 rows
 ?flights
 head(flights)
 #airports
 str(airports)  #8 variables with 1458 rows
 summary(airports)
 ?airports
 
 #airlines
 str(airlines)  #2 variables with 16 rows
 ?airlines
 
 
#1. How many airports in NYC ?  What are they ? 
#Which one has the most flights ? Use a bar graph to show the number of flights in each airport.
 
flights$origin
unique(flights$origin) #3 airports and they are EWR, LGA, and JFK

#Which one has the most flights?
(EWR.Count<- count(flights[flights$origin == "EWR",])) #120835  #Has most flights
(LGA.Count<- count(flights[flights$origin == "LGA",])) #104662
(JFK.Count<- count(flights[flights$origin == "JFK",])) #111279
LGA.Count+JFK.Count+EWR.Count #336776, which matches the total number of rows in flights data. 

#Plot information using bar graph
ggplot(flights, aes(origin)) + geom_bar()



###################################### ALT (1)
count(flights, origin)  #Counts and shows the 3 origins of NYC airports. 

#Another way to plot information
counts <- table(flights$origin)
barplot(counts, main = "NYC Airport Distribution")









#2. Find top five destinations in terms of the number of flights. 
#Use a bar graph to show the number of flights from various NYC airports (origin).

#Get a count of number of flights for each destination
flights %>% group_by(dest) %>% 
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(-n)

#Now create with only the top 5
flights %>%
  group_by(dest) %>%
  summarise(n=n()) %>%
  arrange(rank(-n)) %>%
  filter(rank(-n) <6)

#Top 5 Dest: ORD, ATL, LAX, BOS, MCO

#Plot of all the destinations and how much out of the 3 origin airports they make up (How many flights)
ggplot(flights, aes(origin, fill = dest)) + geom_bar(color = "black")

#Plot of the top 5 destinations and how much out of the 3 origin airports they make up (How many flights)
ggplot(subset(flights, dest == "ORD" | dest == "ATL" | dest == "LAX" | dest == "BOS" | dest == "MCO") , aes(origin, fill = dest)) + geom_bar(color = "black")

######################## ALT (2)

#Code to get top 5 destinations
five_freq_dests <- flights %>%
  group_by(dest) %>%
  summarize(flight = n()) %>%
  arrange(desc(flight)) %>%
  top_n(n = 5)

five_freq_dests

#Plot Again
five.count <- subset(flights, dest == "ORD" | dest == "ATL" | dest == "LAX" | dest == "BOS" | dest == "MCO")
alt.five.count <- table(five.count$dest, five.count$origin)
barplot(alt.five.count, legend = rownames(alt.five.count))



#3. Find the number of flights each month. Use a graph to show the number of flights each month 
#from various NYC airports(origin).
daily <- group_by(flights,year,month,day)
#Day
(per_day <- summarise(daily, flights=n()))

#Month
(per_month<- summarise(per_day, flights = sum(flights)))

ggplot(flights, aes(origin)) + geom_bar() + facet_wrap(~month)



################################ ALT (3)


by_monthly <- flights %>%
  group_by(month) %>%
  summarize(count = n())
by_monthly

#Plot Again monthly by origin
month.count <- table(flights$month, flights$origin)
barplot(month.count, legend.text = rownames(month.count))








#4. What time of day should you fly if you want to avoid delays as much as possible? 
#Use a graph to show the relationship between its departure  time (in hour) and average arrival delays.

#See how many time slots there are
unique(flights$sched_dep_time) #Many different one's so let's round to get them to their closests hour

#Rounded scheuled Depature Times
non_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
non_cancelled #created non_cancelled data so we don't have to deal with NA's

non_cancelled.rounded.rsdt <- non_cancelled%>%mutate(time = round(sched_dep_time,-2))
head(non_cancelled.rounded.rsdt)
head(non_cancelled.rounded.rsdt$time) #Okay now all times have been rounded so we can better see when is a good time not to fly

bbb <- non_cancelled.rounded.rsdt%>%
  group_by(time)%>%
  summarise(avg.delay1 = mean(arr_delay),
            avg.delay2 = mean(arr_delay[arr_delay>0]))#mean of those who actually have delays


#Avoid going planning a plane ride at 1900
bbb[bbb$avg.delay2 == max(bbb$avg.delay2),]

#Go during 5am to avoid delays
bbb[bbb$avg.delay2 == min(bbb$avg.delay2),]

#Plot of avg. time delays throughout a day
ggplot(bbb, aes(time, avg.delay2)) + geom_point()+geom_smooth(se = F)
#One can see going during the morning times is better than evening times when trying to avoid delays


#Just in case we are talking about avg.delay1: which is the delay if we also included times that arrived early or on time
ggplot(bbb, aes(time, avg.delay1)) + geom_point() + geom_smooth(se = F) #pattern is the same


################# ALT(4)
#Didn't need to round like the code above
aaa <- non_cancelled%>%
              group_by(hour)%>%
              summarise(avg.delay1 = mean(arr_delay),
                        avg.delay2 = mean(arr_delay[arr_delay>0])) #mean of those who actually have delays
aaa
aaa[aaa$avg.delay2 == max(aaa$avg.delay2),] #Same answer as above (1900 is not a good time)

#Plot again, but using histogram
ggplot(aaa, aes(hour, avg.delay2)) + geom_histogram(stat = "identity") #pattern is the same


#5. Look at the number of cancelled flights per day. Is there a pattern? 
#Is the proportion of cancelled flights related to the average delay?  Use a graph to show the relationship.


cancelled <-filter(flights, is.na(air_time))
cancelled

#number of cancels per across the year
ddd<-cancelled %>%
  transmute(cal.year = (month -1)*30 +day)
ddd 
ggplot(ddd, aes(cal.year)) + geom_density() + scale_x_continuous(breaks = seq(1, 360, 30),labels = seq(1:12))  #One can see that there are delays during the holidays


##Let's look at the proportion of canceled to avg. delay time
nnn <- flights%>%
  group_by(month, day) %>%
  summarise(pro.canceled = sum(is.na(air_time))/ n(), 
            avg.arr.delay = mean(arr_delay, na.rm= T)) %>%
              mutate(cal.year = (month-1)*30 + day)
ggplot(nnn, aes(avg.arr.delay, pro.canceled)) + geom_smooth(se= F) +geom_point() ##Looks like there is a positive pattern between canceled and avg. delay


########################## Alt(5)
#Create canceled and delayed table
cancel.delayed <- 
  flights %>%
  mutate(cancel = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(prop_cancel = mean(cancel),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE))

ggplot(cancel.delayed, aes(avg_arr_delay, prop_cancel)) +
  geom_point() +
  geom_smooth() #Same answer (Same pattern)







#6. Which carrier has the worst delays? Challenge: can you disentangle the effects of 
#bad airports vs. bad carriers? Why/why not?
#(Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))

yyy <- non_cancelled%>%
  group_by(carrier)%>%
  summarise(avg.delay1 = mean(arr_delay),
            avg.delay2 = mean(arr_delay[arr_delay>0])) #delay2 = delay if we are only counting when there are delays. 


yyy
#carrier OO has the worst delay2
yyy[yyy$avg.delay2 == max(yyy$avg.delay2),]

#carrier F9 has the worst delay1 (This delay includes time even when the airplane arrives on time/early)
yyy[yyy$avg.delay1 == max(yyy$avg.delay1),]

##Challenge Attempt:
non_cancelled %>% group_by(carrier, origin) %>% # using origin to restrict to NYC. So only looking at flights that leave NYC
  summarise(avg.delay = mean(arr_delay[arr_delay>0])) %>%
  ggplot(aes(origin, avg.delay, fill = carrier)) +
  geom_col()  #Looks like Carrier 00 the worst still, but I don't know. I couldn't figure it out. I tried at least. 


############################## Alt(6)
flights %>%
filter(arr_delay > 0) %>%
group_by(carrier) %>%
summarise(average_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
arrange(desc(average_arr_delay))  #This came up with 00 as the carrier with the most delays too. 







#7. Which flights traveled the longest (in terms of distance) ? 
#Which traveled the shortest? Identify the destinations. Is there a relationship between the travel 
#distance and  its speed (mph, miles per hour)  ?  Use a graph to show the relationship.

#The longest flights are the Hawaii Air (HA 51) between JFK and HNL (Honolulu) at 4,983 miles.
ppp<-arrange(flights, desc(distance))
ppp%>%
  select(dest, distance, origin)


#Apart from an EWR to LGA flight that was canceled, the shortest flights are the Envoy Air Flights 
#between EWR and PHL at 80 miles
ccc<-arrange(flights, distance)
ccc%>%
  select(dest, distance, origin)



#Create speed = distance/air_time
mmm<-non_cancelled%>%
  group_by(dest,distance)%>%
  mutate(speed = distance/air_time)%>%
  select(dest, speed, distance)

mmm

### Travel distance vs speed
ggplot(mmm, aes(distance, speed)) + geom_point() +geom_smooth(se = F) #Looks like the longer the distance the faster the plane


############# Alt(7)

#The longest flights are the Hawaii Air (HA 51) between JFK and HNL (Honolulu) at 4,983 miles.
flights%>%
  arrange(desc(distance))%>%
  select(dest, distance, origin)


#Apart from an EWR to LGA flight that was canceled, the shortest flights are the Envoy Air Flights 
#between EWR and PHL at 80 miles
flights%>%
  arrange(distance)%>%
  select(dest, distance, origin)


#Create speed = distance/air_time
non_cancelled%>%
  group_by(dest,distance)%>%
  mutate(speed = distance/air_time)%>%
  select(dest, speed, distance)%>%
  ggplot(aes(distance, speed)) + geom_point() +geom_smooth(se = F)


################# Extension 1
#Find bottom five destinations in terms of the number of flights. 
#Use a bar graph to show the number of flights from various NYC airports (origin).
#Code to get bottom 5 destinations
five_leastfreq_dests <- flights %>%
  group_by(dest) %>%
  summarize(flight = n()) %>%
  arrange(desc(flight)) %>%
  top_n(n = -5)

five_leastfreq_dests
#HDN, MTJ, SBN, ANC, LEX, LGA

#Plot Again
ggplot(subset(flights, dest == "HDN" | dest == "MTJ" | dest == "SBN" | dest == "LEX" | dest == "LGA"| dest == "ANC") , aes(origin, fill = dest)) + geom_bar(color = "black")




################# Extension 2
#For each plane, what is the number of flights before the first delay of greater than 1 hour.
Big.delays <-non_cancelled %>% group_by(tailnum) %>%
  arrange(time_hour) %>%
  mutate(delay.more.hour = sum(cumall(dep_delay <60)))

head(Big.delays$delay.more.hour)

#Avg. number of flights before first delay of greater than 1 hour
mean(Big.delays$delay.more.hour)  #17.3 


