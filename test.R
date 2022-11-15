# Project Group 5


#### NO STATE OF DELAWARE or NEW HAMPSHIRE (INSTEAD WE HAVE U.S VIRGINS AND PUERTO RICO) ####

# Loading the necessary packages
library(ggplot2)
library(plyr)
library(dplyr)
library(ggthemes)
library(reshape2)

# Read in all CSV Files
July_2018 <- read.csv("3355 project 2018 July.csv")
June_2018 <- read.csv("3355 project 2018 June.csv")
July_2019 <- read.csv("3355 project 2019 July.csv")
June_2019 <- read.csv("3355 project 2019 June.csv")
July_2020 <- read.csv("3355 project 2020 July.csv")
June_2020 <- read.csv("3355 project 2020 June.csv")
July_2021 <- read.csv("3355 project 2021 July.csv")
June_2021 <- read.csv("3355 project 2021 June.csv")
July_2022 <- read.csv("3355 project 2022 July.csv")
June_2022 <- read.csv("3355 project 2022 June.csv")

# Binded all data frames  Together
tx_flights <- rbind(June_2018, July_2018, 
                    June_2019, July_2019, 
                    June_2020, July_2020, 
                    June_2021, July_2021, 
                    June_2022, July_2022)


# -------------  Average Delay times for Cities  ----------------------
dest_cities <- unique(tx_flights$DEST_CITY_NAME) # Get all City Names
dest_cities <- sort(dest_cities) # Sort City names
sum_of_delay_times_city <- (1:203 * 0) # Set all sums to 0
names(sum_of_delay_times_city) <- dest_cities # name sum vector

num_of_each_city <-(1:203 * 0)
names(num_of_each_city) <- dest_cities


tx_flights$DEP_DELAY_NEW[is.na(tx_flights$DEP_DELAY_NEW)] <- 0 # Set All NA in DEP_DELAY_NEW to 0 
for (x in 1:nrow(tx_flights)) { # Loop through DF
  if(tx_flights[x,"DEP_DELAY_NEW"] > 0) {
    name <- tx_flights[x,"DEST_CITY_NAME"] # Get Name of city
    sum_of_delay_times_city[name] <- sum_of_delay_times_city[name] + tx_flights[x,"DEP_DELAY_NEW"] # Add to sum
    num_of_each_city[name] <- num_of_each_city[name] + 1
  }
}

no_zeros_delays <- tx_flights %>% filter(DEP_DELAY_NEW > 0)

# Average delay time 
avg_of_delay_times_city <- sum_of_delay_times_city / table(no_zeros_delays$DEST_CITY_NAME)

# Round Delay time
avg_of_delay_times_city <- round(avg_of_delay_times_city, digits = 2)


# Plot of Average delay time in top 5 cities 
ggplot(data = head(arrange(as.data.frame(avg_of_delay_times_city), desc(Freq))), aes(x = reorder(Var1, -Freq), y= Freq)) +
  geom_bar(stat="identity", fill="Steelblue") +
  geom_text(aes(label=Freq), vjust=2.0, color="black", size=10) +
  xlab("City") +
  ylab("Average Delay Time (mins)") +
  ggtitle("Top 5 Destination Cities with highest Average Delay Times") +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) 
# -------------  Average Delay times for State  ----------------------
dest_states <- unique(no_zeros_delays$DEST_STATE_NM) # Get all City Names
dest_states<- sort(dest_states) # Sort City names
sum_of_delay_times_states <- (1:50 * 0) # Set all sums to 0
names(sum_of_delay_times_states) <- dest_states # name sum vector
for (x in 1:nrow(no_zeros_delays)) { # Loop through DF
  name <- no_zeros_delays[x,"DEST_STATE_NM"] # Get Name of city
  sum_of_delay_times_states[name] <- sum_of_delay_times_states[name] + no_zeros_delays[x,"DEP_DELAY_NEW"] # Add to sum
}

# Average delay time 
avg_of_delay_times_states <- sum_of_delay_times_states / table(no_zeros_delays$DEST_STATE_NM)

# Round Delay time
avg_of_delay_times_states <- round(avg_of_delay_times_states, digits = 2)


# Plot of Average delay time in top 5 cities 
ggplot(data = head(arrange(as.data.frame(avg_of_delay_times_states), desc(Freq))), aes(x = reorder(Var1, -Freq), y= Freq)) +
  geom_bar(stat="identity", fill="Steelblue") +
  geom_text(aes(label=Freq), vjust=2.0, color="black", size=10) +
  xlab("State") +
  ylab("Average Delay Time (mins)") +
  ggtitle("Top 5 States with highest Average Delay Times") +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5))

#-Question 3.2-------------------------------------------------------(DONE)-----

# Bar plots of Number of Cancelled flights to Texas Cities 
ggplot(data = subset(tx_flights, CANCELLED == 1 & DEST_STATE_NM == "Texas"),
       aes(x = YEAR, fill = factor(MONTH))) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  labs(title = "Cancellations of Domestic Flights to Texas", x = "Year", 
       y = "Number of Cancellations") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), 
                    values = c("steelblue", "salmon")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

# Bar plots of Number of Delayed flights to Texas Cities 
ggplot(data = subset(tx_flights, DEP_DELAY > 0 & DEST_STATE_NM == "Texas"), 
       aes(x = YEAR, fill = factor(MONTH))) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  labs(title = "Delays of Domestic Flights to Texas", x = "Year", 
       y = "Number of Delays") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), 
                    values = c("steelblue", "salmon")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

# Bar plots of Number of Cancelled flights from Texas Cities 
ggplot(data = subset(tx_flights, CANCELLED == 1 & DEST_STATE_NM != "Texas"), 
       aes(x = YEAR, fill = factor(MONTH))) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  labs(title = "Cancellations of Domestic Flights from Texas", x = "Year", 
       y = "Number of Cancellations") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), 
                    values = c("steelblue", "salmon")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

# Bar plots of Number of Delayed flights from Texas Cities 
ggplot(data = subset(tx_flights, DEP_DELAY > 0 & DEST_STATE_NM != "Texas"), 
       aes(x = YEAR, fill = factor(MONTH))) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  labs(title = "Delays of Domestic Flights from Texas", x = "Year", 
       y = "Number of Delays") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), 
                    values = c("steelblue", "salmon")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#----Question 3.3------------------------------------------------(DONE)---------

# Sorted out the top 10 airline carriers with highest count of delayed flights 
sort(table(no_zeros_delays$OP_UNIQUE_CARRIER), decreasing = TRUE)

# Get the Names of those same airlines carriers 
top_10_airlines <- names(sort(table(no_zeros_delays$OP_UNIQUE_CARRIER), 
                              decreasing = TRUE)[1:10])

# Make a new data frame using only those airlines carriers 
df_top_10_airlines <- no_zeros_delays[which(no_zeros_delays$OP_UNIQUE_CARRIER 
                                            %in% top_10_airlines), ]

# 10 Box plots of Delay Time Distribution of top ten Airline Carriers 
ggplot(df_top_10_airlines, aes(x = as.factor(OP_UNIQUE_CARRIER), 
                               y = DEP_DELAY_NEW, 
                               fill = as.factor(OP_UNIQUE_CARRIER))) + 
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Delay Time per Top 10 Airlines",
       x = "Unique Carrier",
       y = "Delay Time in Minutes") +
  ylim(0, 20) +
  theme_economist() +
  scale_fill_manual(values = rep("steelblue4", 10)) + 
  scale_x_discrete(labels = c("American Airlines", "Delta Air Lines", 
                              "ExpressJet Airlines", "Frontier Airlines", 
                              "Envoy Air", "Spirit Air Lines", 
                              "SkyWest Airlines", "United Air Lines", 
                              "Southwest Airlines", "Mesa Airlines")) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20),
        axis.text.x = element_text(angle = 70, vjust = 1.05, hjust = 1), 
        legend.position = "none")

#--- Question 4.2 ----------------------------------------------------(DONE)----

# Make sure each day has roughly the same number of flights (for flights coming 
# into Texas) to directly compare count of cancellations for each day and delay 
# distribution for each day
sort(table(tx_flights[tx_flights$DEST_STATE_NM == "Texas", ]$DAY_OF_WEEK), 
     decreasing = TRUE)


# Check if there were flights with an unknown date of the week
unique(no_zeros_delays$DAY_OF_WEEK)

# Changing DAY_OF_WEEK column into a factor variable
no_zeros_delays$DAY_OF_WEEK <- as.factor(no_zeros_delays$DAY_OF_WEEK)

# Box plots of Delay Time Distribution for each day of the week 
# (for flights coming into Texas)
ggplot(data = subset(no_zeros_delays, DEST_STATE_NM == "Texas"), 
       aes(x = DAY_OF_WEEK, y = DEP_DELAY_NEW)) + 
  geom_boxplot() +
  labs(title = "Delay Time per Day of the Week of Texas Flights", 
       x = "Day of the Week", 
       y = "Delay Time in Minutes") +
  ylim(0, 175) +
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday", "Sunday"))


# Bar plots of count of cancelled flights for each day of the week 
# (for flights coming into Texas)
ggplot(data = subset(tx_flights, CANCELLED == 1 & DEST_STATE_NM == "Texas"), 
       aes(x = DAY_OF_WEEK)) + 
  geom_bar(stat = 'count',  position = 'dodge') +
  labs(title = "Count of Cancelled Texas Flights per Day of the Week", 
       x = "Day of the Week", 
       y = "Count of Cancelled Flights") +
  scale_fill_manual(values = rep("steelblue4", 7)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday", "Sunday"))

#---Question 8a (freq delay time per city in Texas)-----------------------------------------------------------------
ggplot(data = subset(no_zeros_delays, DEST_STATE_NM == "Texas")) +
  geom_count(aes(x = DEST_CITY_NAME, y = DEP_DELAY_NEW), color = "steelblue") +
  scale_x_discrete(labels = c("Abilene","Amarillo","Austin","Beaumont","Brownsville","College Station","Corpus Christi",
                              "Dallas","Fort Worth","Del Rio","El Paso","Harlingen","Houston","Killeen","Laredo",
                              "Longview","Lubbock","Midland","Mission","San Angelo","San Antonio","Tyler","Victoria",
                              "Waco","Wichita Falls")) +
  scale_color_discrete(name = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "City", 
       y = "Delay Time",
       title = "Delay Time and Frequency based on Cities in Texas") +
  ylim(0, 100) +
  theme(plot.title = element_text(hjust = 0.5))

#---Question 8b (freq delay time per State)-----------------------------------------------------------------

median_of_delay_times_states <- c()

for (x in dest_states)
{
  median_of_delay_times_states <- c(median_of_delay_times_states, 
                                    median(tx_flights$DEP_DELAY_NEW[which(x == tx_flights$DEST_STATE_NM & tx_flights$DEP_DELAY_NEW > 0)]))
}

names(median_of_delay_times_states) <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", 
                                         "FL", "GA", "HI", "ID", "IL", "IN", "IA", 
                                         "KS", "KY", "LA", "ME", "MD", "MA", "MI", 
                                         "MS", "MN", "MO", "MT", "NE", "NV", "NJ", 
                                         "NM", "NY", "NC", "ND", "OH", "OK", "OR", 
                                         "PA", "Puerto Rico", "RI", "SC", "SD", "TN", 
                                         "TX", "U.S. Virgin Islands", "UT", "VT", 
                                         "VI", "WA", "WV", "WI", "WY")      
median_of_delay_times_states_df <- data.frame(State_Name = names(median_of_delay_times_states), 
                                              Delay_Time_Median = median_of_delay_times_states)
ggplot(median_of_delay_times_states_df, aes(x = State_Name, y = Delay_Time_Median)) +
  geom_point()


#dpt_June_2018 <- tx_flights  %>% filter(YEAR == 2018 & MONTH == 6 & ORIGIN_STATE_NM == "Texas")

# plot
#g <- ggplot(tx_flights[tx_flights$ORIGIN_STATE_NM == "Texas",],
#aes(x = as.factor(YEAR), y = DEP_DELAY)) + geom_boxplot()



#----------(Looking at question 1)--------------------------------
ggplot(data = subset(no_zeros_delays, DEST_STATE_NM == "Texas"), aes(x = as.factor(MONTH), y = DEP_DELAY_NEW)) +
  geom_boxplot() +
  ylim(0, 75) +
  facet_wrap(~YEAR)
#----------Question 3.1-------------------------------------(DONE)--------------

# Histogram  Displaying Distribution of Delay times
ggplot(data = no_zeros_delays, aes(x = DEP_DELAY_NEW)) +
  geom_histogram(bins = 50, color = "black", fill = "steelblue") +
  xlim(0, 600) +
  ylim(0, 110000) +
  labs(x = "Departure Time in Minutes", y = "Number of Delays", 
       title = "Distribution of Delay Times") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 20)) +
  geom_vline(aes(xintercept = mean(DEP_DELAY_NEW), color = "mean"), 
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(DEP_DELAY_NEW), color = "median"), 
             linetype = "dashed", size = 1) +
  scale_color_manual(name = "Labels", 
                     values = c(median = "aquamarine3", mean = "salmon"))

# Quantile for each quarter of the distribution 
quantile_delay_times <- quantile(no_zeros_delays$DEP_DELAY_NEW)
print(quantile_delay_times) 
#   0%  25%  50%   75%  100%
#    1    6   18    48  3890

# Mean of delay time distribution 
mean_delay_times <- sum(no_zeros_delays$DEP_DELAY_NEW) / sum(table(no_zeros_delays$DEP_DELAY_NEW))
print(mean_delay_times) 
# 41.68 minutes 

# Quantile for each 10th of the distribution 
quantile_10 <- c(1:10) / 10
quantile_10 <- quantile(no_zeros_delays$DEP_DELAY_NEW, probs = o_O)
print(quantile_10) 
#  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#    2    5    8   13   18   27   39   60  102 3890 

#-----------(Looking at question 5)--------------------------------
ggplot(data = subset(no_zeros_delays, YEAR == "2018" && MONTH == "6" && DAY_OF_WEEK == "2")) +
  geom_point(mapping = aes(x = DISTANCE, y = DEP_DELAY_NEW)) +
  ylim(0,2000)
ggplot(data = subset(no_zeros_delays, YEAR == "2020" && MONTH == "6" && DAY_OF_WEEK == "3")) +
  geom_point(mapping = aes(x = DISTANCE, y = DEP_DELAY_NEW))
#------------(Looking at question 10)-----------------------------
sort(table(tx_flights$OP_UNIQUE_CARRIER), decreasing = TRUE)
top_10_airlines_with_zero <- names(sort(table(tx_flights$OP_UNIQUE_CARRIER), decreasing = TRUE)[1:10])
df_top_10_airlines_with_zero <- tx_flights[which(tx_flights$OP_UNIQUE_CARRIER %in% top_10_airlines_with_zero), ]
#---Question 3.5---------------------------------------------------(DONE)-------

# Total Delay time for each delay cause
delay_causes_time <- c(sum(tx_flights$CARRIER_DELAY, na.rm = TRUE), 
                       sum(tx_flights$LATE_AIRCRAFT_DELAY, na.rm = TRUE), 
                       sum(tx_flights$NAS_DELAY, na.rm = TRUE), 
                       sum(tx_flights$SECURITY_DELAY, na.rm = TRUE), 
                       sum(tx_flights$WEATHER_DELAY, na.rm = TRUE))

# Count of flights for each delay cause
delay_causes_count <- c(sum(!is.na(tx_flights$CARRIER_DELAY) & 
                              0 != (tx_flights$CARRIER_DELAY)), 
                        sum(!is.na(tx_flights$LATE_AIRCRAFT_DELAY) & 
                              0 != (tx_flights$LATE_AIRCRAFT_DELAY)),
                        sum(!is.na(tx_flights$NAS_DELAY) & 
                              0 != (tx_flights$NAS_DELAY)), 
                        sum(!is.na(tx_flights$SECURITY_DELAY) & 
                              0 != (tx_flights$SECURITY_DELAY)),
                        sum(!is.na(tx_flights$WEATHER_DELAY) & 
                              0 != (tx_flights$WEATHER_DELAY)))

options(scipen = 9) # Remove Scientific notation

# Create Matrix with 2 vectors Above 
delay_causes <- cbind(delay_causes_time, delay_causes_count, 
                      c("Carrier", "Late Aircraft", "NAS", "Security", "Weather"))

# Name columns
colnames(delay_causes) <- c("Delay_cause_time", "Delay_cause_count", "Cause")

# Make Matrix into a data frame
delay_causes <- as.data.frame(delay_causes)

# Change columns into integer variables
delay_causes$Delay_cause_time <- as.integer(delay_causes$Delay_cause_time)
delay_causes$Delay_cause_count <- as.integer(delay_causes$Delay_cause_count)

# Bar plots of Delay times for each cause
ggplot(delay_causes, aes(x = Cause, y = Delay_cause_time)) +
  geom_bar(stat = 'identity', fill = "Steelblue") +
  labs(title = "Total Time for each Cause of Delay", 
       x = "Causes", 
       y = "Total Delay Time in Minutes") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20)) 

# Bar plots of count of flights for each cause
ggplot(delay_causes, aes(x = Cause, y = Delay_cause_count)) +
  geom_bar(stat = 'identity', fill = "Salmon") +
  labs(title = "Total Count for each Cause of Delay", 
       x = "Causes", 
       y = "Number of Delays") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20)) 

# Bar plots of Average Delay times for each cause
ggplot(delay_causes, 
       aes(x = Cause, y = (Delay_cause_time / Delay_cause_count))) +
  geom_bar(stat = 'identity', fill = "darkseagreen3") +
  labs(title =  " Average Delay Time for each Cause of Delay", 
       x = "Causes", 
       y = "Minutes") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20)) 
#------------(Looking at question 9)-----------------------------


tx_flights["DIV_REACHED_DEST"][tx_flights["DIV_REACHED_DEST"] == 0] <- 2

ggplot(data = subset(tx_flights,!is.na(DIV_ARR_DELAY) & DEST_STATE_NM == "Texas"), aes(x=as.factor(YEAR), color=as.factor(MONTH), y = DIV_ARR_DELAY)) + 
  geom_jitter() +
  geom_boxplot(outlier.shape = NA)

#labs(title = "Cancellations of Domestic Flights to Texas", x = "Year", y = "Number of Cancellations") +
#scale_fill_manual(name = "Month", labels = c("June", "July"), values= c("steelblue", "salmon")) +
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))

ggplot(data = subset(tx_flights,!is.na(DIV_ARR_DELAY) & DEST_STATE_NM == "Texas"), aes(x = DIV_ARR_DELAY, color = as.factor(DIV_REACHED_DEST))) +
  geom_density() +
  scale_x_discrete(drop=FALSE)


# ---- NEW Question -------------------
m1 <- cbind( table(df_top_10_airlines_with_zero$OP_UNIQUE_CARRIER),
             table(df_top_10_airlines$OP_UNIQUE_CARRIER), 
             c("American Airlines","Delta Air Lines", "ExpressJet Airlines","Frontier Airlines", "Envoy Air", 
               "Spirit Air Lines", "SkyWest Airlines","United Air Lines", "Southwest Airlines", "Mesa Airlines"))
colnames(m1) <- c("flight_count","delay_count", "airline_name")
m1 <- as.data.frame(m1)
m1$flight_count <- as.integer(m1$flight_count)
m1$delay_count <- as.integer(m1$delay_count)

ggplot(m1, aes(x =  flight_count, y = delay_count, color  = as.factor(airline_name))) +
  geom_point(aes(size = 5)) 

top_cities <- names( head(sort(table(tx_flights$DEST_CITY_NAME), decreasing = TRUE), 20))
m2 <- cbind( head(sort(table(tx_flights$DEST_CITY_NAME), decreasing = TRUE), 20)[sort(names(head(sort(table(tx_flights$DEST_CITY_NAME), decreasing = TRUE), 20)))],
             table(no_zeros_delays[which(no_zeros_delays$DEST_CITY_NAME %in% top_cities), ]$DEST_CITY_NAME),
             sort(names( head(sort(table(tx_flights$DEST_CITY_NAME), decreasing = TRUE), 20)), decreasing = FALSE)   )




colnames(m2) <- c("flight_count","delay_count", "city_name")
m2 <- as.data.frame(m2)
m2$flight_count <- as.integer(m2$flight_count)
m2$delay_count <- as.integer(m2$delay_count)

ggplot(m2, aes(x =  flight_count, y = delay_count, color  = as.factor(city_name))) +
  geom_point(aes(size = 5))+
  labs( x = "Number of Flights", y = "Number of Delays", title = "Flight vs. Delay for Top 20 Cities")+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))+
  scale_colour_discrete(name = "City Names")



top_10_cities <- names(sort(table(no_zeros_delays$DEST_CITY_NAME), decreasing = TRUE)[1:10])
df_top_10_cities <- no_zeros_delays[which(no_zeros_delays$DEST_CITY_NAME %in% top_10_cities), ]

ggplot(df_top_10_cities, aes(x = DEST_CITY_NAME , y = DEP_DELAY_NEW , fill = as.factor(DEST_CITY_NAME))) +
  geom_boxplot()+
  ylim(0, 100)

#werer
