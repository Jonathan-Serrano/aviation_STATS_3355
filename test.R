# Project Group 5
# Loading the necessary packages
#### NO STATE OF DELAWARE or NEW HAMPSHIRE (INSTEAD WE HAVE U.S VIRGINS AND PUERTO RICO) ####
library(ggplot2)
library(plyr)
library(dplyr)
library(ggthemes)

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

# Bind them Together
tx_flights <- rbind(June_2018, July_2018, 
                    June_2019, July_2019, 
                    June_2020, July_2020, 
                    June_2021, July_2021, 
                    June_2022, July_2022)
#tx_flights["month_year"] <- c(1: length(tx_flights$YEAR))

#for (x in 1:nrow(tx_flights)) {
 # if (tx_flights[x, YEAR] == 2018 & tx_flights[x, MONTH] == "June") {
 #   tx_flights["month_year"] <- 1
 # }
#}
#for (x in 1:nrow(tx_flights)) {
  #if (tx_flights$YEAR[x] == 2018 & tx_flights$MONTH[x] == "June") {
   # tx_flights$month_year[x] <- 1
  #}
#}

#dpt_July_2018 <- July_2018$DEP_DELAY[July_2018$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_July_2018 <- mean(dpt_July_2018, na.rm = TRUE)
#dpt_June_2018 <- June_2018$DEP_DELAY[June_2018$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_June_2018 <- mean(dpt_June_2018, na.rm = TRUE)
#dpt_July_2019 <- July_2019$DEP_DELAY[July_2019$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_July_2019 <- mean(dpt_July_2019, na.rm = TRUE)
#dpt_June_2019 <- June_2019$DEP_DELAY[June_2019$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_June_2019 <- mean(dpt_June_2019, na.rm = TRUE)
#dpt_July_2020 <- July_2020$DEP_DELAY[July_2020$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_July_2020 <- mean(dpt_July_2020, na.rm = TRUE)
#dpt_June_2020 <- June_2020$DEP_DELAY[June_2020$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_June_2020 <- mean(dpt_June_2020, na.rm = TRUE)
#dpt_July_2021 <- July_2021$DEP_DELAY[July_2021$ORIGIN_STATE_NM == "Texas"]
##mean_dpt_July_2021 <- mean(dpt_July_2021, na.rm = TRUE)
#dpt_June_2021 <- June_2021$DEP_DELAY[June_2021$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_June_2021 <- mean(dpt_June_2021, na.rm = TRUE)
#dpt_July_2022 <- July_2022$DEP_DELAY[July_2022$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_July_2022 <- mean(dpt_July_2022, na.rm = TRUE)
#dpt_June_2022 <- June_2022$DEP_DELAY[June_2022$ORIGIN_STATE_NM == "Texas"]
#mean_dpt_June_2022 <- mean(dpt_June_2022, na.rm = TRUE)



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
# ---------------------------------------------- ----------------------


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
# -Question 8b--------------------------------------------- ----------------------

#median_of_delay_times_states <- c()

#for (x in dest_states)
#{
#  median_of_delay_times_states <- c(median_of_delay_times_states, median(tx_flights$DEP_DELAY_NEW[which(x == tx_flights$DEST_STATE_NM & tx_flights$DEP_DELAY_NEW > 0)]))
#}
#names(median_of_delay_times_states) <- dest_states

#-Question 2a and b--------------------------------------------------------------------
June2018 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2018  & tx_flights$MONTH == 6)
July2018 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2018  & tx_flights$MONTH == 7)
June2019 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2019  & tx_flights$MONTH == 6)
July2019 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2019  & tx_flights$MONTH == 7)
June2020 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2020  & tx_flights$MONTH == 6)
July2020 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2020  & tx_flights$MONTH == 7)
June2021 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2021  & tx_flights$MONTH == 6)
July2021 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2021  & tx_flights$MONTH == 7)
June2022 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2022  & tx_flights$MONTH == 6)
July2022 <- (tx_flights$DEST_STATE_NM == "Texas" & tx_flights$CANCELLED == 1 & tx_flights$YEAR == 2022  & tx_flights$MONTH == 7)

# All Cancellations to Texas Cities
ggplot(data = subset(tx_flights, CANCELLED == 1), aes(x=YEAR, fill=factor(MONTH))) + 
  geom_bar(stat='count', position='dodge') + 
  labs(title = "Cancellations of Domestic Flights to Texas", x = "Year", y = "Number of Cancellations") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), values= c("red", "blue")) +
  theme(plot.title = element_text(hjust = 0.5))


# All Delays to Texas Cities
ggplot(data = subset(tx_flights, DEP_DELAY > 0), aes(x=YEAR, fill=factor(MONTH))) + 
  geom_bar(stat='count', position='dodge') + 
  labs(title = "Delays of Domestic Flights to Texas", x = "Year", y = "Number of Delays") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), values= c("red", "blue")) +
  theme(plot.title = element_text(hjust = 0.5))



# All Cancellations from Texas Cities to Everywhere else in USA
ggplot(data = subset(tx_flights, CANCELLED == 1 & DEST_STATE_NM != "Texas"), aes(x=YEAR, fill=factor(MONTH))) + 
  geom_bar(stat='count', position='dodge') + 
  labs(title = "Cancellations of Domestic Flights to Texas", x = "Year", y = "Number of Cancellations") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), values= c("red", "blue")) +
  theme(plot.title = element_text(hjust = 0.5))


# All Delays from Texas Cities to Everywhere else in USA
ggplot(data = subset(tx_flights, DEP_DELAY > 0 & DEST_STATE_NM != "Texas"), aes(x=YEAR, fill=factor(MONTH))) + 
  geom_bar(stat='count', position='dodge') + 
  labs(title = "Cancellations of Domestic Flights to Texas", x = "Year", y = "Number of Cancellations") +
  scale_fill_manual(name = "Month", labels = c("June", "July"), values= c("red", "blue")) +
  theme(plot.title = element_text(hjust = 0.5))

#-Question 8a--------------------------------------------------------------------

median_of_delay_times_cities <- c()

for (x in dest_cities)
{
  median_of_delay_times_cities <- c(median_of_delay_times_cities, median(tx_flights$DEP_DELAY_NEW[which(x == tx_flights$DEST_CITY_NAME & tx_flights$DEP_DELAY_NEW > 0)]))
}
names(median_of_delay_times_cities) <- dest_cities

#- Question 6 JUSTIFICATION (Question 7)--------------------------------------------------------------------

sort(table(no_zeros_delays$OP_UNIQUE_CARRIER), decreasing = TRUE)
top_10_airlines <- names(sort(table(no_zeros_delays$OP_UNIQUE_CARRIER), decreasing = TRUE)[1:10])
df_top_10_airlines <- no_zeros_delays[which(no_zeros_delays$OP_UNIQUE_CARRIER %in% top_10_airlines), ]

ggplot(df_top_10_airlines, aes(x = as.factor(OP_UNIQUE_CARRIER), y = DEP_DELAY_NEW, fill = as.factor(OP_UNIQUE_CARRIER))) + 
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Delay Time per Top 10 Airlines",
       x = "Unique Carrier",
       y = "Delay Time in Minutes") +
  ylim(0, 175) +
  theme_economist() +
  scale_fill_manual(values = c("lightblue", "blue", "steelblue", "navy", "grey", "lightblue","blue", "steelblue", "navy", "grey"),
                    name = "Airlines",
                    labels = c("American Airlines","Delta Air Lines", "ExpressJet Airlines","Frontier Airlines", "Envoy Air", 
                               "Spirit Air Lines", "SkyWest Airlines","United Air Lines", "Southwest Airlines", "Mesa Airlines")) +
  theme(plot.title = element_text(hjust = 0.5))

#-Question 6a-------------------------------------------------------------------------------------------

unique(df_top_10_airlines$DAY_OF_WEEK)
df_top_10_airlines$DAY_OF_WEEK <- as.factor(df_top_10_airlines$DAY_OF_WEEK)
ggplot(data = df_top_10_airlines, aes(x = DAY_OF_WEEK, y = DEP_DELAY_NEW)) + 
  geom_boxplot() +
  labs(title = "Delay Time per Day of the Week", 
       x = "Day of the Week", 
       y = "Delay Time in Minutes") +
  ylim(0, 175) +
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday", "Sunday"))


#-Question 6b-------------------------------------------------------------------------------------------

#Not sure how to get flight time from the data frames we have already...
#I don't think that data is there....

#---Question 8a (freq delay time per city in Texas)-----------------------------------------------------------------
texas_cities <- c(outlier.shape = NA)
for (x in 1:length(dest_cities))
    {
      texas_cities <- append(texas_cities, grepl("TX", dest_cities[x]))
    }
texas_cities <- dest_cities[texas_cities]

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

#- Question 8a -- Take 2-----------------------------------------------------------------------------------------------

no_zeros_delays["Delay_Range"] <- 0
no_zeros_delays$Delay_Range[no_zeros_delays$DEP_DELAY_NEW > 0 & no_zeros_delays$DEP_DELAY_NEW <= 10] <- 1
no_zeros_delays$Delay_Range[no_zeros_delays$DEP_DELAY_NEW > 10 & no_zeros_delays$DEP_DELAY_NEW <= 20] <- 2
no_zeros_delays$Delay_Range[no_zeros_delays$DEP_DELAY_NEW > 20 & no_zeros_delays$DEP_DELAY_NEW <= 30] <- 3
no_zeros_delays$Delay_Range[no_zeros_delays$DEP_DELAY_NEW > 30 & no_zeros_delays$DEP_DELAY_NEW <= 40] <- 4
no_zeros_delays$Delay_Range[no_zeros_delays$DEP_DELAY_NEW > 40] <- 5

ggplot(data = subset(no_zeros_delays, DEST_STATE_NM == "Texas" & OP_UNIQUE_CARRIER %in% top_10_airlines),
       aes(Delay_Range,DEST_CITY_NAME)) + # Create heatmap with ggplot2
  geom_tile(aes(fill = Delay_Range))


  


  


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
ggplot(data = no_zeros_delays, aes(x = as.factor(MONTH), y = DEP_DELAY_NEW)) +
  geom_boxplot() +
  ylim(0, 75) +
  facet_wrap(~YEAR)
#----------(Looking at question 3)--------------------------------
ggplot(data = no_zeros_delays, aes(x = as.factor(FLIGHTS), y = DEP_DELAY_NEW)) +
  geom_boxplot() +
  ylim(0, 175)
quantile_delay_times <- quantile(no_zeros_delays$DEP_DELAY_NEW)
mean_delay_times <- sum(no_zeros_delays$DEP_DELAY_NEW) / sum(table(no_zeros_delays$DEP_DELAY_NEW))
print(quantile_delay_times)
print(mean_delay_times)
o_O <- c(1:10) / 10
o_O_quantile <- quantile(no_zeros_delays$DEP_DELAY_NEW, probs = o_O)
#could look at flights just from Texas, or just to certain cities, etc...potentially interesting
#-----------(Looking at question 5)--------------------------------
ggplot(data = subset(no_zeros_delays, YEAR == "2018" && MONTH == "6" && DAY_OF_WEEK == "2")) +
  geom_point(mapping = aes(x = DISTANCE, y = DEP_DELAY_NEW))
ggplot(data = subset(no_zeros_delays, YEAR == "2020" && MONTH == "6" && DAY_OF_WEEK == "3")) +
  geom_point(mapping = aes(x = DISTANCE, y = DEP_DELAY_NEW))
#------------(Looking at question 10)-----------------------------
sort(table(tx_flights$OP_UNIQUE_CARRIER), decreasing = TRUE)
top_10_airlines_with_zero <- names(sort(table(tx_flights$OP_UNIQUE_CARRIER), decreasing = TRUE)[1:10])
df_top_10_airlines_with_zero <- tx_flights[which(tx_flights$OP_UNIQUE_CARRIER %in% top_10_airlines_with_zero), ]
#------------(Looking at question 4)-----------------------------
# only coming into texas 
# count and the sum of each delay type 


delay_causes_time <- c(sum(tx_flights$CARRIER_DELAY,na.rm=TRUE), sum(tx_flights$WEATHER_DELAY,na.rm=TRUE), 
                  sum(tx_flights$NAS_DELAY,na.rm=TRUE), sum(tx_flights$SECURITY_DELAY,na.rm=TRUE), sum(tx_flights$LATE_AIRCRAFT_DELAY,na.rm=TRUE))


delay_causes_count <- c(sum(!is.na(tx_flights$CARRIER_DELAY) & 0 != (tx_flights$CARRIER_DELAY)), sum(!is.na(tx_flights$WEATHER_DELAY) & 0 != (tx_flights$WEATHER_DELAY)),
                        sum(!is.na(tx_flights$NAS_DELAY) & 0 != (tx_flights$NAS_DELAY)), sum(!is.na(tx_flights$SECURITY_DELAY) & 0 != (tx_flights$SECURITY_DELAY)),
                        sum(!is.na(tx_flights$LATE_AIRCRAFT_DELAY) & 0 != (tx_flights$LATE_AIRCRAFT_DELAY)))

mat3 <- cbind(delay_causes_time,delay_causes_count)
ggplot(as.data.frame(mat3), aes(x = nrow(as.data.frame(mat3)), y= delay_causes_time))+
  geom_bar()

