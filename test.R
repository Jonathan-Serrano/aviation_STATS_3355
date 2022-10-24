# Project Group 5
# Loading the necessary packages
#### NO STATE OF DELAWARE or NEW HAMPSHIRE (INSTEAD WE HAVE U.S VIRGINS AND PUERTO RICO) ####
library(ggplot2)
library(plyr)
library(dplyr)

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
# -Question 8a--------------------------------------------- ----------------------

median_of_delay_times_states <- c()

for (x in dest_states)
{
  median_of_delay_times_states <- c(median_of_delay_times_states, median(tx_flights$DEP_DELAY_NEW[which(x == tx_flights$DEST_STATE_NM & tx_flights$DEP_DELAY_NEW > 0)]))
}
names(median_of_delay_times_states) <- dest_states

#-Question 8b--------------------------------------------------------------------

median_of_delay_times_cities <- c()

for (x in dest_cities)
{
  median_of_delay_times_cities <- c(median_of_delay_times_cities, median(tx_flights$DEP_DELAY_NEW[which(x == tx_flights$DEST_CITY_NAME & tx_flights$DEP_DELAY_NEW > 0)]))
}
names(median_of_delay_times_cities) <- dest_cities

#-Question 6a--------------------------------------------------------------------

sort(table(no_zeros_delays$OP_UNIQUE_CARRIER), decreasing = TRUE)
top_10_airlines <- names(sort(table(no_zeros_delays$OP_UNIQUE_CARRIER), decreasing = TRUE)[1:10])
df_top_10_airlines <- no_zeros_delays[which(no_zeros_delays$OP_UNIQUE_CARRIER %in% top_10_airlines), ]

ggplot(df_top_10_airlines, aes(x = OP_UNIQUE_CARRIER, y = DEP_DELAY_NEW)) + 
  geom_boxplot(outlier.shape = NA, fill = "steelblue") +
  labs(title = "Delay Time per Top 10 Airlines",
       x = "Unique Carrier",
       y = "Delay Time in Minutes") +
  theme(legend.title = element_blank())
#  scale_color_manual(name = "Legend",
 #      breaks = c("American Airlines","Delta Air Lines", "ExpressJet Airlines", "Frontier Airlines", "Envoy Air", "Spirit Air Lines", "SkyWest Airlines", "United Air Lines", "Southwest Airlines", "Mesa Airlines" ))










#dpt_June_2018 <- tx_flights  %>% filter(YEAR == 2018 & MONTH == 6 & ORIGIN_STATE_NM == "Texas")

# plot
#g <- ggplot(tx_flights[tx_flights$ORIGIN_STATE_NM == "Texas",],
            #aes(x = as.factor(YEAR), y = DEP_DELAY)) + geom_boxplot()
