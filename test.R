# Project Group 5
# Loading the necessary packages
library(ggplot2)
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
tx_flights <- rbind(June_2018, July_2018, 
                    June_2019, July_2019, 
                    June_2020, July_2020, 
                    June_2021, July_2021, 
                    June_2022, July_2022)
tx_flights["month_year"] <- c(1: length(tx_flights$YEAR))

for (x in 1:nrow(tx_flights)) {
  if (tx_flights[x, YEAR] == 2018 & tx_flights[x, MONTH] == "June") {
    tx_flights["month_year"] <- 1
  }
}
for (x in 1:nrow(tx_flights)) {
  #if (tx_flights$YEAR[x] == 2018 & tx_flights$MONTH[x] == "June") {
    tx_flights$month_year[x] <- 1
  #}
}

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
# plot
g <- ggplot(tx_flights[tx_flights$ORIGIN_STATE_NM == "Texas",],
            aes(x = as.factor(YEAR), y = DEP_DELAY)) + geom_boxplot()
