library(tidyverse)

# read data
evn_data <- as_tibble(data.frame(
    read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
))
hpi_data <- as_tibble(data.frame(
    read.csv("hpi.csv", header = TRUE, sep = ",")
))

# change to date format
evn_data$Date <- as.Date(evn_data$File.Date, format = "%m/%d/%Y")
hpi_data$Date <- as.Date(hpi_data$date, format = "%Y-%m-%d")

# hpi only goes to june 2023 so use eviction data up to that point
# evn only starts in 1997-01-01 so use hpi data from that point
evn_data <- evn_data %>%
    filter(Date < "2023-06-01")
hpi_data <- hpi_data %>%
    filter(Date >= "1997-01-01")

# since hpi data is monthly, group eviction data by month and year
# also change date format to just year and month
evn_data <- evn_data %>%
    group_by(year = format(Date, "%Y"), month = format(Date, "%m")) %>%
    summarise(count = n())
hpi_data <- hpi_data %>%
    group_by(year = format(Date, "%Y"), month = format(Date, "%m")) %>%
    summarise(hpi = mean(hpi))