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

# # plot evn
# ggplot(evn_data, aes(x = Date)) +
#     geom_histogram(binwidth = 30, color = "black", fill = "white") +
#     labs(title = "Evictions by Month",
#          x = "Date",
#          y = "Count")

# # plot hpi
# ggplot(hpi_data, aes(x = Date, y = hpi)) +
#     geom_line(color = "black") +
#     labs(title = "HPI by Month",
#          x = "Date",
#          y = "HPI")

# group evn by month and just use first day of each month
evn_data <- evn_data %>%
    group_by(Date = floor_date(Date, "month")) %>%
    summarise(count_by_month = n())

# plot evn and hpi together but hpi is too small to see so scale it up by 15
ggplot() +
    geom_line(data = evn_data, aes(x = Date, y = count_by_month), color = "black") +
    geom_line(data = hpi_data, aes(x = Date, y = hpi * 10), color = "red") +
    labs(title = "Evictions and HPI by Month",
         x = "Date",
         y = "Count")



# # since hpi data is monthly, group eviction data by month and year
# # also change date format to just year and month
# evn_data <- evn_data %>%
#     group_by(year = format(Date, "%Y"), month = format(Date, "%m")) %>%
#     summarise(count = n())
# hpi_data <- hpi_data %>%
#     group_by(year = format(Date, "%Y"), month = format(Date, "%m")) %>%
#     summarise(hpi = mean(hpi))