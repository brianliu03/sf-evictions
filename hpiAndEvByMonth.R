library(tidyverse)

# read data
evn_data <- as_tibble(data.frame(
    read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
))
hpi_data <- as_tibble(data.frame(
    read.csv("hpi.csv", header = TRUE, sep = ",")
))

# clean data
evn_data <- evn_data %>%
    mutate(
        file_date = as.Date(file_date, format = "%m/%d/%Y"),
        month = format(file_date, "%m"),
        year = format(file_date, "%Y")
    ) %>%
    group_by(year, month) %>%
    summarise(
        evn_count = n()
    )

hpi_data <- hpi_data %>%
    mutate(
        month = format(date, "%m"),
        year = format(date, "%Y")
    ) %>%
    filter(
        year >= 1997,
        year <= 2018
    ) %>%
    group_by(year, month) %>%
    summarise(
        hpi = mean(hpi)
    )
