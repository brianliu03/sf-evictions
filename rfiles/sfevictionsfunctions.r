library(tidyverse)

# function to get evictions by season
get_evictions_by_season <- function(data) {
  spring_evictions <- data %>%
    filter(substring(File.Date, 6, 10) >= "03-01" &
             substring(File.Date, 6, 10) <= "05-31")
  summer_evictions <- data %>%
    filter(substring(File.Date, 6, 10) >= "06-01" &
             substring(File.Date, 6, 10) <= "08-31")
  fall_evictions <- data %>%
    filter(substring(File.Date, 6, 10) >= "09-01" &
             substring(File.Date, 6, 10) <= "11-30")
  winter_evictions <- data %>%
    filter(substring(File.Date, 6, 10) >= "12-01" &
             substring(File.Date, 6, 10) <= "2-29")
  return(list(
    spring_evictions, summer_evictions, fall_evictions, winter_evictions))
}

# function to get evictions by year
get_evictions_by_year <- function(data) {
  eviction_data_by_year <- list()
  for (i in 1:26) {
    eviction_data_by_year[[i]] <- data %>%
      filter(substring(File.Date, 1, 4) == toString(1996 + i))
  }
  return(eviction_data_by_year)
}

# function to get evictions by month
get_evictions_by_month <- function(data) {
  eviction_data_by_month <- list()
  for (i in 1:12) {
    if (i >= 10) {
      eviction_data_by_month[[i]] <- data %>%
        filter(substring(File.Date, 6, 7) == toString(i))
    } else {
      eviction_data_by_month[[i]] <- data %>%
        filter(substring(File.Date, 6, 7) == paste0("0", toString(i)))
    }
  }
  return(eviction_data_by_month)
}

# function to get eviction counts by month
get_eviction_counts_by_month <- function(data) {
  eviction_counts_by_month <- get_evictions_by_month(data)
  for (i in 1:12) {
    eviction_counts_by_month[[i]] <- eviction_counts_by_month[[i]] %>%
      summarise(count = n()) %>%
      pull(count)
  }
  return(eviction_counts_by_month)
}

# function to get eviction counts by month and year
get_counts_by_month_year <- function(data) {
    eviction_counts_month_year <- list()
    for (i in 1:312) {
        if ((i - 1) %% 12 + 1 >= 10) {
            eviction_counts_month_year[[i]] <- data %>%
                filter(
                    substring(File.Date, 1, 4) == toString(1997 + floor((i - 1) / 12)) &
                         substring(File.Date, 6, 7) == toString((i - 1) %% 12 + 1)) %>%
                summarise(count = n()) %>%
                pull(count)
        } else {
            eviction_counts_month_year[[i]] <- data %>%
                filter(
                    substring(
                        File.Date, 1, 4) == toString(1997 + floor((i - 1) / 12)) &
                        substring(File.Date, 6, 7) == paste0("0", toString((i - 1) %% 12 + 1))) %>%
                summarise(count = n()) %>%
                pull(count)
        }
    }
    return(eviction_counts_month_year)
}

# function to create a boostrap estimate of 95% confidence
# interval for mean eviction counts by season and year
get_bs_ci <- function(data) {
  n_boot <- 10000
  size_boot <- length(data)
  counts_boot <- rep(0, n_boot)

  for (i in 1:n_boot) {
    sample_boot <- sample(data, size_boot, replace = TRUE)
    counts_boot[i] <- mean(sample_boot, na.rm = TRUE)
  }

  return(quantile(counts_boot, c(0.025, 0.975)))
}

# function to get eviction counts by season and year
get_counts_season_year <- function(data) {
  eviction_counts_by_season <- get_evictions_by_season(data)
  eviction_counts_by_year <- get_evictions_by_year(data)
  for (i in 1:4) {
    eviction_counts_by_season[[i]] <- eviction_counts_by_season[[i]] %>%
      summarise(count = n()) %>%
      pull(count)
  }
  for (i in 1:26) {
    eviction_counts_by_year[[i]] <- eviction_counts_by_year[[i]] %>%
      summarise(count = n()) %>%
      pull(count)
  }
  return(list(eviction_counts_by_season, eviction_counts_by_year))
}