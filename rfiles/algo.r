library(dplyr)
library(tidyr)


# function for dataset to be easier to work with
restructure <- function(data) {
  data <- mutate_file_point(data)
  data <- delete_unnecessary_columns(data)
  data <- format_types(data)
  return(na.omit(data))
}

# function to mutate file date and shape column
mutate_file_point <- function(data) {
  return(data %>%
    mutate(
      File.Date = as.Date(data$File.Date, format = "%m/%d/%Y"),
      longitude = as.double(
        gsub("\\s.*", "", gsub("POINT \\(", "", data$Shape))),
      latitude = as.double(
        gsub("\\).*", "", gsub(".*\\s", "", gsub("POINT \\(", "", data$Shape)))
      )
    )
  )
}

# function to delete unnecessary columns
delete_unnecessary_columns <- function(data) {
  return(data %>%
    select(
      -c("Eviction.ID", "Address", "City", "State", "Constraints.Date",
        "Location", "Shape", "SF.Find.Neighborhoods", "Analysis.Neighborhoods",
        "Current.Police.Districts", "Eviction.Notice.Source.Zipcode",
        "DELETE...Neighborhoods", "DELETE...Police.Districts",
        "DELETE...Supervisor.Districts", "DELETE...Zip.Codes",
        "Supervisor.District", "DELETE...Fire.Prevention.Districts",
        "CBD..BID.and.GBD.Boundaries.as.of.2017", "Neighborhoods",
        "Areas.of.Vulnerability..2016", "Central.Market.Tenderloin.Boundary",
        "Central.Market.Tenderloin.Boundary.Polygon...Updated",
        "Fix.It.Zones.as.of.2018.02.07", "Current.Supervisor.Districts",
        "Central.Market.Tenderloin.Boundary")
    )
  )
}

# function to format eviction types into one col
format_types <- function(data) {
  return(data <- data %>%
    pivot_longer(
      cols =
        c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
          "Failure.to.Sign.Renewal", "Access.Denial", "Unapproved.Subtenant",
          "Owner.Move.In", "Demolition", "Capital.Improvement",
          "Ellis.Act.WithDrawal", "Condo.Conversion", "Roommate.Same.Unit",
          "Other.Cause", "Late.Payments", "Lead.Remediation", "Development",
          "Good.Samaritan.Ends", "Substantial.Rehab"),
      names_to = "eviction_type",
      values_to = "eviction_count",
      values_transform = list(eviction_count = as.logical)) %>%
  filter(eviction_count == TRUE) %>% # nolint
  select(-eviction_count)
  )
}

# function to divide eviction types by fault and no fault
filter_fault_no_fault <- function(data) {
  return(data <- data %>%
    mutate(
      Fault = ifelse(
        eviction_type %in% c(
          "Non.Payment", "Breach", "Nuisance", "Failure.to.Sign.Renewal", "Illegal.Use",
          "Access.Denial", "Unapproved.Subtenant", "Roommate.Same.Unit", "Other.Cause",
          "Late.Payments"
        ),
        TRUE,
        FALSE
      ),
      No_Fault = ifelse(
        eviction_type %in% c(
          "Owner.Move.In", "Demolition", "Capital.Improvement", "Ellis.Act.Withdrawal",
          "Condo.Conversion", "Development", "Good.Samaritan.Ends",
          "Substantial.Rehab", "Lead.Remediation"
        ),
        TRUE,
        FALSE
      )
    ) %>%
    select(-eviction_type)
  )
}

filter_no_2023 <- function(data) {
  return(data %>%
    filter(File.Date < "2023-01-01")
  )
}

# function to divide eviction types into 4 categories
filter_eviction_types_small <- function(data) {
  return(data %>%
    mutate(
      eviction_category = case_when(
        eviction_type %in% c("Non.Payment", "Late.Payments",
                             "Failure.to.Sign.Renewal", "Good.Samaritan.Ends"
                             ) ~ "Financial",
        eviction_type %in% c("Breach", "Nuisance", "Illegal.Use",
                             "Access.Denial", "Unapproved.Subtenant",
                             "Roomate.Same.Unit") ~ "Fault",
        eviction_type %in% c("Owner.Move.In", "Condo.Conversion", "Demolition",
                             "Capital.Improvement", "Substantial.Rehab",
                             "Lead.Remediation", "Development",
                             "Ellis.Act.Withdrawal") ~ "Property_Change",
        TRUE ~ "Other"
      )
    ) %>%
    select(-eviction_type) %>%
    arrange(File.Date)
  )
}

# function to get evictions by season
get_evictions_by_season <- function(data) {
  spring_evictions <- data %>%
    filter(substring(data$File.Date, 6, 10) >= "03-01" &
                substring(data$File.Date, 6, 10) <= "05-31")
  summer_evictions <- data %>%
    filter(substring(data$File.Date, 6, 10) >= "06-01" &
             substring(data$File.Date, 6, 10) <= "08-31")
  fall_evictions <- data %>%
    filter(substring(data$File.Date, 6, 10) >= "09-01" &
             substring(data$File.Date, 6, 10) <= "11-30")
  winter_evictions <- data %>%
    filter(substring(data$File.Date, 6, 10) >= "12-01" &
             substring(data$File.Date, 6, 10) <= "2-29")
  return(list(
    spring_evictions, summer_evictions, fall_evictions, winter_evictions))
}

# function to get evictions by month
get_evictions_by_month <- function(data) {
  # Extract month and month name from File.Date
  data <- data %>%
    mutate(Month = lubridate::month(File.Date),
           Month_Name = month.name[lubridate::month(File.Date)])
  
  # Group by Month and Month_Name, and count the number of evictions
  counts <- data %>%
    group_by(Month, Month_Name) %>%
    summarise(Eviction_Count = n())
  
  return(counts)
}

# function to get eviction counts by month and year
get_counts_by_month_year <- function(data) {
  # Extract year, month, and month name from File.Date
  data <- data %>%
    mutate(Year = lubridate::year(File.Date),
           Month = lubridate::month(File.Date),
           Month_Name = month.name[lubridate::month(File.Date)])
  
  # Group by Year, Month, and Month_Name, and count the number of evictions
  counts <- data %>%
    group_by(Year, Month, Month_Name) %>%
    summarise(Eviction_Count = n())
  
  return(counts)
}

# function to get eviction counts by year
# keep eviction types as well
get_counts_by_year_preserve_type <- function(data) {
  # Extract year from File.Date
  data <- data %>%
    mutate(Year = lubridate::year(File.Date))
  
  # Group by Year, and count the number of evictions
  counts <- data %>%
    group_by(Year, eviction_type) %>%
    summarise(Eviction_Count = n())
  
  return(counts)
}

get_counts_by_year <- function(data) {
  # Extract year from File.Date
  data <- data %>%
    mutate(Year = lubridate::year(File.Date))
  
  # Group by Year, and count the number of evictions
  counts <- data %>%
    group_by(Year) %>%
    summarise(Eviction_Count = n())
  
  return(counts)
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