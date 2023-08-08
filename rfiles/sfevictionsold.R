library(osmdata)
library(tidyverse)
library(sf)
library(data.table)

# # download OpenStreetMap data for San Francisco
# sf_map <- opq("San Francisco") %>%
#   add_osm_feature(key = "highway") %>%
#   osmdata_sf()

source("sfevictionsfunctions.R")

# read in eviction data
eviction_data <- read.csv("sfevictionnotices.csv", header = TRUE, sep = ",")
eviction_data <- as_tibble(data.frame(eviction_data))

# add longitude and latitude columns to eviction data
# and convert File.Date to Date
eviction_data <- eviction_data %>%
  mutate(
    longitude = as.double(
      gsub("\\s.*", "",
        gsub("POINT \\(", "", eviction_data$Shape))),
    latitude = as.double(
      gsub("\\).*", "",
          gsub(".*\\s", "",
              gsub("POINT \\(", "", eviction_data$Shape)))),
    File.Date = as.Date(File.Date, format = "%m/%d/%Y"), )

counts_by_month_year <- get_counts_by_month_year(eviction_data)

# convert counts_by_month_year to data frame with two columns, month and count
counts_by_month_year <- as_tibble(data.frame(
  month = rep(1:312),
  count = unlist(counts_by_month_year)))


# analyze below 4 variables

evictions_2009_2012 <- eviction_data %>%
  filter(
    File.Date >= as.Date("2009-01-01") &
    File.Date <= as.Date("2012-12-31"))

evictions_2013_2016 <- eviction_data %>%
  filter(
    File.Date >= as.Date("2013-01-01") &
    File.Date <= as.Date("2016-12-31"))

evictions_2017_2019 <- eviction_data %>%
  filter(
    File.Date >= as.Date("2017-01-01") &
    File.Date <= as.Date("2019-12-31"))

evictions_2020_2023 <- eviction_data %>%
  filter(
    File.Date >= as.Date("2020-01-01") &
    File.Date <= as.Date("2023-12-31"))

evictions_2009_2023 <- eviction_data %>%
  filter(
    File.Date >= as.Date("2009-01-01") &
    File.Date <= as.Date("2023-12-31"))


elong_09_12 <- evictions_2009_2012 %>%
  pivot_longer(cols = c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                        "Owner.Move.In", "Demolition", "Capital.Improvement",
                        "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                        "Late.Payments", "Development"),
               names_to = "eviction_type",
               values_to = "eviction_count",
               values_transform = list(eviction_count = as.logical)) %>%
  filter(eviction_count == TRUE) %>%
  mutate(eviction_type = fct_relevel(eviction_type, "Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                                     "Owner.Move.In", "Demolition", "Capital.Improvement",
                                     "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                                     "Late.Payments","Development"))
elong_13_16 <- evictions_2013_2016 %>%
  pivot_longer(cols = c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                        "Owner.Move.In", "Demolition", "Capital.Improvement",
                        "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                        "Late.Payments", "Development"),
               names_to = "eviction_type",
               values_to = "eviction_count",
               values_transform = list(eviction_count = as.logical)) %>%
  filter(eviction_count == TRUE) %>%
  mutate(eviction_type = fct_relevel(eviction_type, "Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                                     "Owner.Move.In", "Demolition", "Capital.Improvement",
                                     "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                                     "Late.Payments", "Development"))

elong_17_19 <- evictions_2017_2019 %>%
  pivot_longer(cols = c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                        "Owner.Move.In", "Demolition", "Capital.Improvement",
                        "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                        "Late.Payments","Development"),
               names_to = "eviction_type",
               values_to = "eviction_count",
               values_transform = list(eviction_count = as.logical)) %>%
  filter(eviction_count == TRUE) %>%
  mutate(eviction_type = fct_relevel(eviction_type, "Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                                     "Owner.Move.In", "Demolition", "Capital.Improvement",
                                     "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                                     "Late.Payments", "Development"))

elong_20_23 <- evictions_2020_2023 %>%
  pivot_longer(cols = c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                        "Owner.Move.In", "Demolition", "Capital.Improvement",
                        "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                        "Late.Payments","Development"),
               names_to = "eviction_type",
               values_to = "eviction_count",
               values_transform = list(eviction_count = as.logical)) %>%
  filter(eviction_count == TRUE) %>%
  mutate(eviction_type = fct_relevel(eviction_type, "Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                                     "Owner.Move.In", "Demolition", "Capital.Improvement",
                                     "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                                     "Late.Payments", "Development"))

elong_09_23 <- evictions_2009_2023 %>%
  pivot_longer(cols = c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                        "Owner.Move.In", "Demolition", "Capital.Improvement",
                        "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                        "Late.Payments","Development"),
               names_to = "eviction_type",
               values_to = "eviction_count",
               values_transform = list(eviction_count = as.logical)) %>%
  filter(eviction_count == TRUE) %>%
  mutate(eviction_type = fct_relevel(eviction_type, "Non.Payment", "Breach", "Nuisance", "Illegal.Use",
                                     "Owner.Move.In", "Demolition", "Capital.Improvement",
                                     "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
                                     "Late.Payments", "Development"))

l_elongs <- list(elong_09_12, elong_13_16, elong_17_19, elong_20_23)

sum_l_elongs <- sum(sapply(l_elongs, function(x) nrow(x)))

calc_proportions <- function(data) {
  data %>%
    count(eviction_type) %>%
    mutate(prop = n / sum_l_elongs) %>%
    arrange(eviction_type)
}

# proportions of each eviction type in each time period all sum to 1
prop_e_longs_tp <- lapply(l_elongs, calc_proportions)

prop_e_longs_tp <- bind_rows(prop_e_longs_tp, .id = "data_id")

prop_e_longs_tp <- lapply(l_elongs, function(x) {
  x %>%
    count(eviction_type) %>%
    mutate(prop = n / sum_l_elongs) %>%
    arrange(eviction_type)
})

prop_e_longs_tp <- as_tibble(bind_rows(prop_e_longs_tp, .id = "data_id"))

# proportions of each eviction type overall all sum to 1
prop_e_longs_tot <- prop_e_longs_tp %>%
  group_by(eviction_type) %>%
  summarize(prop = sum(prop))

# proportions of each eviction type in each time period
# each time period sums to 1
prop_e_longs_tp_normp <- prop_e_longs_tp %>%
  group_by(data_id) %>%
  mutate(prop = prop / sum(prop))

# proportions of each eviction type in each time period
# each eviction type sums to 1
prop_e_longs_tp_normt <- prop_e_longs_tp%>%
  group_by(eviction_type) %>%
  mutate(prop = prop / sum(prop))

# confidence interval for one eviction type over the 4 periods,
# how far is the offset and is it significantlly different from 0
# resample whole data set

# for each eviction type, have R_t,i which is the number of evictions
# of type t in time period i
# N_i is number of evictions in period i
# p_t,i is the proportion of evictions of type t in period i
# induces that R_t,i is a binomial in the total number of evictions
# with the proportion p_t,i
# R_t,i ~ Binomial(n_t,i, p_t,i)

# testing how true that is by bootstrapping

# # row: eviction type, column: time period, value: offset from overall proportion
# bs_matrix <- array(NA, dim = c(12, 4, 1000))

size <- 10000

get_bs <- function(size, type, data) {
  bs_matrix <- array(NA, dim = c(4, size))

  for (i in 1:size) {
    print(i)
    print(type)
    bootstrap_indices <- sample(seq_len(nrow(data)), nrow(data), replace = TRUE)

    bootstrap_sample <- data[bootstrap_indices, ]

    bs <- bootstrap_sample %>%
      filter(eviction_type == type)
  
    e_09_12_bs <- bs[bs$File.Date >= as.Date("2009-01-01") &
      bs$File.Date <= as.Date("2012-12-31"), ]
    e_13_16_bs <- bs[bs$File.Date >= as.Date("2013-01-01") &
      bs$File.Date <= as.Date("2016-12-31"), ]
    e_17_19_bs <- bs[bs$File.Date >= as.Date("2017-01-01") &
      bs$File.Date <= as.Date("2019-12-31"), ]
    e_20_23_bs <- bs[bs$File.Date >= as.Date("2020-01-01") &
      bs$File.Date <= as.Date("2023-12-31"), ]

    # proportion where each eviction type sums to 1
    prop_e_09_12_bs <- nrow(e_09_12_bs) / nrow(bs)
    prop_e_13_16_bs <- nrow(e_13_16_bs) / nrow(bs)
    prop_e_17_19_bs <- nrow(e_17_19_bs) / nrow(bs)
    prop_e_20_23_bs <- nrow(e_20_23_bs) / nrow(bs)

    bs_matrix[1, i] <- prop_e_09_12_bs
    bs_matrix[2, i] <- prop_e_13_16_bs
    bs_matrix[3, i] <- prop_e_17_19_bs
    bs_matrix[4, i] <- prop_e_20_23_bs
  }
  return(bs_matrix)
}

# eviction types but in alphabetical order
eviction_type <- c("Breach", "Capital.Improvement", "Demolition", "Development",
                   "Ellis.Act.WithDrawal", "Illegal.Use", "Late.Payments",
                   "Non.Payment", "Nuisance", "Other.Cause", "Owner.Move.In",
                   "Roommate.Same.Unit")
bs_matrices <- list()
for (i in 1:12) {
  bs_matrices[[i]] <- get_bs(size, eviction_type[i], elong_09_23)
}

# # bs_matrices currently in order eviction_type <- c("Non.Payment", "Breach", "Nuisance", "Illegal.Use",
#                     "Owner.Move.In", "Demolition", "Capital.Improvement",
#                     "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause",
#                     "Late.Payments", "Development")
# # reorder bs_matrices to be in alphabetical order
bs_matrices_temp <- bs_matrices[c(2, 7, 6, 12, 8, 4, 11, 1, 3, 10, 5, 9)]

# calculate 95% confidence interval for each eviction type
ci_bs_matrices <- list()
for (i in 1:12) {
  ci_bs_matrices[[i]] <- array(NA, dim = c(4, 2))
  for (j in 1:4) {
    ci_bs_matrices[[i]][j, 1] <- quantile(bs_matrices[[i]][j, ], 0.005)
    ci_bs_matrices[[i]][j, 2] <- quantile(bs_matrices[[i]][j, ], 0.995)
  }
}

type_labels <- c("NP", "Br", "Nu", "IU", "OMI", "Dm", "CI", "EAW", "RSU", "OC", "LP", "Dv")
type_mapping <- setNames(type_labels, eviction_type)

# Calculate signed z-scores for each confidence interval
signed_z_scores <- lapply(1:12, function(i) {
  mean_prop <- get_mean_prop(eviction_type[i])
  ci_low <- ci_bs_matrices[[i]][, 1]
  ci_high <- ci_bs_matrices[[i]][, 2]
  z_scores <- numeric(4)
  for (j in 1:4) {
    if (mean_prop > ci_high[j]) {
      z_scores[j] <- (ci_high[j] - mean_prop) / sd(bs_matrices[[i]][j, ])
    } else if (mean_prop < ci_low[j]) {
      z_scores[j] <- (ci_low[j] - mean_prop) / sd(bs_matrices[[i]][j, ])
    } else {
      z_scores[j] <- 0
    }
  }
  return(z_scores)
})

# Flatten the signed z-scores into a data frame
signed_z_scores_df <- data.frame(
  eviction_type = rep(eviction_type, each = 4),
  time_period = rep(c("09-12", "13-16", "17-19", "20-23"), 12),
  signed_z_score = unlist(signed_z_scores)
)


get_mean_prop <- function(type) {
  return(sum(prop_e_longs_tp_normt %>%
           filter(eviction_type == type) %>%
           pull(prop)) / 4)
}

# list of ggplots for each eviction type
ggplots <- list()

for (i in 1:12) {
  type <- eviction_type[i]
  prop_e_longs <- prop_e_longs_tp_normt %>%
    filter(eviction_type == type)


  ci_df <- data.frame(
    data_id = prop_e_longs$data_id,
    prop = prop_e_longs$prop,
    ymin = ci_bs_matrices[[i]][, 1],
    ymax = ci_bs_matrices[[i]][, 2]
  )

  # mean proportion of each kind of eviction type based on what type is now
  mean_prop <- get_mean_prop(type)

  ggobj <- ggplot(prop_e_longs, aes(x = data_id, y = prop)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = mean_prop, color = "red") +
    geom_errorbar(data = ci_df, aes(x = as.numeric(data_id),
                                    ymin = ymin, ymax = ymax),
                  width = 0.3, color = "blue") +
    labs(x = "Time Period", y = "Proportion",
         title = paste(type)) +
    scale_x_discrete(labels = c("09-12", "13-16", "17-19", "20-23")) +
    scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF", "#7CAE00"),
                      labels = c("09-12", "13-16", "17-19", "20-23"))

  ggplots[[i]] <- ggobj
}
map_page <- ggarrange(plotlist = ggplots, ncol = 3, nrow = 4)

annotate_figure(map_page,
                top = text_grob(paste0("Counts by type, proportional"), size = 14),
                bottom = text_grob(" ", size = 2, face = "italic"))
ggsave(filename = paste0("test.jpg"), dpi = 500)


# proportion using MLE estimate that proportion is p_t,i = R_t,i / N_i
prop_overall <- sum(prop_e_longs_tp %>% pull(prop)) / 48


# Then, I modelled the proportions of each eviction type, finding their mean proportions. I added confidence intervals based on the bootstrapped samples of evictions from 2009 - 2023. Noticing that there is not a lot of consistency, in each time periods

# Further quantify what I am seeing by transforming the plot to find how many standard deviations in this confidence interval I am to the mean, like a z score

# goal is 2 sets of plots, one set organizes z scores by time period, the other by eviction type

# Plot signed z-scores organized by time period
ggplot(signed_z_scores_df, aes(x = time_period, y = signed_z_score, fill = eviction_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Time Period", y = "Signed Z-Score", title = "Signed Z-Scores of Confidence Intervals by Time Period") +
  scale_fill_manual(values = c("#E63946", "#F1BEAC", "#A8DADC", "#457B9D", "#1D3557", "#FCA311", "#E9C46A", "#2A9D8F", "#264653", "#FF9F1C", "#06D6A0", "#FF6B6B"), breaks = c("Non.Payment", "Breach", "Nuisance", "Illegal.Use", "Owner.Move.In", "Demolition", "Capital.Improvement", "Ellis.Act.WithDrawal", "Roommate.Same.Unit", "Other.Cause", "Late.Payments", "Development")) +
  theme_minimal() +
  scale_x_discrete(labels = c("2009-2012", "2013-2016", "2017-2019", "2020-2023"))




# Plot signed z-scores organized by eviction type
ggplot(signed_z_scores_df, aes(x = eviction_type, y = signed_z_score, fill = time_period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Eviction Type", y = "Signed Z-Score", title = "Signed Z-Scores of Confidence Intervals by Eviction Type") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF", "#7CAE00")) +
  theme_minimal() +
  scale_x_discrete(labels = type_mapping)



# # calculate 95% confidence interval for each eviction type
# ci_bs_matrix <- array(NA, dim = c(4, 2))
# for (i in 1:4) {
#   ci_bs_matrix[i, 1] <- quantile(bs_matrix[i, ], 0.025)
#   ci_bs_matrix[i, 2] <- quantile(bs_matrix[i, ], 0.975)
# }

# ci_df <- data.frame(
#   data_id = prop_e_longs_np$data_id,
#   prop = prop_e_longs_np$prop,
#   ymin = ci_bs_matrix[, 1],
#   ymax = ci_bs_matrix[, 2]
# )

# # plot histogram
# ggplot(prop_e_longs_np, aes(x = data_id, y = prop)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     geom_segment(aes(x = as.numeric(data_id) - 0.5,
#                      xend = as.numeric(data_id) + 0.5,
#                      y = mean_prop_np, yend = mean_prop_np),
#                  color = "red") +
#     geom_errorbar(data = ci_df, aes(x = as.numeric(data_id),
#                                     ymin = ci_df[, 3], ymax = ci_df[, 4]),
#                   width = 0.3, color = "blue") +  # Adjust width and color as needed
#     labs(x = "Time Period", y = "Proportion", 
#          title = "Non.Payment Eviction Counts by Time Period (Proportional)") +
#     scale_x_discrete(labels = c("2009-2012", "2013-2016",
#                                 "2017-2019", "2020-2023")) +
#     scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF", "#7CAE00"),
#                       labels = c("2009-2012", "2013-2016",
#                                  "2017-2019", "2020-2023"))

# # proportion of each eviction type in each time period
# prop_types_tp <- matrix(NA, nrow = 12, ncol = 4)
# prop_types_tp[, 1] <- table(elong_09_12$eviction_type) / nrow(elong_09_12)
# prop_types_tp[, 2] <- table(elong_13_16$eviction_type) / nrow(elong_13_16)
# prop_types_tp[, 3] <- table(elong_17_19$eviction_type) / nrow(elong_17_19)
# prop_types_tp[, 4] <- table(elong_20_23$eviction_type) / nrow(elong_20_23)

# # proportion of eviction type overall
# prop_types_tot <- table(elong_09_23$eviction_type) / nrow(elong_09_23)

# # bootstrap
# for (i in 1:1000) {
#   bootstrap_indices <- sample.int(nrow(elong_09_23), nrow(elong_09_23), replace = TRUE)

#   bootstrap_sample <- elong_09_23[bootstrap_indices, ]

#   e_09_12_bs <- bootstrap_sample[bootstrap_sample$File.Date >= as.Date("2009-01-01") &
#   bootstrap_sample$File.Date <= as.Date("2012-12-31"), ]
#   e_13_16_bs <- bootstrap_sample[bootstrap_sample$File.Date >= as.Date("2013-01-01") &
#   bootstrap_sample$File.Date <= as.Date("2016-12-31"), ]
#   e_17_19_bs <- bootstrap_sample[bootstrap_sample$File.Date >= as.Date("2017-01-01") &
#   bootstrap_sample$File.Date <= as.Date("2019-12-31"), ]
#   e_20_23_bs <- bootstrap_sample[bootstrap_sample$File.Date >= as.Date("2020-01-01") &
#     bootstrap_sample$File.Date <= as.Date("2023-12-31"), ]

#   # proportion of each eviction type in each time period

#   prop_types_tp_bs <- matrix(NA, nrow = 12, ncol = 4)
#   prop_types_tp_bs[, 1] <- table(e_09_12_bs$eviction_type) / nrow(e_09_12_bs)
#   prop_types_tp_bs[, 2] <- table(e_13_16_bs$eviction_type) / nrow(e_13_16_bs)
#   prop_types_tp_bs[, 3] <- table(e_17_19_bs$eviction_type) / nrow(e_17_19_bs)
#   prop_types_tp_bs[, 4] <- table(e_20_23_bs$eviction_type) / nrow(e_20_23_bs)
#   # proportion of eviction type overall

#   prop_types_tot_bs <- table(bootstrap_sample$eviction_type) / nrow(bootstrap_sample)
#   # offset from overall proportion

#   bs_matrix[, i] <- prop_types_tot_bs
# }

# # calculate 95% confidence interval for each eviction type
# ci_bs_matrix <- array(NA, dim = c(12, 2))
# for (i in 1:12) {
#   ci_bs_matrix[i, 1] <- quantile(bs_matrix[i, ], 0.025)
#   ci_bs_matrix[i, 2] <- quantile(bs_matrix[i, ], 0.975)
# }

# # Create CI data frame
# ci_df <- data.frame(
#   eviction_type = prop_e_longs_tot$eviction_type,
#   prop = prop_e_longs_tot$prop,
#   ymin = ci_bs_matrix[, 1],
#   ymax = ci_bs_matrix[, 2]
# )

# ggplot(prop_e_longs_tot, aes(x = eviction_type, y = prop)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_segment(aes(x = as.numeric(eviction_type) - 0.4, 
#                    xend = as.numeric(eviction_type) + 0.4,
#                    y = prop_types_tot, yend = prop_types_tot),
#                color = "red") +
#   geom_errorbar(data = ci_df, aes(x = as.numeric(eviction_type),
#                                  ymin = ymin, ymax = ymax),
#                 width = 0.3, color = "blue") +  # Adjust width and color as needed
#   labs(x = "Eviction Type", y = "Proportion", 
#        title = "Eviction Counts by Type (Proportional)") +
#   scale_x_discrete(labels = c(
#     "NP", "Br", "Nu", "IU", "OMI", "Dm", "CI",
#     "EAW", "RSU", "OC", "LP", "Dv")) +
#   scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF", "#7CAE00"),
#                     labels = c("2009-2012", "2013-2016",
#                                "2017-2019", "2020-2023"))




# # find actual offsets
# offsets <- array(NA, dim = c(12, 4))
# offsets[, 1] <- prop_types_tp[, 1] - prop_types_tot
# offsets[, 2] <- prop_types_tp[, 2] - prop_types_tot
# offsets[, 3] <- prop_types_tp[, 3] - prop_types_tot
# offsets[, 4] <- prop_types_tp[, 4] - prop_types_tot

# # calculate 95% confidence interval for each eviction type

# ci_bs_matrix <- array(NA, dim = c(12, 4, 2))
# for (i in 1:12) {
#   for (j in 1:4) {
#     ci_bs_matrix[i, j, 1] <- quantile(bs_matrix[i, j, ], 0.025)
#     ci_bs_matrix[i, j, 2] <- quantile(bs_matrix[i, j, ], 0.975)
#   }
# }
# # find if bootstrap offsets are within 95% confidence interval

# offsets_in_ci <- (offsets >= ci_bs_matrix[, , 1]) & (offsets <= ci_bs_matrix[, , 2])





# # Plot histogram and add in proportion using MLE estimate that proportion is p_t,i = R_t,i / N_i
# ggplot(prop_e_longs_tp, aes(x = eviction_type, y = prop, fill = data_id)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_hline(yintercept = prop_overall, color = "red") +
#   labs(x = "Eviction Type", y = "Proportion", 
#        title = "Eviction Counts by Type (Proportional)") +
#   scale_x_discrete(labels = c(
#     "NP", "Br", "Nu", "IU", "OMI", "Dm", "CI",
#     "EAW", "RSU", "OC", "LP", "Dv")) +
#   scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF", "#7CAE00"),
#                     labels = c("2009-2012", "2013-2016",
#                     "2017-2019", "2020-2023"))

# find mean evictions per month overall
mean_evictions_per_month <- mean(counts_by_month_year$count)


lm <- lm(count ~ month, data = counts_by_month_year)

ggplot(counts_by_month_year, aes(x = month, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Month", y = "Eviction Count", title = "Eviction Counts by Month and Year") +
  scale_x_continuous(breaks = seq(1, 312, 12),
                     labels = c(1997:2023))

# same plot as above but with loess
ggplot(counts_by_month_year, aes(x = month, y = count)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Month", y = "Eviction Count", title = "Eviction Counts by Month and Year") +
  scale_x_continuous(breaks = seq(1, 312, 12),
                     labels = c(1997:2023))

# add different loess smoothness, do 3
ggplot(counts_by_month_year, aes(x = month, y = count)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  labs(x = "Month", y = "Eviction Count", title = "Eviction Counts by Month and Year") +
  scale_x_continuous(breaks = seq(1, 324, 12),
                     labels = c(1997:2023))


# create sf object from evictionData
# eviction_sf <- st_as_sf(data_frame, coords = c("longitude", "latitude"),
#                         crs = 4326)


# set the projection of the eviction data to match the map
# eviction_sf <- st_set_crs(eviction_sf, st_crs(sf_map$osm_lines))

# # plot overall eviction notices
# ggplot() +
#   geom_sf(data = sf_map$osm_lines, color = "gray", size = 0.5) +
#   geom_sf(data = eviction_sf, color = "red", size = 0.2) +
#   coord_sf(xlim = c(-122.5247, -122.3366), ylim = c(37.6983, 37.8312)) +
#   theme_void() +
#   theme(legend.position = "none") +
#   labs(title = "Eviction Notices in San Francisco (1997-2023)",
#        subtitle = "Data Source: SF Rent Arbitration Board",
#        caption = "Source: https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5")


# # order eviction data by years
# eviction_data_array <- list()
# for (i in 1:27) {
#   eviction_data_array[[i]] <- eviction_data %>%
#     filter(File.Date >= as.Date(paste0(i + 1996, "-01-01")) &
#              File.Date <= as.Date(paste0(i + 1996, "-12-31")))
# }

# # remove rows that have lat long as NA
# for (i in 1:27) {
#   eviction_data_array[[i]] <- eviction_data_array[[i]] %>%
#     filter(!is.na(longitude) & !is.na(latitude))
# }

# # create sf object from eviction_data_array
# for (i in 1:27) {
#   eviction_data_array[[i]] <-
#     st_as_sf(eviction_data_array[[i]],
#     coords = c("longitude", "latitude"),
#     crs = 4326)
# }

# # set the projection of the eviction data to match the map
# for (i in 1:27) {
#   eviction_data_array[[i]] <-
#     st_set_crs(eviction_data_array[[i]], st_crs(sf_map$osm_lines))
# }


# # plot eviction notices by year
# for (i in 1:27) {
#   ggplot() +
#     geom_sf(data = sf_map$osm_lines, color = "#9BA4B5", size = 0.2) +
#     geom_sf(data = eviction_data_array[[i]], color = "#212A3E", size = 0.5) +
#     coord_sf(xlim = c(-122.5247, -122.3366), ylim = c(37.6983, 37.8312)) +
#     theme(plot.background = element_rect(fill = "#F1F6F9")) +
#     theme(legend.position = "none") +
#     labs(title = paste0("Eviction Notices in San Francisco (", i + 1996, ")"),
#          subtitle = "Data Source: SF Rent Arbitration Board",
#          caption = "Source: https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5")
# ggsave(paste0("/Users/brianliu03/Documents/Stats2023/data-project/eviction_notices_by_year/", i + 1996, ".png"))
# }

# # plot eviction notices over time, with y=evictions and x=time
# ggplot(data_frame, aes(x = date)) +
#   geom_histogram(binwidth = 365, color = "#212A3E", fill = "#9BA4B5") +
#   theme(plot.background = element_rect(fill = "#F1F6F9")) +
#   theme(legend.position = "none") +
#   labs(title = "Eviction Notices in San Francisco (1997-2023)",
#        subtitle = "Data Source: SF Rent Arbitration Board",
#        caption = "Source: https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5",
#        x = "Year",
#        y = "Eviction Notices"
#          )

# # filter eviction_data by Owner Move In, where field is Owner.Move.In and true or false
# # then filter by Ellis Act Withdrawal
# omi <- eviction_data[which(eviction_data$Owner.Move.In == "true"), ]
# omi_dates <- tibble(omi$File.Date)
# eaw <- eviction_data[which(eviction_data$Ellis.Act.WithDrawal == "true"), ]
# eaw_dates <- tibble(eaw$File.Date)
# nui <- eviction_data[which(eviction_data$Nuisance == "true"), ]
# nui_dates <- tibble(nui$File.Date)

# # create a tibble with all dates from Jan 1, 1997 to Dec 31, 2023
# all_dates <- tibble(File.Date = seq(as.Date("1997-01-01"), as.Date("2023-12-31"), by = "day"))

# # summarize omi and eaw evictions by date
# omi_dates <- omi %>% group_by(File.Date) %>% summarize(n_omi = n())
# eaw_dates <- eaw %>% group_by(File.Date) %>% summarize(n_eaw = n())

# nui_dates <- nui %>% group_by(File.Date) %>% summarize(n_nui = n())

# omi_dates$File.Date <- as.Date(omi_dates$File.Date, format = "%m/%d/%Y")
# eaw_dates$File.Date <- as.Date(eaw_dates$File.Date, format = "%m/%d/%Y")
# nui_dates$File.Date <- as.Date(nui_dates$File.Date, format = "%m/%d/%Y")

# merged_dates <- left_join(data.frame(date = dates), omi_dates, by = c("date" = "File.Date")) %>% 
#   left_join(eaw_dates, by = c("date" = "File.Date")) %>%
#   left_join(nui_dates, by = c("date" = "File.Date"))
# merged_dates <- replace_na(merged_dates, list(n_omi = 0, n_eaw = 0, n_nui = 0))


# # > head(merged_dates)
# #         date n_omi n_eaw
# # 1 1997-01-01    NA    NA
# # 2 1997-01-02    20    NA
# # 3 1997-01-03    16    NA
# # 4 1997-01-04    NA    NA
# # 5 1997-01-05    NA    NA
# # 6 1997-01-06    24    NA
# # plot omi and eaw evictions over time
# ggplot(merged_dates, aes(x = date)) +
#   geom_line(aes(y = n_omi, color = "Owner Move In")) +
#   geom_line(aes(y = n_eaw, color = "Ellis Act Withdrawal")) +
#   geom_line(aes(y = n_nui, color = "Nuisance")) +
#   scale_color_manual(name = "Eviction Type", values = c("Owner Move In" = "#212A3E", "Ellis Act Withdrawal" = "#9BA4B5")) +
#   theme(plot.background = element_rect(fill = "#F1F6F9")) +
#   theme(legend.position = "none") +
#   labs(title = "Eviction Notices in San Francisco (1997-2023)",
#        subtitle = "Data Source: SF Rent Arbitration Board",
#        caption = "Source: https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5",
#        x = "Year",
#        y = "Eviction Notices"
#   )


# merged_dates_by_year <- merged_dates %>% group_by(year = year(date)) %>% summarize(n_omi = sum(n_omi), n_eaw = sum(n_eaw), n_nui = sum(n_nui))

# # plot omi and eaw evictions over time
# ggplot(merged_dates_by_year, aes(x = year)) +
#   geom_line(aes(y = n_omi, color = "Owner Move In")) +
#   geom_line(aes(y = n_eaw, color = "Ellis Act Withdrawal")) +
#   scale_color_manual(name = "Eviction Type", values = c("Owner Move In" = "#212A3E", "Ellis Act Withdrawal" = "#9BA4B5")) +
#   theme(plot.background = element_rect(fill = "#F1F6F9")) +
#   theme(legend.position = "none") +
#   labs(title = "Eviction Notices in San Francisco (1997-2023)",
#        subtitle = "Data Source: SF Rent Arbitration Board",
#        caption = "Source: https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5",
#        x = "Year",
#        y = "Eviction Notices"
#   )


# kde2d

# bootstrap, CLT

# png


# ggplot(merged_dates_by_year, aes(x = year)) +
#   geom_line(aes(y = n_omi, color = "Owner Move In")) +
#   geom_line(aes(y = n_eaw, color = "Ellis Act Withdrawal")) +
#   geom_line(aes(y = n_nui, color = "Nuisance")) + 
#   scale_color_manual(name = "Eviction Type", values = c("Owner Move In" = "#334257", "Ellis Act Withdrawal" = "#7882A4", "Nuisance" = "#548CA8")) +
#   theme(plot.background = element_rect(fill = "#F1F6F9")) +
#   theme(legend.position = "none") +
#   labs(title = "Eviction Notices in San Francisco (1997-2023)",
#        subtitle = "Data Source: SF Rent Arbitration Board",
#        caption = "Source: https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5",
#        x = "Year",
#        y = "Eviction Notices"
#  )