library(tidyverse)

d <- read.csv("data/CPS001-1985---2024.csv")

d <- dplyr::rename(d, year = LANDING_YEAR,
                   species = PACFIN_SPECIES_COMMON_NAME)

d$FIRST_QTR_LANDED_WEIGHT_MTONS[which(is.na(d$FIRST_QTR_LANDED_WEIGHT_MTONS))] <- 0
d$SECOND_QTR_LANDED_WEIGHT_MTONS[which(is.na(d$SECOND_QTR_LANDED_WEIGHT_MTONS))] <- 0
d$THIRD_QTR_LANDED_WEIGHT_MTONS[which(is.na(d$THIRD_QTR_LANDED_WEIGHT_MTONS))] <- 0
d$FOURTH_QTR_LANDED_WEIGHT_MTONS[which(is.na(d$FOURTH_QTR_LANDED_WEIGHT_MTONS))] <- 0

d$weight <- d$FIRST_QTR_LANDED_WEIGHT_MTONS + 
  d$SECOND_QTR_LANDED_WEIGHT_MTONS + 
  d$THIRD_QTR_LANDED_WEIGHT_MTONS + 
  d$FOURTH_QTR_LANDED_WEIGHT_MTONS

d$FIRST_QTR_EXVESSEL_REVENUE[which(is.na(d$FIRST_QTR_EXVESSEL_REVENUE))] <- 0
d$SECOND_QTR_EXVESSEL_REVENUE[which(is.na(d$SECOND_QTR_EXVESSEL_REVENUE))] <- 0
d$THIRD_QTR_EXVESSEL_REVENUE[which(is.na(d$THIRD_QTR_EXVESSEL_REVENUE))] <- 0
d$FOURTH_QTR_EXVESSEL_REVENUE[which(is.na(d$FOURTH_QTR_EXVESSEL_REVENUE))] <- 0

d$revenue <- d$FIRST_QTR_EXVESSEL_REVENUE + 
  d$SECOND_QTR_EXVESSEL_REVENUE + 
  d$THIRD_QTR_EXVESSEL_REVENUE + 
  d$FOURTH_QTR_EXVESSEL_REVENUE

# aggregate by year
d_agg <- dplyr::group_by(d, year, species) |>
  dplyr::summarise(
    tot_weight = sum(weight, na.rm = TRUE),
    tot_revenue = sum(revenue, na.rm = TRUE)
  ) 

library(ggplot2)

ggplot(d_agg, aes(x = year, y = tot_revenue)) +
  geom_rect(aes(xmin = 2013, xmax = 2024, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.01) +
  geom_line() +
  labs(
    title = "PFMC revenue by species",
    x = "Year",
    y = "Revenue (in USD)"
  ) +
  theme_minimal() + 
  facet_wrap(~ species, scale="free_y")
ggsave("figures/pfmc_revenue_by_species.png", width = 10, height = 6)

ggplot(d_agg, aes(x = year, y = tot_weight)) +
  geom_rect(aes(xmin = 2013, xmax = 2024, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.01) +
  geom_line() +
  labs(
    title = "PFMC weight by species",
    x = "Year",
    y = "Weight (in MT)"
  ) +
  theme_minimal() + 
  facet_wrap(~ species, scale="free_y")
ggsave("figures/pfmc_revenue_by_species.png", width = 10, height = 6)





d <- read.csv("data/GMT001-1985---2025.csv")
d <- dplyr::rename(d, year = LANDING_YEAR,
                   species = COMMON_NAME) |>
  dplyr::select(year, species, TOTAL_LANDED_WEIGHT_MTONS,
                TOTAL_EXVESSEL_REVENUE)

# filter out things with at least 25 years of data
d <- dplyr::group_by(d, species) |>
  dplyr::filter(dplyr::n() >= 25) |>
  dplyr::ungroup()

dplyr::filter(d, species %in% c("__ALL FLATFISH",
                                "__ALL GROUNDFISH",
                                "__ALL ROCKFISH",
                                "__ALL ROUNDFISH")) |>
  ggplot(aes(year,TOTAL_LANDED_WEIGHT_MTONS, group = species)) + 
  geom_rect(aes(xmin = 2013, xmax = 2024, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.01) + 
  geom_line() + facet_wrap(~ species, scales = "free_y") + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Landed weight (mt)")
ggsave("figures/pfmc_gfish_wt_aggregates.png", width = 10, height = 6)


spec_names <- unique(d$species)
spec_names <- spec_names[-grep("UNSP", spec_names)]
spec_names <- spec_names[-grep("__", spec_names)]
spec_names <- spec_names[-grep("NOM.", spec_names)]

d <- dplyr::filter(d, species %in% spec_names) 

dplyr::group_by(d, species) |>
  dplyr::mutate(sum_w = sum(TOTAL_LANDED_WEIGHT_MTONS)) |>
  dplyr::filter(sum_w > 10000) |>
  ungroup() |>
  ggplot(aes(year,TOTAL_LANDED_WEIGHT_MTONS, group = species)) + 
  geom_rect(aes(xmin = 2013, xmax = 2024, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.01) + 
  geom_line() + facet_wrap(~ species, scales = "free_y") + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Landed weight (mt)")
ggsave("figures/pfmc_gfish_wt_species.png", width = 10, height = 6)

