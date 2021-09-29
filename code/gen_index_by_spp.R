library(mgcv)
library(rerddap)
library(dplyr)
library(lubridate)
library(sf)
library(tidyr)
library(purrr)

# settings
source("code/set_control_params.R")

# load species tot cpue
tot_cpue <- readRDS("indices/tot_cpue_species.rds")
tot_cpue <- dplyr::arrange(tot_cpue,-tot_cpue)
tot_cpue <- tot_cpue[1:top_species,]

# load prediction grid
pred_grid <- readRDS("indices/pred_grid.rds")
pred_grid$season <- 2

# model function - edit to change form of model
gam_fit <- function(df) {
  gam(larvae_10m2 ~ as.factor(year) + s(latitude, longitude, by = year),
      data = df,
      family = tw()
  )
}

# grab data for all species
unique_files <- unique(tot_cpue$erddap)
for (i in 1:length(unique_files)) {
  out <- info(as.character(unique_files[i]))
  # station_dat <- tabledap(out, fields = c(
  #   "station", "line", "latitude",
  #   "longitude", "time", "scientific_name", "larvae_10m2"
  # ))
  station_dat <- tabledap(out,
    fields = c(
      "larvae_10m2", "latitude", "longitude",
      "station", "scientific_name", "time"
    )
  )
  # filter out species in question
  species <- dplyr::filter(tot_cpue, erddap==unique_files[i])
  dat <- dplyr::filter(station_dat,
        scientific_name %in% species$scientific_name)

  # convert date
  dat <- as.data.frame(dat)
  dat$date <- lubridate::as_date(dat$time)
  dat$year <- lubridate::year(dat$date)
  dat$month <- lubridate::month(dat$date)
  dat$yday <- lubridate::yday(dat$date)

  # filter out recent years with consistent sampling
  dat <- dplyr::filter(dat, year >= min_year)

  # # time windows https://calcofi.org/cruises.html
  dat$season <- NA
  dat$season[which(dat$yday %in% 2:52)] <- 1
  dat$season[which(dat$yday %in% 90:141)] <- 2
  dat$season[which(dat$yday %in% 181:233)] <- 3
  dat$season[which(dat$yday %in% 273:325)] <- 4
  dat <- dplyr::filter(dat, !is.na(season))
  dat$season <- as.factor(dat$season)
  dat <- dplyr::filter(dat, season==2)

  # convert to UTM - kms
  # make the UTM cols spatial (X/Easting/lon, Y/Northing/lat)
  dat$latitude <- as.numeric(dat$latitude)
  dat$longitude <- as.numeric(dat$longitude)
  dat <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)
  dat<-st_transform(x = dat, crs = 32610)
  dat$longitude = st_coordinates(dat)[,1]
  dat$latitude = st_coordinates(dat)[,2]
  dat <- as.data.frame(dat)
  dat$longitude <- dat$longitude / 1000 # to kms
  dat$latitude <- dat$latitude / 1000 # to kms

  # format response
  dat$larvae_10m2 <- as.numeric(dat$larvae_10m2)
  dat$larvae_10m2[which(is.na(dat$larvae_10m2))] <- 0

  # remove species with 0 records
  dat = dplyr::group_by(dat, scientific_name) %>%
    dplyr::mutate(tot = sum(larvae_10m2)) %>%
    dplyr::filter(tot>0) %>%
    dplyr::select(-tot)

  # expand predicted grid to have separate rows for each spp
  new_grid <- expand.grid("scientific_name"=unique(dat$scientific_name),
              "station"=unique(pred_grid$station))
  new_grid <- dplyr::left_join(new_grid, pred_grid) %>%
    dplyr::rename(species = scientific_name) %>%
    dplyr::filter(year >= min(dat$year), year <= max(dat$year))

  # nest fitted and predicted data
  dat_nested <-
    dat %>%
    dplyr::rename(species = scientific_name) %>%
    nest(-species) %>%
    rename(myorigdata = data)

  # create second dataset - predictions
  pred_nested <-
    new_grid %>%
    nest(-species) %>%
    rename(mynewdata = data)

  predictions_all <-
    dat_nested %>%
    mutate(my_model = map(myorigdata, gam_fit)) %>%
    full_join(pred_nested, by = "species") %>%
    mutate(my_new_pred = map2(my_model, mynewdata, predict)) %>%
    select(species, mynewdata, my_new_pred) %>%
    unnest(mynewdata, my_new_pred) %>%
    dplyr::group_by(species, year) %>%
    dplyr::summarise(index = log(sum(exp(my_new_pred)))) %>%
    as.data.frame()

  # also calculate summaries from data
  summaries <- dplyr::rename(dat, species = scientific_name) %>%
    dplyr::group_by(species, year) %>%
    dplyr::summarize(
      mean_cpue = mean(larvae_10m2),
      n_pos_cpue = length(which(larvae_10m2 > 0))
    )
  predictions_all <- dplyr::left_join(predictions_all, summaries)

  if (i == 1) {
    all_pred <- predictions_all
  } else {
    all_pred <- rbind(all_pred, predictions_all)
  }
}

saveRDS(all_pred, "indices/predicted_indices.rds")

# Filter out experimental stations
# https://calcofi.org/field-work/station-positions.html
# stations <- read.csv("data/CalCOFIStationOrder.csv")
# stations <- dplyr::rename(stations, station = Station)
# dat <- dplyr::left_join(dat, stations[, c("station", "StaType")])
# dat <- dplyr::filter(dat, StaType == "ROS")

