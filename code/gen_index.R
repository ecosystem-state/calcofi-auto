library(mgcv)
library(rerddap)
library(dplyr)
library(lubridate)
library(sp)
library(tidyr)
library(purrr)
# settings
top_species <- 50
pred_resolution <- 5 # resolution of prediction grid, km
#
# list of all datasets
calcofi_erddap <- c(
  "erdCalCOFIlrvcntAtoAM",
  "erdCalCOFIlrvcntANtoAR",
  "erdCalCOFIlrvcntAStoBA",
  "erdCalCOFIlrvcntBCEtoBZ",
  "erdCalCOFIlrvcntCtoCE",
  "erdCalCOFIlrvcntCDtoCH",
  "erdCalCOFIlrvcntCItoCO",
  "erdCalCOFIlrvcntCPtoDE",
  "erdCalCOFIlrvcntDHtoEC",
  "erdCalCOFIlrvcntEDtoEU",
  "erdCalCOFIlrvcntEVtoGN",
  "erdCalCOFIlrvcntGOtoHA",
  "erdCalCOFIlrvcntHBtoHI",
  "erdCalCOFIlrvcntHJtoID",
  "erdCalCOFIlrvcntIEtoLA",
  "erdCalCOFIlrvcntLBtoLI",
  "erdCalCOFIlrvcntLJtoMA",
  "erdCalCOFIlrvcntMBtoMO",
  "erdCalCOFIlrvcntMPtoNA",
  "erdCalCOFIlrvcntNBtoOL",
  "erdCalCOFIlrvcntOMtoOX",
  "erdCalCOFIlrvcntOYtoPI",
  "erdCalCOFIlrvcntPLtoPO",
  "erdCalCOFIlrvcntPPtoPZ",
  "erdCalCOFIlrvcntQtoSA",
  "erdCalCOFIlrvcntSBtoSC",
  "erdCalCOFIlrvcntSDtoSI",
  "erdCalCOFIlrvcntSJtoST",
  "erdCalCOFIlrvcntSUtoTE",
  "erdCalCOFIlrvcntTFtoU",
  "erdCalCOFIlrvcntVtoZ"
)

# grab data for all species
for (i in 1:length(calcofi_erddap)) {
  out <- info(as.character(calcofi_erddap[i]))
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

  if (i == 1) {
    dat <- station_dat
  } else {
    dat <- rbind(dat, station_dat)
  }
}

# dataset is very large; > 70 million rows
dat <- as.data.frame(dat)
dat$date <- lubridate::as_date(dat$time)
dat$year <- lubridate::year(dat$date)
dat$month <- lubridate::month(dat$date)
dat$yday <- lubridate::yday(dat$date)

# filter out recent years with consistent sampling
dat <- dplyr::filter(dat, year >= 1985)

# # time windows https://calcofi.org/cruises.html
dat$season <- NA
dat$season[which(dat$yday %in% 2:52)] <- 1
dat$season[which(dat$yday %in% 90:141)] <- 2
dat$season[which(dat$yday %in% 181:233)] <- 3
dat$season[which(dat$yday %in% 273:325)] <- 4
dat <- dplyr::filter(dat, !is.na(season))
dat$season <- as.factor(dat$season)

# Filter out experimental stations
# https://calcofi.org/field-work/station-positions.html
stations <- read.csv("data/CalCOFIStationOrder.csv")
stations <- dplyr::rename(stations, station = Station)
dat <- dplyr::left_join(dat, stations[, c("station", "StaType")])
dat <- dplyr::filter(dat, StaType == "ROS")

dat$latitude <- as.numeric(dat$latitude)
dat$longitude <- as.numeric(dat$longitude)

# convert to UTM - kms
sp::coordinates(dat) <- c("longitude", "latitude")
sp::proj4string(dat) <- sp::CRS("+proj=longlat + ellps=WGS84 +datum=WGS84")
dat <- try(sp::spTransform(dat, CRS = "+proj=utm +zone=10 +datum=WGS84"),
  silent = TRUE
)
dat <- as.data.frame(dat)
dat$longitude <- dat$longitude / 1000
dat$latitude <- dat$latitude / 1000

# format response
dat$larvae_10m2 <- as.numeric(dat$larvae_10m2)
dat$larvae_10m2[which(is.na(dat$larvae_10m2))] <- 0

# filtering of top species by
top_cpue <- dplyr::group_by(dat, scientific_name) %>%
  dplyr::summarise(tot = sum(larvae_10m2[which(season == 2)])) %>%
  dplyr::arrange(-tot)
top_cpue <- top_cpue[1:top_species, ]
dat <- dplyr::filter(dat, scientific_name %in% top_cpue$scientific_name)

# come up with prediction grid
resolution <- pred_resolution
dat$floor_lon <- floor(dat$longitude / resolution)
dat$floor_lat <- floor(dat$latitude / resolution)
dat$station <- paste(dat$floor_lon, dat$floor_lat)

pred_grid <- expand.grid(
  station = unique(dat$station),
  season = 2,
  year = unique(dat$year),
  species = unique(dat$scientific_name)
)
station_df <- data.frame(station = unique(dat$station))
station_df$longitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 1)))
station_df$latitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 2)))
pred_grid <- dplyr::left_join(pred_grid, station_df)

# model function
gam_fit <- function(df) {
  gam(larvae_10m2 ~ as.factor(year) + s(latitude, longitude, by = year),
    data = df,
    family = tw()
  )
}

# nest fitted and predicted data
dat_nested <-
  dat %>%
  dplyr::rename(species = scientific_name) %>%
  nest(-species) %>%
  rename(myorigdata = data)

# create second dataset - predictions
pred_nested <-
  pred_grid %>%
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

saveRDS(predictions_all, "indices/predicted_indices.rds")
