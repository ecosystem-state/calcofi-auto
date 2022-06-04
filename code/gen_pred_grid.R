library(rerddap)
library(dplyr)
library(lubridate)
library(sf)

source("code/set_control_params.R")

# list of all datasets
calcofi_erddap <- c(
  "erdCalCOFIlrvcntAtoAM"
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

dat <- as.data.frame(dat)
dat$date <- lubridate::as_date(dat$time)
dat$year <- lubridate::year(dat$date)
dat$month <- lubridate::month(dat$date)
dat$yday <- lubridate::yday(dat$date)

# filter out recent years with consistent sampling
dat <- dplyr::filter(dat, year >= min_year)

spp <- unique(dat$scientific_name)[1]
dat = dplyr::filter(dat,
                    scientific_name == spp)
dat$latitude <- as.numeric(dat$latitude)
dat$longitude <- as.numeric(dat$longitude)

# convert to UTM - kms
# make the UTM cols spatial (X/Easting/lon, Y/Northing/lat)
dat <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)
# transform to UTM
dat<-st_transform(x = dat, crs = 32610)
dat$longitude = st_coordinates(dat)[,1]
dat$latitude = st_coordinates(dat)[,2]

dat <- as.data.frame(dat)
dat$longitude <- dat$longitude / 1000
dat$latitude <- dat$latitude / 1000

dat$season <- NA
dat$season[which(dat$yday %in% 2:52)] <- 1
dat$season[which(dat$yday %in% 90:141)] <- 2
dat$season[which(dat$yday %in% 181:233)] <- 3
dat$season[which(dat$yday %in% 273:325)] <- 4
dat <- dplyr::filter(dat,!is.na(season))
dat$season <- as.factor(dat$season)
dat <- dplyr::filter(dat, season %in% use_seasons)

# come up with prediction grid
resolution <- pred_resolution
dat$floor_lon <- floor(dat$longitude / resolution)
dat$floor_lat <- floor(dat$latitude / resolution)
dat$station <- paste(dat$floor_lon, dat$floor_lat)
#
pred_grid <- expand.grid(
  station = unique(dat$station),
  year = unique(dat$year)
)

station_df <- data.frame(station = unique(dat$station))
station_df$longitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 1)))
station_df$latitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 2)))
pred_grid <- dplyr::left_join(pred_grid, station_df)

saveRDS(pred_grid, "indices/pred_grid.rds")
