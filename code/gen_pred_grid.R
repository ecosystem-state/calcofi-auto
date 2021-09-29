library(rerddap)
library(dplyr)
library(lubridate)
library(sp)

pred_resolution <- 5 # resolution of prediction grid, km
min_year <- 1985

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

# # filter out recent years with consistent sampling
# dat <- dplyr::filter(dat, year >= min_year)
#
# dat = dplyr::filter(dat,
#                     scientific_name == unique(dat$scientific_name)[1])
# dat$latitude <- as.numeric(dat$latitude)
# dat$longitude <- as.numeric(dat$longitude)
#
# # convert to UTM - kms
# sp::coordinates(dat) <- c("longitude", "latitude")
# sp::proj4string(dat) <- sp::CRS("+proj=longlat + ellps=WGS84 +datum=WGS84")
# dat <- try(sp::spTransform(dat, CRS = "+proj=utm +zone=10 +datum=WGS84"),
#   silent = TRUE
# )
# dat <- as.data.frame(dat)
# dat$longitude <- dat$longitude / 1000
# dat$latitude <- dat$latitude / 1000
#
# # come up with prediction grid
# resolution <- pred_resolution
# dat$floor_lon <- floor(dat$longitude / resolution)
# dat$floor_lat <- floor(dat$latitude / resolution)
# dat$station <- paste(dat$floor_lon, dat$floor_lat)
#
# pred_grid <- expand.grid(
#   station = unique(dat$station),
#   season = 2,
#   year = unique(dat$year)
# )
#
# station_df <- data.frame(station = unique(dat$station))
# station_df$longitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 1)))
# station_df$latitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 2)))
# pred_grid <- dplyr::left_join(pred_grid, station_df)
pred_grid = data.frame(x=1)
saveRDS(pred_grid, "indices/pred_grid.rds")
