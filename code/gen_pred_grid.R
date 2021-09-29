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

# filter out recent years with consistent sampling
dat <- dplyr::filter(dat, year >= min_year)

spp <- unique(dat$scientific_name)[1]
dat = dplyr::filter(dat,
                    scientific_name == spp)
dat$latitude <- as.numeric(dat$latitude)
dat$longitude <- as.numeric(dat$longitude)

pred_grid = data.frame(x=1)
saveRDS(pred_grid, "indices/pred_grid.rds")
