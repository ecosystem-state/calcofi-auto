library(dplyr)
library(lubridate)
library(usethis)

source("code/set_control_params.R")

url_str <- "https://github.com/ecosystem-state/ecodata/blob/main/inst/calcofi_index_data.rds"
usethis::use_github_file(url_str,
                         save_as = "data/index_data.rds")

dat = readRDS("data/index_data.rds")

# filter out recent years with consistent sampling
dat <- dplyr::filter(dat, year >= min_year)

spp <- unique(dat$scientific_name)[1]
dat = dplyr::filter(dat,
                    scientific_name == spp)
dat$latitude <- as.numeric(dat$latitude)
dat$longitude <- as.numeric(dat$longitude)

# convert to UTM - kms
# make the UTM cols spatial (X/Easting/lon, Y/Northing/lat)
#dat <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)
# transform to UTM
#dat<-st_transform(x = dat, crs = 32610)
#dat$longitude = st_coordinates(dat)[,1]
#dat$latitude = st_coordinates(dat)[,2]

#dat <- as.data.frame(dat)
#dat$longitude <- dat$longitude / 1000
#dat$latitude <- dat$latitude / 1000

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
# expand stations: years
pred_grid <- expand.grid(
  station = unique(dat$station),
  year = unique(dat$year)
)

station_df <- data.frame(station = unique(dat$station))
station_df$longitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 1))) * resolution
station_df$latitude <- as.numeric(unlist(lapply(strsplit(as.character(station_df$station), " "), getElement, 2))) * resolution
pred_grid <- dplyr::left_join(pred_grid, station_df)

saveRDS(pred_grid, "indices/pred_grid.rds")


