library(rerddap)
library(dplyr)
library(lubridate)
library(sf)

# settings
source("code/set_control_params.R")
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
  station_dat <- NULL
  try(
  station_dat <- tabledap(out, fields = c(
    "station", "line", "latitude",
    "longitude", "time", "scientific_name", "larvae_10m2"
  )), silent = TRUE)

  if(!is.null(station_dat)) {
    # dataset is very large; > 70 million rows
    station_dat <- as.data.frame(station_dat)
    station_dat$date <- lubridate::as_date(station_dat$time)
    station_dat$year <- lubridate::year(station_dat$date)
    station_dat$month <- lubridate::month(station_dat$date)
    station_dat$yday <- lubridate::yday(station_dat$date)

    # filter out recent years with consistent sampling
    station_dat <- dplyr::filter(station_dat, year >= min_year)
    # format response
    station_dat$larvae_10m2 <- as.numeric(station_dat$larvae_10m2)
    station_dat$larvae_10m2[which(is.na(station_dat$larvae_10m2))] <- 0

    # do lat / lon processing and add UTM
    station_dat$file = calcofi_erddap[i]

    if (i == 1) {
      dat <- station_dat
    } else {
      dat <- rbind(dat, station_dat)
    }
  }
}

stations <- read.csv("data/CalCOFIStationOrder.csv")
stations <- dplyr::rename(stations, station = Station, line = Line)
dat <- dplyr::left_join(dat, stations[, c("station", "line", "StaType")])
dat <- dplyr::filter(dat, StaType == "ROS") %>%
  dplyr::select(-StaType)

# remove species with 0 records - cuts about 150
dat <- dplyr::group_by(dat, scientific_name) %>%
  dplyr::mutate(tot = sum(larvae_10m2)) %>%
  dplyr::filter(tot > 0) %>%
  dplyr::select(-tot)

# filter out species with 30 or more years of data
thresh_spp <- dplyr::group_by(dat, scientific_name, year) %>%
  dplyr::summarise(s = sum(larvae_10m2,na.rm=T)) %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarize(n = length(which(s != 0))) %>%
  dplyr::filter(n >= min_years)
dat <- dplyr::filter(dat, scientific_name %in% thresh_spp$scientific_name)

# further drop spp with <
dat <- dplyr::group_by(dat, scientific_name) %>%
  dplyr::mutate(npos = length(which(larvae_10m2 > 0))) %>%
  dplyr::filter(npos > min_n) %>%
  dplyr::select(-npos)

dat <- dplyr::filter(dat, scientific_name %in% c("Disintegrated fish larvae",
                                     "Argyropelecus sladeni",
                                     "Lestidiops ringens",
                                     "Citharichthys",
                                     "Cyclothone",
                                     "Idiacanthus antrostomus",
                                     "Ceratoscopelus townsendi") == FALSE)


# convert to UTM - kms
# make the UTM cols spatial (X/Easting/lon, Y/Northing/lat)
dat$latitude = dat$lat_dd = as.numeric(dat$latitude)
dat$longitude = dat$lon_dd = as.numeric(dat$longitude)

dat_coords <-
  st_as_sf(dat[,c("longitude", "latitude")],
           coords = c("longitude", "latitude"),
           crs = 4326)
dat_coords <- st_transform(x = dat_coords, crs = 32610)
dat$longitude = as.numeric(st_coordinates(dat_coords)[, 1])
dat$latitude = as.numeric(st_coordinates(dat_coords)[, 2])
dat <- as.data.frame(dat)
dat$longitude <- dat$longitude / 1000 # to kms
dat$latitude <- dat$latitude / 1000 # to kms

# save a file with minimal -- but all key info for the index standardization
saveRDS(dat, "data/index_data.rds")
