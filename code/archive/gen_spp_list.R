library(rerddap)
library(dplyr)
library(lubridate)

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
  # station_dat <- tabledap(out, fields = c(
  #   "station", "line", "latitude",
  #   "longitude", "time", "scientific_name", "larvae_10m2"
  # ))
  station_dat <- tabledap(out,
    fields = c(
      "larvae_10m2", "scientific_name", "time"
    )
  )

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

  station_dat$file = calcofi_erddap[i]
  station_dat <- dplyr::group_by(station_dat, scientific_name) %>%
    dplyr::summarize(tot_cpue = sum(larvae_10m2),
                     erddap = file[1])

  if (i == 1) {
    dat <- station_dat
  } else {
    dat <- rbind(dat, station_dat)
  }
}

saveRDS(dat, "indices/tot_cpue_species.rds")
