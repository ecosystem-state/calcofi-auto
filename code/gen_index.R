library(sdmTMB)
library(dplyr)
library(lubridate)
library(tidyr)
library(curl)

# settings
source("code/set_control_params.R")

# load prediction grid
pred_grid <- readRDS("indices/pred_grid.rds")
pred_grid$season <- 2
pred_grid$yday <- 105 # Apr 15
pred_grid$fyear <- as.factor(pred_grid$year)
pred_grid <- dplyr::rename(pred_grid, X = longitude, Y = latitude) |>
  dplyr::filter(year == 2020)
pred_grid <- sdmTMB::replicate_df(pred_grid, time_name = "year", time_values = 1985:2023)
# model function - edit to change form of model
# gam_fit <- function(df) {
#   gam(larvae_10m2 ~ as.factor(year) + s(latitude, longitude, by = year),
#       data = df,
#       family = tw()
#   )
# }

#url_str <- "https://github.com/ecosystem-state/ecodata/blob/main/inst/calcofi_index_data.rds"
#usethis::use_github_file(url_str,
#                         save_as = "data/index_data.rds")

githubURL <- ("https://raw.githubusercontent.com/ecosystem-state/ecodata/main/inst/calcofi_index_data.rds")
download.file(githubURL,"data/index_data.rds", method="curl")

dat = readRDS("data/index_data.rds") |>
  dplyr::filter(larvae_10m2 > 0)
  
# create unique grid 
station_dat <- dplyr::group_by(dat, station, line, latitude, longitude,
                               time, date, year, month, yday, lat_dd, lon_dd) |>
  dplyr::summarise(n = n()) |> 
  dplyr::ungroup() |>
  dplyr::mutate(zday = (yday - mean(yday)) / sd(yday))

species_names = c(names(sort(table(dat$scientific_name), decreasing = TRUE)))

df = data.frame(species_names = species_names,
                spatiotemporal1 = "iid",
                spatiotemporal2 = "off")
# df is ordered by species, data rich to poor. many data poor species
# can't support spatiotemporal pos fields
df$spatiotemporal1[4] <- "off"
df$spatiotemporal1[10] <- "off"
df$spatiotemporal1[16] <- "off"
df$spatiotemporal1[20:nrow(df)] <- "off"

for(spp in 1:nrow(df)) {

    # fit sdmTMB model
    newdat <- dplyr::filter(dat,
                            scientific_name==df$species_names[spp])

    dat_summary <- dplyr::left_join(station_dat, newdat)
    dat_summary$larvae_10m2[which(is.na(dat_summary$larvae_10m2))] <- 0
    dat_summary <- add_utm_columns(dat_summary, ll_names = c("lon_dd", "lat_dd"))
      #if(spp == 1) {
        # 10 ~ 181 knots
        mesh = make_mesh(dat_summary, xy_cols = c("X","Y"),
                         cutoff = 50)
      #}

      dat_summary$fyear = as.factor(dat_summary$year)
      #newdat$present = ifelse(newdat$larvae_10m2 > 0, 1, 0)
      m <- sdmTMB(larvae_10m2 ~ -1 + s(yday) + fyear,
                  spatiotemporal = list(df$spatiotemporal1[spp], df$spatiotemporal2[spp]),
                  time="year",
                  spatial="on",
                  family = delta_lognormal(),
                  mesh=mesh,
                  share_range = TRUE,
                  data=dat_summary,
                  priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 5, sigma_lt = 1)))

      if(sanity(m, silent=TRUE)$all_ok == TRUE) {

        
        pred = predict(m,
                       pred_grid,
                       return_tmb_object = TRUE)
        index = get_index(pred, bias_correct = TRUE) 
  
        summaries <- dat_summary |> 
          dplyr::group_by(year) %>%
          dplyr::summarize(mean_cpue = mean(larvae_10m2),
                           n_pos_cpue = length(which(larvae_10m2 > 0)))
        summaries$species <- newdat$common_name[1]
        pred = left_join(summaries, index[,c("year","est","lwr","upr","log_est","se")])
  
        if(spp == 1) {
          predictions_all = pred
        } else {
          predictions_all = rbind(predictions_all, pred)
        }
        saveRDS(predictions_all, "indices/predicted_indices_sdmtmb.rds")
      }
      print(spp)
} # end spp loop



