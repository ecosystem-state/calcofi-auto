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

dat = read.csv(file.choose())

dat$year <- lubridate::year(dat$time)
dat$yday <- lubridate::yday(dat$time)

years <- 
  dplyr::filter(dat, larvae_10m2 > 0) |>
  dplyr::group_by(scientific_name) |>
  dplyr::summarise(n = length(unique(year))) |>
  dplyr::filter(n >= 40)
# filter to species with enough years
dat <- dplyr::filter(dat, scientific_name %in% years$scientific_name)

samples <- dplyr::group_by(dat, scientific_name) |>
  dplyr::summarise(n = length(which(larvae_10m2>0))) |>
  dplyr::arrange(n) |>
  dplyr::filter(n >= 500)
dat <- dplyr::filter(dat, scientific_name %in% samples$scientific_name)


# create unique grid 
station_dat <- dplyr::group_by(dat, line.station, latitude, longitude,
                               time, year, yday) |>
  dplyr::summarise(n = n()) |> 
  dplyr::ungroup() |>
  dplyr::mutate(zday = (yday - mean(yday)) / sd(yday))

species_names = c(names(sort(table(dat$scientific_name), decreasing = TRUE)))

pred_grid <- dplyr::filter(dat, scientific_name == dat$scientific_name[1])
pred_grid <- sdmTMB::add_utm_columns(pred_grid, ll_names = c("longitude", "latitude"))
pred_grid <- dplyr::group_by(pred_grid, line.station) |>
  dplyr::summarise(X = mean(X), Y = mean(Y)) |>
  dplyr::ungroup()
pred_grid <- sdmTMB::replicate_df(pred_grid, time_name = "year", time_values = unique(dat$year))
pred_grid$fyear <- as.factor(pred_grid$year)
pred_grid$yday <- 105
pred_grid <- dplyr::filter(pred_grid, year >= 1985)
df = data.frame(species_names = species_names)

for(spp in 1:nrow(df)) {

    # fit sdmTMB model
    newdat <- dplyr::filter(dat,
                            scientific_name==df$species_names[spp],
                            year >= 1985)

    dat_summary <- sdmTMB::add_utm_columns(newdat, ll_names = c("longitude","latitude"))
    
    mesh = sdmTMB::make_mesh(dat_summary, xy_cols = c("X","Y"), cutoff = 50)

    dat_summary$fyear = as.factor(dat_summary$year)
    #newdat$present = ifelse(newdat$larvae_10m2 > 0, 1, 0)
    m <- sdmTMB::sdmTMB(larvae_10m2 ~ -1 + s(yday) + fyear,
        spatiotemporal = "iid",
        time="year",
        spatial="on",
        family = tweedie(),
        mesh=mesh,
        share_range = TRUE,
        data=dat_summary,
        priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 5, sigma_lt = 1)))

      s <- sanity(m, silent=TRUE)
      checks <- s$hessian_ok + s$eigen_values_ok + s$nlminb_ok + s$range_ok + s$se_na_ok + s$sigmas_ok
      if(checks == 6) {

        pred = predict(m,
                       pred_grid,
                       return_tmb_object = TRUE)
        index = get_index(pred, bias_correct = TRUE) 
  
        summaries <- dat_summary |> 
          dplyr::group_by(year) %>%
          dplyr::summarize(mean_cpue = mean(larvae_10m2),
                           n_pos_cpue = length(which(larvae_10m2 > 0)))
        summaries$species <- newdat$scientific_name[1]
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



library(viridis)
library(ggplot2)

predicted_indices_sdmtmb <- readRDS("~/Documents/Github projects/calcofi-auto/indices/predicted_indices_sdmtmb.rds")

ggplot(predicted_indices_sdmtmb, aes(x = year, y = est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = viridis(1)) +
  geom_line(col = viridis(1)) +
  labs(
    x = "Year",
    y = "Predicted index") +
  theme_bw() + 
  facet_wrap(~ species, scales = "free_y")
ggsave("figures/biomass_estimates_1985_2019.png", width = 10, height = 6)

ggplot(predicted_indices_sdmtmb, aes(x = year, y = log_est)) +
  geom_ribbon(aes(ymin = log_est-2*se, ymax = log_est+2*se), alpha = 0.2,fill = viridis(1)) +
  geom_line(col = viridis(1)) +
  labs(
    x = "Year",
    y = "Predicted ln index") +
  theme_bw() + 
  facet_wrap(~ species, scales = "free_y")
ggsave("figures/ln_biomass_estimates_1985_2019.png", width = 10, height = 6)
