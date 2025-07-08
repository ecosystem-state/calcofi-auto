library(sdmTMB)
library(dplyr)
library(lubridate)
library(tidyr)
library(curl)
library(tidyverse)
# settings
source("code/set_control_params.R")


dat = read.csv("data/updated through 2304 on 3.28.24 ichthyoplankton by line and station core stations.csv")
# Pivot columns 11 to 109 (species) to longer format
dat_long <- dat %>%
  pivot_longer(
    cols = 11:109,
    names_to = "species",
    values_to = "count"
  ) %>%
  # Clean species names:
  mutate(
    species = species %>%
      str_replace_all("\\.", " ") %>%           # Replace periods with spaces
      str_remove_all("\\d+$") %>%               # Remove trailing digits
      str_trim()                                # Remove any leading/trailing whitespace
  )
# keep recent years
dat_long <- dplyr::filter(dat_long, year >= 1985)

species_to_keep <- dplyr::group_by(dat_long, species) |>
  dplyr::summarise(n = length(unique(year[which(count>0)]))) |>
  dplyr::filter(n == length(unique(dat_long$year))) |>
  dplyr::arrange(desc(n))

# filter to species with enough years
dat_long <- dplyr::filter(dat_long, species %in% species_to_keep$species)

species_names = unique(dat_long$species)

pred_grid <- dat_long |>
  dplyr::mutate(SL_SS = paste(S_L, S_S)) |>
  dplyr::group_by(SL_SS) |>
  dplyr::summarise(S_L = S_L[1], S_S = S_S[1],
                   longitude = longitude[1],
                   latitude= latitude[1])
pred_grid <- sdmTMB::replicate_df(pred_grid, time_name = "year", time_values = unique(dat_long$year))

pred_grid$fyear <- as.factor(pred_grid$year)
pred_grid$season = "spring"
pred_grid$stratum <- ifelse(pred_grid$S_S <= 60, "inshore", "offshore")
pred_grid <- add_utm_columns(pred_grid, ll_names = c("longitude","latitude"))

df = data.frame(species_names = species_names)

for(spp in 1:nrow(df)) {

    # fit sdmTMB model
    newdat <- dplyr::filter(dat_long,
                            species==df$species_names[spp]) |>
      dplyr::select(-X)

    dat_summary <- sdmTMB::add_utm_columns(newdat, ll_names = c("longitude","latitude"))
    
    mesh = sdmTMB::make_mesh(dat_summary, xy_cols = c("X","Y"), cutoff = 50)

    dat_summary$fyear = as.factor(dat_summary$year)
    #newdat$present = ifelse(newdat$larvae_10m2 > 0, 1, 0)
    m <- sdmTMB::sdmTMB(count ~ -1 + season + fyear,
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
                       pred_grid[which(pred_grid$stratum=="inshore"),],
                       return_tmb_object = TRUE)
        index_inshore = get_index(pred, bias_correct = TRUE) 
        index_inshore$index <- "Inshore"
        
        pred = predict(m,
                       pred_grid[which(pred_grid$stratum!="inshore"),],
                       return_tmb_object = TRUE)
        index_offshore = get_index(pred, bias_correct = TRUE) 
        index_offshore$index <- "Offshore"
        
        pred = predict(m,
                       pred_grid,
                       return_tmb_object = TRUE)
        index = get_index(pred, bias_correct = TRUE) 
        index$index <- "Total"
        
        index_all <- rbind(index_inshore, index_offshore, index)
        index$species <- newdat$species[1]

        if(spp == 1) {
          predictions_all = index
        } else {
          predictions_all = rbind(predictions_all, index)
        }
        saveRDS(predictions_all, "indices/predicted_indices_sdmtmb_newdata.rds")
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
