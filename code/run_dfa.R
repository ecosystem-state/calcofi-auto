library(bayesdfa)
library(dplyr)
library(ggplot2)

dat <- readRDS("indices/predicted_indices.rds")

# optional - remove species with > max_zeros years with 0 cpue
max_zeros <- 10 # number of years with missing data allowed
dat <- dplyr::group_by(dat, species) %>%
  dplyr::mutate(n = length(which(mean_cpue==0))) %>%
  dplyr::filter(n <= max_zeros)

# turn all mean_cpue values to NAs
dat$index[which(dat$mean_cpue==0)] = NA

# rename cols to meet needs of fit_dfa
dat <- dplyr::rename(dat, obs = index)
dat$time <- dat$year - min(dat$year) + 1
dat$ts <- as.numeric(as.factor(dat$species))

# 3 trend model does a bit better than 1-2 trend models
fit <- fit_dfa(y = dat,
                 num_trends = 3,
                 data_shape="long",
                 trend_model = "ps",
                 n_knots = 20,
                 iter=3000,
                 chains=3)
r <- rotate_trends(fit)

spp_names = levels(as.factor(fit$orig_data$name))

# Make plot of trends
jpeg("figures/trends.jpeg", quality=100)
plot_trends(r, years = unique(dat$year)) +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"))
dev.off()

# Make plot of loadings
jpeg("figures/loadings.jpeg", quality=100, height = 480*1.5, width=480*1.1)
plot_loadings(r, names = spp_names) +
  theme_bw() +
  scale_fill_viridis_d(end=0.8) +
  theme(strip.background =element_rect(fill="white"))
dev.off()

# Make abbreviated names for plots
abbr_names <- spp_names
for(i in 1:length(abbr_names)) {
  genus_spp <- strsplit(abbr_names[i]," ")
  if(length(unlist(genus_spp)) == 2) {
    abbr_names[i] <- paste0(substr(unlist(genus_spp)[1],1,1),". ",unlist(genus_spp)[2])
  }
}

# Make plot of fitted
jpeg("figures/fitted.jpeg", quality=100, height = 480, width = 480*1.5)
plot_fitted(fit, names = abbr_names, time_labels = unique(dat$year)) +
  theme_bw() +
  scale_fill_viridis_d(end=0.8) +
  theme(strip.background =element_rect(fill="white")) +
  theme(strip.text.x = element_text(size = 8))
dev.off()

