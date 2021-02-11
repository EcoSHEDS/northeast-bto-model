# generate model-input from cleaned data
# <- {wd}/data-obs.rds
# <- {wd}/data-huc.rds
# <- {wd}/data-covariates.rds
# <- {wd}/data-temp-model.rds
# -> {wd}/model-input.rds

library(tidyverse)
library(glue)

source("src/functions.R")

config <- load_config()


# load datasets -----------------------------------------------------------

df_obs <- readRDS(file.path(config$wd, "data-obs.rds"))
df_huc <- readRDS(file.path(config$wd, "data-huc.rds")) %>%
  filter(featureid %in% df_obs$featureid)
df_covariates <- readRDS(file.path(config$wd, "data-covariates.rds")) %>%
  filter(featureid %in% df_obs$featureid)
df_temp <- readRDS(file.path(config$wd, "data-temp-model.rds")) %>%
  filter(featureid %in% df_obs$featureid) %>%
  select(featureid, mean_jul_temp, mean_summer_temp, n_day_temp_gt_18)
gis <- read_rds(file.path(config$wd, "gis.rds"))


# merge -------------------------------------------------------------------

df_merge <- df_obs %>%
  left_join(df_huc, by = "featureid") %>%
  left_join(df_covariates, by = "featureid") %>%
  left_join(df_temp, by = "featureid") %>%
  mutate_at(vars(starts_with("huc")), as.factor)


# filter ------------------------------------------------------------------

message(glue("Removing featureids with missing stream temperature predictions (n = {sum(is.na(df_merge$mean_jul_temp))})"))
df_filter <- filter(df_merge, !is.na(mean_jul_temp))

stopifnot(all(!is.na(df_filter)))

df_filter <- df_filter %>%
  gather(var, value, -featureid, -presence, -huc12, -huc4, -huc8, -huc10)

# split -------------------------------------------------------------------

valid_frac <- 0.2
calib_frac <- 1 - valid_frac

n_calib_huc10 <- floor(length(unique(df_std$huc10)) * (1 - valid_frac))

set.seed(24744)
calib_huc10 <- sample(unique(df_std$huc10), n_calib_huc10, replace = FALSE)
valid_huc10 <- setdiff(unique(df_std$huc10), calib_huc10)

df <- df_filter %>%
  spread(var, value) %>%
  mutate(partition = if_else(huc10 %in% calib_huc10, "calib", "valid")) %>%
  relocate(partition)

message(glue("split dataset into calibration (n={sum(df$partition == 'calib')}) and validation (n={sum(df$partition == 'valid')})"))


# standardize -------------------------------------------------------------

df_var_std <- df %>%
  pivot_longer(-c(partition, featureid, presence, huc4, huc8, huc10, huc12)) %>%
  group_by(name) %>%
  summarize(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

df_std <- df %>%
  pivot_longer(-c(partition, featureid, presence, huc4, huc8, huc10, huc12)) %>%
  group_by(name) %>%
  mutate(
    value = (value - mean(value)) / sd(value)
  ) %>%
  spread(name, value) %>%
  arrange(partition, huc12, featureid)

# summary -----------------------------------------------------------------

covariates <- c(
  "AreaSqKM",
  "agriculture",
  "devel_hi",
  "forest",
  "allonnet",
  "summer_prcp_mm",
  "mean_jul_temp"
)

# calibration/validation
gis$catchments %>%
  inner_join(df, by = "featureid") %>%
  ggplot() +
  geom_sf(aes(color = partition)) +
  geom_sf(data = gis$states, fill = NA, size = 1) +
  scale_color_brewer("Parition", type = "qual", palette = 2)

df %>%
  select(partition, featureid, all_of(covariates)) %>%
  pivot_longer(-c(partition, featureid)) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(vars(partition), vars(name), scales = "free") +
  labs(x = NULL, y = "# Catchments")

df_std %>%
  select(partition, featureid, all_of(covariates)) %>%
  pivot_longer(-c(partition, featureid)) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(vars(partition), vars(name), scales = "free") +
  labs(x = NULL, y = "# Catchments")

p <- map(c(covariates, "presence"), function (v) {
  gis$catchments %>%
    inner_join(df, by = "featureid") %>%
    ggplot() +
    geom_sf(aes_string(color = v), size = 0.5) +
    geom_sf(data = gis$states, fill = NA, size = 0.5) +
    scale_color_viridis_c() +
    labs(title = v)
})
cowplot::plot_grid(plotlist = p, ncol = 3)

df %>%
  select(presence, all_of(covariates)) %>%
  pivot_longer(-presence) %>%
  ggplot(aes(value, presence)) +
  geom_jitter(width = 0, height = 0.01, alpha = 0.1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(vars(name), scales = "free_x", strip.position = "bottom") +
  labs(x = NULL, y = "prob(presence)") +
  theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))

df %>%
  select(presence, all_of(covariates)) %>%
  pivot_longer(-presence) %>%
  ggplot(aes(factor(presence), value)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "red") +
  facet_wrap(vars(name), scales = "free_y", strip.position = "left") +
  labs(x = "Presence/Absence", y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))

df %>%
  select(all_of(covariates), presence) %>%
  GGally::ggpairs(columns = covariates, mapping = aes(alpha = 0.5, color = factor(presence))) +
  scale_fill_manual("Presence/\nAbsence", values = c("0" = "orangered", "1" = "deepskyblue"))

df %>%
  select(all_of(covariates)) %>%
  GGally::ggpairs(mapping = aes(alpha = 0.25))


# export ------------------------------------------------------------------

list(
  inputs = list(
    obs = df_obs,
    covariates = df_covariates,
    huc = df_huc,
    temp = df_temp
  ),
  data = df,
  data_std = df_std,
  var_std = df_var_std
) %>%
  saveRDS(file.path(config$wd, "model-input.rds"))
