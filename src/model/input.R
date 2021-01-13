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


# merge -------------------------------------------------------------------

df_merge <- df_obs %>%
  left_join(df_huc, by = "featureid") %>%
  left_join(df_covariates, by = "featureid") %>%
  left_join(df_temp, by = "featureid") %>%
  mutate_at(vars(starts_with("huc")), as.factor)


# filter ------------------------------------------------------------------

message(glue("Removing featureids with drainage area > 200 km2 (n = {sum(df_merge$AreaSqKM > 200)})"))
df_filter <- filter(df_merge, AreaSqKM <= 200)

message(glue("Removing featureids with missing stream temperature predictions (n = {sum(is.na(df_filter$mean_jul_temp))})"))
df_filter <- filter(df_filter, !is.na(mean_jul_temp))

message(glue("Removing featureids with (mean # days/year > 18 degC) >= 300 (n = {sum(is.na(df_filter$mean_jul_temp))})"))
# df_filter <- filter(df_filter, n_day_temp_gt_18 < 300)

stopifnot(all(!is.na(df_filter)))

df_filter <- df_filter %>%
  gather(var, value, -featureid, -presence, -huc12, -huc4, -huc8, -huc10)


# standardize -------------------------------------------------------------

df_var_std <- df_filter %>%
  group_by(var) %>%
  summarize(
    mean = mean(value),
    sd = sd(value),
    .groups = "drop"
  )

df_std <- df_long %>%
  group_by(var) %>%
  mutate(
    value = (value - mean(value)) / sd(value)
  ) %>%
  spread(var, value) %>%
  arrange(huc12, featureid)


# split -------------------------------------------------------------------

valid_frac <- 0.2
calib_frac <- 1 - valid_frac

n_calib_huc10 <- floor(length(unique(df_std$huc10)) * (1 - valid_frac))

set.seed(24744)
calib_huc10 <- sample(unique(df_std$huc10), n_calib_huc10, replace = FALSE)
valid_huc10 <- setdiff(unique(df_std$huc10), calib_huc10)

df_calib <- df %>%
  filter(huc10 %in% calib_huc10)
df_valid <- df %>%
  filter(huc10 %in% valid_huc10)

df_std_calib <- df_std %>%
  filter(huc10 %in% calib_huc10)
df_std_valid <- df_std %>%
  filter(huc10 %in% valid_huc10)

message(glue("split dataset into calibration (n={nrow(df_std_calib)}) and validation (n={nrow(df_std_valid)})"))


# export ------------------------------------------------------------------

list(
  datasets = list(
    obs = df_obs,
    covariates = df_covariates,
    huc = df_huc,
    temp = df_temp
  ),
  calib = list(
    huc10 = calib_huc10,
    data = df_calib,
    data_std = df_std_calib
  ),
  valid = list(
    huc10 = valid_huc10,
    data = df_valid,
    data_std = df_std_valid
  ),
  var_std = df_var_std
) %>%
  saveRDS(file.path(config$wd, "model-input.rds"))
