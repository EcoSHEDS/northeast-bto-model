# generate model-input from cleaned data
# <- {wd}/data-obs.rds
# <- {wd}/data-huc.rds
# <- {wd}/data-covariates.rds
# <- {wd}/data-temp.rds
# -> {wd}/model-input.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-input: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()


# load datasets -----------------------------------------------------------

cat("loading obs, huc, covariates, temp datasets...")
df_obs <- readRDS(file.path(config$wd, "data-obs.rds"))
df_huc <- readRDS(file.path(config$wd, "data-huc.rds")) %>%
  filter(featureid %in% df_obs$featureid)
df_covariates <- readRDS(file.path(config$wd, "data-covariates.rds")) %>%
  filter(featureid %in% df_obs$featureid)
df_temp <- readRDS(file.path(config$wd, "data-temp.rds")) %>%
  filter(featureid %in% df_obs$featureid)
cat("done\n")

# merge -------------------------------------------------------------------

cat("merging datasets...")
df <- df_obs %>%
  left_join(df_huc, by = "featureid") %>%
  left_join(df_covariates, by = "featureid") %>%
  left_join(df_temp, by = "featureid")
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("converting hucs to factors...")
df <- df %>%
  mutate_at(vars(starts_with("huc")), as.factor)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

# filter ------------------------------------------------------------------

cat("removing featureids with drainage area > 200 km2 (n = ", scales::comma(sum(df$AreaSqKM > 200 )), ")...", sep = "")
df <- filter(df, AreaSqKM <= 200)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("removing featureids with missing temp predictions (n = ", scales::comma(sum(is.na(df$mean_jul_temp))), ")...", sep = "")
df <- filter(df, !is.na(mean_jul_temp))
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("removing featureids with [mean # days > 18 degC] >= 300 (n = ", scales::comma(sum(df$n_day_temp_gt_18 >= 300)), ")...", sep = "")
df <- filter(df, n_day_temp_gt_18 < 300)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

if (any(is.na(df))) {
  stop("ERROR: cleaned dataset contains missing values (n = ", sum(is.na(df)), ")")
}

# standardize -------------------------------------------------------------

cat("converting to long format...")
df_long <- df %>%
  gather(var, value, -featureid, -presence, -huc12, -huc4, -huc8, -huc10)
cat("done\n")

cat("computing mean/sd of each variable...")
df_var_std <- df_long %>%
  group_by(var) %>%
  summarize(
    mean = mean(value),
    sd = sd(value)
  )
cat("done\n")

cat("standardizing variables...")
df_std <- df_long %>%
  group_by(var) %>%
  mutate(
    value = (value - mean(value)) / sd(value)
  ) %>%
  spread(var, value) %>%
  arrange(huc12, featureid)
cat("done\n")


# split -------------------------------------------------------------------

valid_frac <- 0.2
calib_frac <- 1 - valid_frac

cat("splitting dataset into calibration (", scales::percent(calib_frac), ") and validation (", scales::percent(valid_frac), ") by huc10...", sep = "")
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

cat("done (nrow calib = ", scales::comma(nrow(df_std_calib)), ", nrow valid = ", scales::comma(nrow(df_std_valid)), ")\n", sep = "")

# export ------------------------------------------------------------------

cat("saving model input datasets to model-input.rds...")
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
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished model-input: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed, digits = 1), " sec)\n", sep = "")

