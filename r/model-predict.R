# predict all catchments
# <- {wd}/model-input.rds
# <- {wd}/model-calib.rds
# -> {wd}/model-predict.rds
# -> {wd}/model-predict.csv

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-predict: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(AUC))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(boot))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(lme4))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading model-input...")
inp <- readRDS(file.path(config$wd, "model-input.rds"))
cat("done\n")

cat("loading model-calib...")
calib <- readRDS(file.path(config$wd, "model-calib.rds"))
cat("done\n")

# fitted model
glmm <- calib$model

cat("loading huc, covariates, temp datasets...")
df_huc <- readRDS(file.path(config$wd, "data-huc.rds"))
df_covariates <- readRDS(file.path(config$wd, "data-covariates.rds"))
df_temp <- readRDS(file.path(config$wd, "data-temp.rds"))
cat("done\n")

cat("merging datasets...")
df <- df_huc %>%
  left_join(df_covariates, by = "featureid") %>%
  left_join(df_temp, by = "featureid")
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("converting hucs to factors...")
df <- df %>%
  mutate_at(vars(starts_with("huc")), as.factor)
cat("done\n")

# filter ------------------------------------------------------------------

cat("removing featureids with drainage area > 200 km2 (n = ", scales::comma(sum(df$AreaSqKM > 200 )), ")...", sep = "")
df <- filter(df, AreaSqKM <= 200)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("removing featureids with missing temp predictions (n = ", scales::comma(sum(is.na(df$meanJulyTemp))), ")...", sep = "")
df <- filter(df, !is.na(meanJulyTemp))
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("removing featureids with [mean # days > 18 degC] >= 300 (n = ", scales::comma(sum(df$meanDays.18 >= 300)), ")...", sep = "")
df <- filter(df, meanDays.18 < 300)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

if (any(is.na(df))) {
  stop("ERROR: cleaned dataset contains missing values (n = ", sum(is.na(df)), ")")
}


# standardize -------------------------------------------------------------

cat("standardizing covariates...")

df_std <- inp$var_std

df_long <- df %>%
  gather(var, value, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
  left_join(df_std, by = "var") %>%
  mutate(
    value = (value - mean) / sd
  )

df <- df_long %>%
  select(-mean, -sd) %>%
  spread(var, value)

cat("done\n")

if (any(is.na(df))) {
  stop("ERROR: standardized dataset contains missing values (n = ", sum(is.na(df)), ")")
}


# predict -----------------------------------------------------------------

cat("computing predictions and GOF stats...")
df <- df %>%
  mutate(
    pred_pct = inv.logit(predict(glmm, df, allow.new.levels = TRUE)),
    pred_presence = pred_pct >= 0.5
  )
cat("done\n")

# export ------------------------------------------------------------------

list(
  data = df
) %>%
  saveRDS(file.path(config$wd, "model-predict.rds"))

df_huc %>%
  select(featureid) %>%
  left_join(
    df %>%
      select(featureid, pred_pct, pred_presence),
    by = "featureid"
  ) %>%
  write_csv(file.path(config$wd, "model-predict.csv"), na = "")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished model-predict: ", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n", sep = "")
