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

df_var_std <- inp$var_std

df_z_long <- df %>%
  gather(var, value, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
  left_join(df_var_std, by = "var") %>%
  mutate(
    value = (value - mean) / sd
  )

df_z <- df_z_long %>%
  select(-mean, -sd) %>%
  spread(var, value)

cat("done\n")

if (any(is.na(df_z))) {
  stop("ERROR: standardized dataset contains missing values (n = ", sum(is.na(df_z)), ")")
}


# predict: temp7pN ----------------------------------------------------
# increase meanJulyTemp by N degC

cat("calculating predictions for temp7p scenarios\n")
temp7p_values <- seq(0, 6, by = 0.5)

df_temp7p <- lapply(seq_along(temp7p_values), function (i) {
  temp7p_value <- temp7p_values[i]
  cat("computing temp7p scenario: ", temp7p_value, " degC\n", sep = "")

  df_temp_scenario <- df %>%
    mutate(
      meanJulyTemp = meanJulyTemp + temp7p_value
    )

  df_temp_scenario_z_long <- df_temp_scenario %>%
    gather(var, value, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
    left_join(df_var_std, by = "var") %>%
    mutate(
      value = (value - mean) / sd
    )

  df_temp_scenario_z <- df_temp_scenario_z_long %>%
    select(-mean, -sd) %>%
    spread(var, value)

  data_frame(
    featureid = df_temp_scenario_z$featureid,
    prob = inv.logit(predict(glmm, df_temp_scenario_z, allow.new.levels = TRUE))
  ) %>%
    mutate(
      temp7p_value = temp7p_value
    )
}) %>%
  bind_rows() %>%
  mutate(
    scenario = paste0("temp7p", sprintf("%02d", temp7p_value * 10))
  )


# table(df_temp7p$scenario)
# df_temp7p %>%
#   ggplot(aes(prob)) +
#   geom_histogram() +
#   facet_wrap(~ scenario)


# predict: current --------------------------------------------------------

cat("extracting predictions for current conditions...")
df_current <- df_temp7p %>%
  filter(temp7p_value == 0) %>%
  select(-temp7p_value) %>%
  mutate(scenario = "current")
cat("done\n")



# predict: max_temp7p_occN ------------------------------------------------
# maximum mean July temperature increase corresponding with occupancy prob >= N

cat("calculating predictions for max_temp7p_occN scenarios...")
max_temp7p_probs <- c(0.3, 0.5, 0.7)

df_max_temp7p <- lapply(seq_along(max_temp7p_probs), function(i) {
  max_temp7p_prob <- max_temp7p_probs[i]
  x <- df_temp7p %>%
    filter(prob >= max_temp7p_prob) %>%
    group_by(featureid) %>%
    summarise(
      value = max(temp7p_value)
    ) %>%
    mutate(
      scenario = paste0("max_temp7p_occ", max_temp7p_prob * 100)
    )
}) %>%
  bind_rows()
cat("done\n")
#
# table(df_max_temp7p$scenario)
# df_max_temp7p %>%
#   ggplot(aes(value)) +
#   geom_histogram() +
#   facet_wrap(~ scenario)

# merge -------------------------------------------------------------------

cat("merging prediction scenarios...")
df <- df_current %>%
  bind_rows(
    df_temp7p %>%
      filter(temp7p_value > 0) %>%
      select(-temp7p_value)
  ) %>%
  mutate(scenario = paste0("occ_", scenario)) %>%
  spread(scenario, prob) %>%
  full_join(
    df_max_temp7p %>%
      spread(scenario, value),
    by = "featureid"
  )

df <- df_huc %>%
  select(featureid, huc12) %>%
  left_join(
    df,
    by = "featureid"
  )

stopifnot(sum(duplicated(df$featureid)) == 0)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

# export ------------------------------------------------------------------

cat("saving to model-predict.rds...")
df %>%
  saveRDS(file.path(config$wd, "model-predict.rds"))
cat("done\n")

cat("saving to model-predict.csv...")
df %>%
  mutate(
    featureid = sprintf("%.0f", featureid)
  ) %>%
  write_csv(file.path(config$wd, "model-predict.csv"), na = "")
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished model-predict: ", as.character(end, tz = "US/Eastern"), "(elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
