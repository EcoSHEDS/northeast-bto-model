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

cat("removing featureids with missing temp predictions (n = ", scales::comma(sum(is.na(df$mean_jul_temp))), ")...", sep = "")
df <- filter(df, !is.na(mean_jul_temp))
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

cat("removing featureids with [mean # days > 18 degC] >= 300 (n = ", scales::comma(sum(df$n_day_temp_gt_18 >= 300)), ")...", sep = "")
df <- filter(df, n_day_temp_gt_18 < 300)
cat("done (nrow = ", scales::comma(nrow(df)), ")\n", sep = "")

if (any(is.na(df))) {
  stop("ERROR: cleaned dataset contains missing values (n = ", sum(is.na(df)), ")")
}


# set up air temp scenarios -----------------------------------------------

df_temp_air <- df %>%
  select(featureid, starts_with("mean_jul_temp")) %>%
  rename(mean_jul_temp_air0 = mean_jul_temp) %>%
  gather(var, mean_jul_temp, -featureid) %>%
  mutate(
    air = as.numeric(str_sub(var, 18, 19))
  ) %>%
  select(air, featureid, mean_jul_temp)

df <- df %>%
  select(-starts_with("mean_jul_temp"), -mean_summer_temp, -n_day_temp_gt_18) %>%
  full_join(
    df_temp_air,
    by = "featureid"
  ) %>%
  select(air, everything())

df_base <- df %>%
  filter(air == 0) %>%
  select(-air)
df_air <- df %>%
  filter(air > 0)

# standardize -------------------------------------------------------------

cat("standardizing covariates...")

df_var_std <- inp$var_std

# NOTE: no longer used, standardization happens in temp7p loop
#
# df_z_long <- df %>%
#   gather(var, value, -air, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
#   left_join(df_var_std, by = "var") %>%
#   mutate(
#     value = (value - mean) / sd
#   )
#
# df_z <- df_z_long %>%
#   select(-mean, -sd) %>%
#   spread(var, value)
#
cat("done\n")
#
# if (any(is.na(df_z))) {
#   stop("ERROR: standardized dataset contains missing values (n = ", sum(is.na(df_z)), ")")
# }
#

# predict: current --------------------------------------------------------

cat("calculating predictions for current conditions...")
df_current <- df_base %>%
  gather(var, value, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
  left_join(df_var_std, by = "var") %>%
  mutate(
    value = (value - mean) / sd
  ) %>%
  select(-mean, -sd) %>%
  spread(var, value)
df_current$prob <- inv.logit(predict(glmm, df_current, allow.new.levels = TRUE))
df_current <- df_current %>%
  select(featureid, prob) %>%
  mutate(scenario = "current")
cat("done\n")

# predict: temp7 ------------------------------------------------------
# increase mean_jul_temp by N degC

cat("calculating predictions for temp7 scenarios\n")
temp7_values <- seq(1, 6, by = 1)

df_temp7 <- lapply(seq_along(temp7_values), function (i) {
  temp7_value <- temp7_values[i]
  cat("computing temp7 scenario: ", temp7_value, " degC\n", sep = "")

  df_temp_scenario <- df_base %>%
    mutate(
      mean_jul_temp = mean_jul_temp + temp7_value
    )

  df_temp_scenario_z <- df_temp_scenario %>%
    gather(var, value, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
    left_join(df_var_std, by = "var") %>%
    mutate(
      value = (value - mean) / sd
    ) %>%
    select(-mean, -sd) %>%
    spread(var, value)

  tibble(
    featureid = df_temp_scenario_z$featureid,
    prob = inv.logit(predict(glmm, df_temp_scenario_z, allow.new.levels = TRUE))
  ) %>%
    mutate(
      temp7 = temp7_value
    )
}) %>%
  bind_rows() %>%
  mutate(
    scenario = paste0("temp7_", temp7)
  )


# table(df_temp7$scenario)
# df_temp7 %>%
#   ggplot(aes(prob)) +
#   geom_histogram(bins = 30) +
#   facet_wrap(~ scenario)

# predict: air temp -------------------------------------------------------

cat("computing predictions for air temperature scenarios...")
df_air <- df_air %>%
  gather(var, value, -air, -featureid, -huc12, -huc4, -huc8, -huc10) %>%
  left_join(df_var_std, by = "var") %>%
  mutate(
    value = (value - mean) / sd
  ) %>%
  select(-mean, -sd) %>%
  spread(var, value)
df_air$prob <- inv.logit(predict(glmm, df_air, allow.new.levels = TRUE))
df_air <- df_air %>%
  select(featureid, prob, air) %>%
  mutate(scenario = paste0("air_", air))
cat("done\n")

# table(df_air$scenario)
# df_air %>%
#   ggplot(aes(prob)) +
#   geom_histogram(bins = 30) +
#   facet_wrap(~ scenario)

# predict: max_temp7_occN -------------------------------------------------
# maximum mean July temperature increase corresponding with occupancy prob >= N

cat("calculating predictions for max_temp7_occN scenarios...")
df_max_temp7 <- bind_rows(
    df_current %>%
      mutate(temp7 = 0),
    df_temp7
  ) %>%
  arrange(featureid, temp7) %>%
  group_by(featureid) %>%
  summarise(
    max_temp7_occ30 = approx(prob, temp7, xout = 0.3, yleft = 6, yright = 0)$y,
    max_temp7_occ50 = approx(prob, temp7, xout = 0.5, yleft = 6, yright = 0)$y,
    max_temp7_occ70 = approx(prob, temp7, xout = 0.7, yleft = 6, yright = 0)$y
  )
cat("done\n")

# df_max_temp7 %>%
#   gather(var, value, -featureid) %>%
#   ggplot(aes(value)) +
#   geom_histogram(nbin = 30) +
#   facet_wrap(~ var)

# predict: max_air_occN --------------------------------------------------
# maximum air temperature increase corresponding with occupancy prob >= N

cat("calculating predictions for max_air_occN scenarios...")
df_max_air <- bind_rows(
  df_current %>%
    mutate(air = 0),
  df_air
) %>%
  arrange(featureid, air) %>%
  group_by(featureid) %>%
  summarise(
    max_air_occ30 = approx(prob, air, xout = 0.3, yleft = 6, yright = 0)$y,
    max_air_occ50 = approx(prob, air, xout = 0.5, yleft = 6, yright = 0)$y,
    max_air_occ70 = approx(prob, air, xout = 0.7, yleft = 6, yright = 0)$y
  )
cat("done\n")

# df_max_air %>%
#   gather(var, value, -featureid) %>%
#   ggplot(aes(value)) +
#   geom_histogram(bins = 30) +
#   facet_wrap(~ var)

# merge -------------------------------------------------------------------

cat("merging prediction scenarios...")
df <- df_current %>%
  bind_rows(
    df_temp7 %>%
      select(-temp7)
  ) %>%
  bind_rows(
    df_air %>%
      select(-air)
  ) %>%
  mutate(scenario = paste0("occ_", scenario)) %>%
  spread(scenario, prob) %>%
  full_join(
    df_max_air,
    by = "featureid"
  ) %>%
  full_join(
    df_max_temp7,
    by = "featureid"
  ) %>%
  select(featureid, occ_current, everything())

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
cat("finished model-predict: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
