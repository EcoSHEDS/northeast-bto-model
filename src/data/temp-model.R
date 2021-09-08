# import stream temp model derived metrics to working directory
# -> {wd}/data-temp-model.rds

library(tidyverse)
library(glue)

source("src/functions.R")

config <- load_config()
con <- db_connect()


# load --------------------------------------------------------------------

message(glue("Fetching stream temperature model results (v{config$version$tempmodel})"))

temp_model_variables <- c(
  "mean_jul_temp",
  "mean_jul_temp_air2",
  "mean_jul_temp_air4",
  "mean_jul_temp_air6",
  "mean_summer_temp",
  "n_day_temp_gt_18"
)

df_db <- tbl(con, "temp_model") %>%
  select(featureid, version, variable, value) %>%
  filter(
    version == !!config$version$tempmodel,
    variable %in% !!temp_model_variables
  ) %>%
  collect()

df <- df_db %>%
  select(-version) %>%
  spread(variable, value) %>%
  relocate(featureid)

stopifnot(all(!duplicated(df$featureid)))

summary(df)

DBI::dbDisconnect(con)


# export ------------------------------------------------------------------

write_rds(df, file.path(config$wd, "data-temp-model.rds"))
