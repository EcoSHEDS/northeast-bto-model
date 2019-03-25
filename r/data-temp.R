# import stream temp model derived metrics to working directory
# -> {wd}/data-temp.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-temp: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

# fetch covariates from db
db <- src_postgres(
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

# load --------------------------------------------------------------------

cat("loading stream temperature model results (version ", config$stm$version, ")...", sep = "")

df <- tbl(db, "temp_model") %>%
  select(featureid, version, variable, value) %>%
  filter(
    version == config$stm$version,
    variable %in% c("mean_jul_temp", "mean_summer_temp", "n_day_temp_gt_18")
  ) %>%
  collect() %>%
  select(-version) %>%
  spread(variable, value) %>%
  select(featureid, mean_jul_temp, mean_summer_temp, n_day_temp_gt_18)
cat("done\n")

stopifnot(sum(duplicated(df$featureid)) == 0)

# export ------------------------------------------------------------------

cat("saving temp dataset to data-temp.rds...")
saveRDS(df, file.path(config$wd, "data-temp.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-temp: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed, digits = 1), " sec)\n", sep = "")
