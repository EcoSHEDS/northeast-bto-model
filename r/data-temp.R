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

# load --------------------------------------------------------------------

cat("loading temp model derived metrics (", config$temp$path, ")...", sep = "")
df_all <- read_csv(config$temp$path, col_types = cols(
  featureid = col_double(),
  mean_max_temp = col_double(),
  max_max_temp = col_double(),
  mean_jun_temp = col_double(),
  mean_jul_temp = col_double(),
  mean_aug_temp = col_double(),
  mean_summer_temp = col_double(),
  max_temp_30d = col_double(),
  n_day_temp_gt_18 = col_double(),
  n_day_temp_gt_20 = col_double(),
  n_day_temp_gt_22 = col_double(),
  resist = col_double()
))
cat("done\n")

df <- df_all[, c("featureid", "mean_jul_temp", "mean_summer_temp", "n_day_temp_gt_18")]

stopifnot(sum(duplicated(df$featureid)) == 0)

# export ------------------------------------------------------------------

cat("saving temp dataset to data-temp.rds...")
saveRDS(df, file.path(config$wd, "data-temp.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-temp: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed, digits = 1), " sec)\n", sep = "")
