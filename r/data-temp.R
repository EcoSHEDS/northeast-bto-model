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
df_all <- read.table(config$temp$path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
cat("done\n")

df <- df_all[, c("featureid", "meanJulyTemp", "meanSummerTemp", "meanDays.18")]

stopifnot(sum(duplicated(df$featureid)) == 0)

# export ------------------------------------------------------------------

cat("saving temp dataset to data-temp.rds...")
saveRDS(df, file.path(config$wd, "data-temp.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-temp: ", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n", sep = "")
