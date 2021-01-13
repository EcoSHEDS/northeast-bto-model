# export predictions to csv
# <- model-predict.rds
# -> csv/bto-model-v{VERSION}.csv

start <- lubridate::now(tzone = "US/Eastern")
cat("starting export-csv:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading predictions...")
df <- readRDS(file.path(config$wd, "model-predict.rds")) %>%
  select(-huc12)
cat("done\n")

# export ------------------------------------------------------------------

cat("dataset structure:\n")
str(df)
cat("\n")

cat("rounding values...")
df_round <- df %>%
  mutate_at(vars(-featureid), signif, digits = 3)
cat("\n")

if (!dir.exists(file.path(config$wd, "csv"))) {
  cat("creating csv directory...")
  dir.create(file.path(config$wd, "csv"))
  cat("done\n")
}

fname <- paste0("sheds-bto-model-v", config$version$bto, ".csv")
cat("saving to csv/", fname, "...", sep = "")
write_csv(df_round, file.path(config$wd, "csv", fname), na = "")
cat("done\n")

# done --------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
cat("finished export-csv:", as.character(end, tz = "US/Eastern"), "\n")
