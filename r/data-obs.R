# import observational (presence/absence) data to working directory
# -> {wd}/data-obs.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-obs:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading raw observational dataset...")
df <- read.csv(file.path("../data/obs", "regional_occupancy_data.csv"), header = TRUE, stringsAsFactors = FALSE)
df <- df %>%
  mutate(presence = ifelse(catch > 0, 1, catch)) %>%
  select(featureid, presence) %>%
  filter(!is.na(presence))
cat("done\n")

df_dups <- df %>%
  group_by(featureid) %>%
  mutate(
    n = n(),
    mean_presence = mean(presence)
  ) %>%
  filter(n > 1, presence != mean_presence) %>%
  arrange(featureid)
if(nrow(df_dups) > 0) {
  cat(
    "WARNING: ",
    length(unique(df_dups$featureid)), " featureids have duplicate observations that are not equal (",
      paste0(unique(df_dups$featureid), collapse = ", "),
    "). Using the maximum value...\n",
    sep = ""
  )
}

df <- df %>%
  group_by(featureid) %>%
  summarise(presence = max(presence)) %>%
  ungroup()

stopifnot(sum(duplicated(df$featureid)) == 0)

# export ------------------------------------------------------------------

cat("saving obs dataset to data-obs.rds...")
saveRDS(df, file.path(config$wd, "data-obs.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-obs:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")
