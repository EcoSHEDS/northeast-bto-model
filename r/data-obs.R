# import observational (presence/absence) data to working directory
# -> {wd}/data-obs.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-obs: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading raw regional observation dataset...")
df_regional <- read_csv(file.path("../data/obs", "regional_occupancy_data.csv"), col_types = cols(
  featureid = col_double(),
  species = col_character(),
  catch = col_double(),
  year_min = col_double(),
  year_max = col_double()
))
df_regional <- df_regional %>%
  mutate(presence = ifelse(catch > 0, 1, catch)) %>%
  select(featureid, presence) %>%
  filter(!is.na(presence))
cat("done\n")

df_regional_dups <- df_regional %>%
  group_by(featureid) %>%
  mutate(
    n = n(),
    mean_presence = mean(presence)
  ) %>%
  filter(n > 1, presence != mean_presence) %>%
  arrange(featureid)
if(nrow(df_regional_dups) > 0) {
  cat(
    "WARNING: ",
    length(unique(df_regional_dups$featureid)), " featureids have duplicate observations that are not equal (",
      paste0(unique(df_regional_dups$featureid), collapse = ", "),
    "). Using the maximum value...\n",
    sep = ""
  )
}


# load madfw --------------------------------------------------------------

cat("loading raw MA DFW observation dataset...")
df_madfw <- read_csv("../data/obs/madfw-ebt.csv", col_types = cols(
  latitude = col_double(),
  longitude = col_double(),
  year = col_double(),
  featureid = col_double()
)) %>%
  select(featureid) %>%
  distinct() %>%
  mutate(
    presence = 1
  )
cat("done\n")

# merging -----------------------------------------------------------------

cat("loading raw observation datasets...")
df <- bind_rows(
    df_regional,
    df_madfw
  )%>%
  group_by(featureid) %>%
  summarise(presence = max(presence)) %>%
  ungroup()

stopifnot(sum(duplicated(df$featureid)) == 0)
cat("done\n")

# export ------------------------------------------------------------------

cat("saving obs dataset to data-obs.rds...")
saveRDS(df, file.path(config$wd, "data-obs.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-obs: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed, digits = 1), " sec)\n", sep = "")
