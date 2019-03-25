# process MA DFW dataset
# <- ../data/obs/madfw-ebt-raw.csv
# -> ../data/obs/madfw-ebt.csv

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-obs-madfw: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

db <- src_postgres(
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

# load raw data (presence only)
# source: 20181206 - MA Wildlife Data/MassWildlife coldwater spp/Brook_Trout_nat_prod.shp
cat("loading raw data from ../data/obs/madfw-ebt-raw.csv...")
df <- read_csv("../data/obs/madfw-ebt-raw.csv", col_types = cols(
  .default = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  year = col_double()
)) %>%
  select(latitude, longitude, year)
cat("done\n")

find_featureid <- function (latitude, longitude) {
  x <- dbGetQuery(
    db$con,
    "select featureid::int from catchments where st_contains(geom, st_setsrid(st_makepoint($1, $2), 4326))",
    param = list(longitude, latitude)
  )
  stopifnot(nrow(x) == 1)
  x$featureid
}
# find_featureid(df$latitude[1], df$longitude[1])

# find featureid for each row
cat("assigning featureids from lat/lon...")
df <- df %>%
  mutate(
    featureid = map2_int(latitude, longitude, ~ find_featureid(.x, .y))
  )
cat("done\n")

# export ------------------------------------------------------------------

cat("saving cleaned madfw dataset to ../data/obs/madfw-ebt.csv...")
write_csv(df, "../data/obs/madfw-ebt.csv")
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-obs-madfw: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed, digits = 1), " sec)\n", sep = "")
