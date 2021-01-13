# process MA DFW dataset by adding featureid
# <- data/obs/madfw-ebt-raw.csv
# -> data/obs/madfw-ebt.csv

library(tidyverse)

source("src/functions.R")


# load --------------------------------------------------------------------

# load raw data (presence only)
# source: 20181206 - MA Wildlife Data/MassWildlife coldwater spp/Brook_Trout_nat_prod.shp
df_raw <- read_csv("data/obs/madfw-ebt-raw.csv", col_types = cols(
  .default = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  year = col_double()
)) %>%
  select(latitude, longitude, year)


# process -----------------------------------------------------------------

find_featureid <- function (latitude, longitude) {
  # helper function to find the catchment for a given lat/lon
  x <- DBI::dbGetQuery(
    con,
    "select featureid::int from catchments where st_contains(geom, st_setsrid(st_makepoint($1, $2), 4326))",
    param = list(longitude, latitude)
  )
  stopifnot(nrow(x) == 1)
  x$featureid
}
# find_featureid(df$latitude[1], df$longitude[1])

# connect to database
con <- db_connect()

# find featureid for each row
df <- df_raw %>%
  rowwise() %>%
  mutate(
    featureid = find_featureid(latitude, longitude)
  ) %>%
  ungroup()

stopifnot(all(!is.na(df$featureid)))

# disconnect
DBI::dbDisconnect(con)


# export ------------------------------------------------------------------

write_csv(df, "data/obs/madfw-ebt.csv")
