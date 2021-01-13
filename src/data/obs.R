# import observational (presence/absence) data to working directory
# -> {wd}/data-obs.rds

library(tidyverse)
library(glue)
library(janitor)
library(sf)

source("src/functions.R")

config <- load_config()


# load: gis ---------------------------------------------------------------

con <- db_connect()

sf_catchment_points <- st_read(con, query = "select featureid, geom_pour as geom from truncated_flowlines;")

DBI::dbDisconnect(con)


# load: regional ----------------------------------------------------------

df_regional <- read_csv("data/obs/regional_occupancy_data.csv", col_types = cols(
  featureid = col_double(),
  species = col_character(),
  catch = col_double(),
  year_min = col_double(),
  year_max = col_double()
)) %>%
  mutate(presence = ifelse(catch > 0, 1, catch)) %>%
  select(featureid, presence) %>%
  filter(!is.na(presence))

tabyl(df_regional, presence)

sf_regional_points <- sf_catchment_points %>%
  inner_join(df_regional, by = "featureid")

sf_regional_points %>%
  ggplot() +
  geom_sf(aes(color = factor(presence))) +
  facet_wrap(vars(presence))


# load: madfw -------------------------------------------------------------

df_madfw <- read_csv("data/obs/madfw-ebt.csv", col_types = cols(
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

tabyl(df_madfw, presence)

sf_madfw_points <- sf_catchment_points %>%
  inner_join(df_madfw, by = "featureid")

sf_madfw_points %>%
  ggplot() +
  geom_sf(aes(color = factor(presence))) +
  facet_wrap(vars(presence))


# merge -------------------------------------------------------------------

df_merge <- bind_rows(
    df_regional,
    df_madfw
  )

merge_dups <- df_merge %>%
  distinct(featureid, presence) %>%
  filter(duplicated(featureid)) %>%
  pull(featureid)

if(length(merge_dups) > 0) {
  warning(
    glue("{length(merge_dups)} featureids have duplicate observations between regional and MADFW datasets that are not equal ({str_c(merge_dups, collapse = ', ')}). Setting all to presence=1...")
  )
}

df <- df_merge %>%
  group_by(featureid) %>%
  summarise(presence = max(presence))

stopifnot(sum(duplicated(df$featureid)) == 0)


# export ------------------------------------------------------------------

saveRDS(df, file.path(config$wd, "data-obs.rds"))
