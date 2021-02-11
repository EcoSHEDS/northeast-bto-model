# fetch GIS data for plotting

library(tidyverse)
library(sf)
library(USAboundaries)

source("src/functions.R")

config <- load_config()

huc <- read_rds(file.path(config$wd, "data-huc.rds"))

# catchments --------------------------------------------------------------

con <- db_connect()

sf_catchments <- st_read(con, query = "select featureid, geom_pour as geom from truncated_flowlines;")

DBI::dbDisconnect(con)

sf_catchments %>%
  sample_frac(0.01) %>%
  ggplot() +
  geom_sf()

# write_rds(sf_catchments, file.path(config$wd, "gis-catchments.rds"))


# huc10 -------------------------------------------------------------------

# need to manually change this for other machines
sf_huc10_all <- st_read("~/Projects/data/gis/WBD_National_GDB/WBD_National_GDB.gdb/", layer = "WBDHU10")
sf_huc10 <- sf_huc10_all %>%
  filter(HUC10 %in% unique(huc$huc10)) %>%
  st_point_on_surface()

sf_huc10 %>%
  ggplot() +
  geom_sf()

# write_rds(sf_huc10, file.path(config$wd, "gis-huc10.rds"))


# states ------------------------------------------------------------------

sf_states <- us_states() %>%
  filter(
    stusps %in% c('CT', 'DC', 'DE', 'MA', 'MD', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VA', 'VT', 'WV')
  )
sf_states %>%
  ggplot() +
  geom_sf()
# write_rds(sf_states, file.path(config$wd, "gis-states.rds"))


# export ------------------------------------------------------------------

list(
  catchments = sf_catchments,
  huc10 = sf_huc10,
  states = sf_states
) %>%
  write_rds(file.path(config$wd, "gis.rds"))
