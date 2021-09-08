# fetch catchment-huc lookup table
# -> {wd}/data-huc.rds

library(RPostgreSQL)
library(tidyverse)
library(jsonlite)
library(lubridate)

source("src/functions.R")

config <- load_config()


# load --------------------------------------------------------------------

con <- db_connect()
df_db <- DBI::dbGetQuery(con, "SELECT * FROM catchment_huc12;") %>%
  as_tibble()
DBI::dbDisconnect(con)


# process -----------------------------------------------------------------

df <- df_db %>%
  mutate(
    huc4 = str_sub(huc12, 1, 4),
    huc8 = str_sub(huc12, 1, 8),
    huc10 = str_sub(huc12, 1, 10)
  ) %>%
  relocate(huc12, .after = last_col())


# export ------------------------------------------------------------------

write_rds(df, file.path(config$wd, "data-huc.rds"))
