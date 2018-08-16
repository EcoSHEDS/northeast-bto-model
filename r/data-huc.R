# fetch huc dataset
# -> {wd}/data-huc.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-huc:", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

# fetch covariates from db
db <- src_postgres(
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

# fetch huc ---------------------------------------------------------------

cat("fetching huc dataset...")
tbl_huc <- tbl(db, "catchment_huc12") %>%
  collect()
cat("done\n")

df <- tbl_huc %>%
  mutate(
    huc4 = str_sub(huc12, 1, 4),
    huc8 = str_sub(huc12, 1, 8),
    huc10 = str_sub(huc12, 1, 10)
  )

# export ------------------------------------------------------------------

cat("saving huc dataset to data-huc.rds...")
saveRDS(df, file.path(config$wd, "data-huc.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-huc:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n", sep = "")
