# export predictions to database
# <- model-predict.rds
# -> db[bto_model]

rm(list=ls())

start <- lubridate::now(tzone = "US/Eastern")
cat("starting export-db:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

source("functions.R")

config <- load_config()


# load --------------------------------------------------------------------

cat("loading predictions...")
df_wide <- readRDS(file.path(config$wd, "model-predict.rds"))

df <- df_wide %>%
  select(-huc12) %>%
  gather(variable, value, -featureid) %>%
  mutate(version = config$version)
cat("done\n")

# save --------------------------------------------------------------------

cat("connecting to db (host = ", config$db$host, ", dbname = ", config$db$dbname, ")...", sep = "")
db <- src_postgres(host = config$db$host, dbname = config$db$dbname, user = config$db$user)
cat("done\n")

cat("saving to database...")
done <- db_insert_into(db$con, "bto_model", df)
cat("done\n")

# done --------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
cat("finished export-db:", as.character(end, tz = "US/Eastern"), "\n")
