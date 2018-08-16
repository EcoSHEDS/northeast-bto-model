# fetch covariates dataset
# -> {wd}/data-covariates.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-covariates:", as.character(start, tz = "US/Eastern"), "\n", sep = "")

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

# fetch covariates --------------------------------------------------------

covariate_ids_full <- c(
  "agriculture", "allonnet", "AreaSqKM", "devel_hi",
  "jan_prcp_mm", "feb_prcp_mm", "mar_prcp_mm", "apr_prcp_mm", "may_prcp_mm", "jun_prcp_mm",
  "jul_prcp_mm", "aug_prcp_mm", "sep_prcp_mm", "oct_prcp_mm", "nov_prcp_mm", "dec_prcp_mm"
)
covariate_ids_riparian200 <- c(
  "forest"
)

cat("fetching full covariates...")
tbl_covariates_full <- tbl(db, "covariates") %>%
  filter(
    variable %in% covariate_ids_full,
    zone == "upstream",
    is.na(riparian_distance_ft)
  )
df_covariates_full_long <- collect(tbl_covariates_full)

df_covariates_full <- df_covariates_full_long %>%
  spread(variable, value) %>%
  select(-zone, -riparian_distance_ft) %>%
  mutate(
    annual_prcp_mm = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm + apr_prcp_mm +
      may_prcp_mm + jun_prcp_mm + jul_prcp_mm + aug_prcp_mm + sep_prcp_mm +
      oct_prcp_mm + nov_prcp_mm + dec_prcp_mm,
    winter_prcp_mm = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm,
    spring_prcp_mm = apr_prcp_mm + may_prcp_mm + jun_prcp_mm,
    summer_prcp_mm = jul_prcp_mm + aug_prcp_mm + sep_prcp_mm,
    fall_prcp_mm = oct_prcp_mm + nov_prcp_mm + dec_prcp_mm
  )
cat("done\n")

cat("fetching riparian (200 ft) covariates...")
tbl_covariates_riparian200 <- tbl(db, "covariates") %>%
  filter(
    variable %in% covariate_ids_riparian200,
    zone == "upstream",
    riparian_distance_ft == 200
  )
df_covariates_riparian200_long <- collect(tbl_covariates_riparian200)

df_covariates_riparian200 <- df_covariates_riparian200_long %>%
  spread(variable, value) %>%
  select(-zone, -riparian_distance_ft)
cat("done\n")

cat("merging full and riparian covariates...")
df_covariates <- df_covariates_full %>%
  left_join(df_covariates_riparian200, by = "featureid")
cat("done\n")


# export ------------------------------------------------------------------

cat("saving covariates dataset to data-covariates.rds...")
saveRDS(df_covariates, file.path(config$wd, "data-covariates.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-covariates:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n", sep = "")
