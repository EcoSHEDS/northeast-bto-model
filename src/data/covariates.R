# fetch covariates dataset
# -> {wd}/data-covariates.rds

library(tidyverse)

source("src/functions.R")

config <- load_config()
con <- db_connect()

# fetch: full covariates --------------------------------------------------

covariate_ids_full <- c(
  "agriculture", "allonnet", "AreaSqKM", "devel_hi",
  "jan_prcp_mm", "feb_prcp_mm", "mar_prcp_mm", "apr_prcp_mm", "may_prcp_mm", "jun_prcp_mm",
  "jul_prcp_mm", "aug_prcp_mm", "sep_prcp_mm", "oct_prcp_mm", "nov_prcp_mm", "dec_prcp_mm"
)

tbl_covariates_full <- tbl(con, "covariates") %>%
  filter(
    variable %in% covariate_ids_full,
    zone == "upstream",
    is.na(riparian_distance_ft)
  )
df_covariates_full_db <- collect(tbl_covariates_full)

df_covariates_full <- df_covariates_full_db %>%
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


# fetch: riparian ---------------------------------------------------------
# only calculated within 200 ft riparian buffer
covariate_ids_riparian200 <- c(
  "forest"
)

tbl_covariates_riparian200_db <- tbl(con, "covariates") %>%
  filter(
    variable %in% covariate_ids_riparian200,
    zone == "upstream",
    riparian_distance_ft == 200
  ) %>%
  collect()

df_covariates_riparian200 <- tbl_covariates_riparian200_db %>%
  spread(variable, value) %>%
  select(-zone, -riparian_distance_ft)


# merge -------------------------------------------------------------------

df_covariates <- df_covariates_full %>%
  left_join(df_covariates_riparian200, by = "featureid")

summary(df_covariates)

DBI::dbDisconnect(con)


# export ------------------------------------------------------------------

saveRDS(df_covariates, file.path(config$wd, "data-covariates.rds"))
