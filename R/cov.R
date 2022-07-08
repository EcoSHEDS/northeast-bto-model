# covariates

tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv"))

targets_cov <- list(
  tar_target(cov_full_ids, {
    c(
      "agriculture", "allonnet", "AreaSqKM", "devel_hi",
      "jan_prcp_mm", "feb_prcp_mm", "mar_prcp_mm", "apr_prcp_mm", "may_prcp_mm", "jun_prcp_mm",
      "jul_prcp_mm", "aug_prcp_mm", "sep_prcp_mm", "oct_prcp_mm", "nov_prcp_mm", "dec_prcp_mm"
    )
  }),
  tar_target(cov_full, {
    # full basin characteristics
    con <- db_connect()
    x <- tbl(con, "covariates") |>
      filter(
        variable %in% cov_full_ids,
        zone == "upstream",
        is.na(riparian_distance_ft)
      ) |>
      collect()
    DBI::dbDisconnect(con)

    x |>
      pivot_wider(names_from = "variable") |>
      select(-zone, -riparian_distance_ft) |>
      mutate(
        featureid = as.numeric(featureid),
        annual_prcp_mm = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm + apr_prcp_mm +
          may_prcp_mm + jun_prcp_mm + jul_prcp_mm + aug_prcp_mm + sep_prcp_mm +
          oct_prcp_mm + nov_prcp_mm + dec_prcp_mm,
        winter_prcp_mm = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm,
        spring_prcp_mm = apr_prcp_mm + may_prcp_mm + jun_prcp_mm,
        summer_prcp_mm = jul_prcp_mm + aug_prcp_mm + sep_prcp_mm,
        fall_prcp_mm = oct_prcp_mm + nov_prcp_mm + dec_prcp_mm
      )
  }),
  tar_target(cov_riparian_ids, "forest"),
  tar_target(cov_riparian, {
    # riparian characteristics
    con <- db_connect()
    x <- tbl(con, "covariates") |>
      filter(
        variable %in% cov_riparian_ids,
        zone == "upstream",
        riparian_distance_ft == 200
      ) |>
      collect()
    DBI::dbDisconnect(con)

    x |>
      pivot_wider(names_from = "variable") |>
      mutate(featureid = as.numeric(featureid)) |>
      select(-zone, -riparian_distance_ft)
  }),
  tar_target(cov_all, {
    left_join(cov_full, cov_riparian, by = "featureid")
  })
)
