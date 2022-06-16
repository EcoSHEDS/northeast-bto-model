tar_option_set(packages = c("tidyverse", "sf", "dotenv"))

targets_huc <- list(
  tar_target(huc_catchment, {
    con <- db_connect()
    x <- DBI::dbGetQuery(con, "SELECT * FROM catchment_huc12;") %>%
      as_tibble()
    DBI::dbDisconnect(con)

    x %>%
      mutate(
        featureid = as.numeric(featureid),
        huc4 = str_sub(huc12, 1, 4),
        huc8 = str_sub(huc12, 1, 8),
        huc10 = str_sub(huc12, 1, 10)
      ) %>%
      relocate(huc12, .after = last_col())
  })
)
