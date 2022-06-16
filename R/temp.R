# temperature model predictions

targets_temp <- list(
  tar_target(temp_model_version, Sys.getenv("BTO_TEMP_MODEL"), cue = tar_cue("always")),
  tar_target(temp_model_variables, {
    c(
      "mean_jul_temp",
      "mean_jul_temp_air2",
      "mean_jul_temp_air4",
      "mean_jul_temp_air6",
      "mean_summer_temp",
      "n_day_temp_gt_18"
    )
  }),
  tar_target(temp_model, {
    con <- db_connect()
    x <- tbl(con, "temp_model") %>%
      select(featureid, version, variable, value) %>%
      filter(
        version == temp_model_version,
        variable %in% temp_model_variables
      ) %>%
      collect()
    DBI::dbDisconnect(con)

    x %>%
      select(-version) %>%
      pivot_wider(names_from = "variable") %>%
      relocate(featureid)
  })
)