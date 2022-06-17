targets_export <- list(
  tar_target(export_csv, {
    stopifnot("bto_wd does not exist" = dir.exists(bto_wd))
    filename <- file.path(bto_wd, glue("bto-model-v", bto_version, ".csv"))
    predict_pred %>%
      mutate(across(-featureid, signif, digits = 3)) %>%
      write_csv(filename, na = "")
    filename
  }, format = "file"),
  tar_target(export_rds, {
    stopifnot("bto_wd does not exist" = dir.exists(bto_wd))
    filename <- file.path(bto_wd, glue("bto-model-v", bto_version, ".rds"))
    list(
      obs = obs_presence,
      inp = predict_inp_all,
      model = predict_model,
      pred = predict_pred
    ) %>%
      write_rds(filename)
    filename
  }, format = "file")
)