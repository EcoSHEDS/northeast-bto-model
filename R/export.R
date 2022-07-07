targets_export <- list(
  tar_target(export_predict_csv, {
    stopifnot("bto_wd does not exist" = dir.exists(bto_wd))
    filename <- file.path(bto_wd, "bto-model.csv")
    predict_pred %>%
      mutate(across(-featureid, signif, digits = 3)) %>%
      write_csv(filename, na = "")
    filename
  }, format = "file"),
  tar_target(export_params_json, {
    stopifnot("bto_wd does not exist" = dir.exists(bto_wd))
    filename <- file.path(bto_wd, "params.json")

    x_std <- predict_inp_variable_std %>%
      nest_by(name)
    x_std <- as.list(set_names(map(x_std$data, as.list), x_std$name))

    x_fixef <- fixef(predict_model)
    names(x_fixef) <- c("intercept", names(x_fixef)[2:length(names(x_fixef))])

    x_ranef <- as_tibble(ranef(predict_model)$huc8, rownames = "huc8") %>%
      rename(intercept = "(Intercept)")

    list(
      std = x_std,
      fixed = as.list(x_fixef),
      random = x_ranef
    ) %>%
      jsonlite::write_json(filename, auto_unbox = TRUE, pretty = TRUE)
    filename
  }),
  tar_target(export_rds, {
    stopifnot("bto_wd does not exist" = dir.exists(bto_wd))
    filename <- file.path(bto_wd, "bto-model.rds")
    list(
      obs = obs_presence,
      inp = predict_inp_all,
      std = predict_inp_variable_std,
      model = predict_model,
      pred = predict_pred
    ) %>%
      write_rds(filename)
    filename
  }, format = "file")
)