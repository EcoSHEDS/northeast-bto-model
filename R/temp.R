# temperature model predictions

targets_temp <- list(
  tar_target(temp_model_file, Sys.getenv("BTO_TEMP_MODEL_FILE"), format = "file"),
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
    read_csv(temp_model_file, col_types = cols(.default = col_double(), featureid = col_double())) |>
      select(featureid, all_of(temp_model_variables))
  }),
  tar_target(temp_model_catchments, {
    gis_catchments |>
      left_join(temp_model, by = "featureid")
  }),
  tar_target(temp_model_catchments_map, {
    temp_model_catchments |>
      filter(!is.na(mean_jul_temp)) |>
      sample_frac(0.1) |>
      ggplot() +
      geom_sf(aes(color = mean_jul_temp), size = 0.25) +
      geom_sf(data = select(gis_states, -name), fill = NA, size = 0.5, color = "grey10") +
      scale_color_viridis_c("Mean July Temp\n(degC)") +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
      )
  })
)
