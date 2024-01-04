tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv", "sjPlot", "lme4"))

targets_predict <- list(
  tar_target(predict_inp_all, {
    obs_presence |>
      left_join(huc_catchment, by = "featureid") |>
      # left_join(cov_all, by = "featureid") |>
      left_join(temp_model, by = "featureid") |>
      mutate_at(vars(starts_with("huc")), as.factor) |>
      select(featureid, presence, starts_with("huc"), mean_jul_temp)
  }),
  tar_target(predict_inp, {
    predict_inp_all |>
      filter(!is.na(mean_jul_temp), !is.na(huc8)) |>
      mutate(partition = "pred")
  }),
  tar_target(predict_model, {
    glmer(
      model_formula,
      family = binomial(link = "logit"),
      data = predict_inp,
      control = glmerControl(optimizer = "bobyqa")
    )
  }),

  tar_target(predict_model_eff_names, {
    setdiff(names(predict_model@frame), c("presence", "huc8"))
  }),
  tar_target(predict_model_gm_plot_est, {
    plot_model(predict_model, sort.est = TRUE, show.values = TRUE, value.offset = 0.3)
  }),
  # tar_target(predict_model_plot_eff, {
  #   x <- predict_inp
  #   plots <- list()
  #   for (name in predict_model_eff_names) {
  #     p <- plot_model(predict_model, type = "eff", terms = glue("{name} [all]")) +
  #       labs(title = name)
  #     plots <- c(plots, p)
  #   }
  #   # p <- map(predict_model_eff_names, function (v) {
  #   #   plot_model(predict_model, type = "eff", terms = glue("{v} [all]")) +
  #   #     labs(title = v)
  #   # })
  #   wrap_plots(p)
  # }),

  tar_target(predict_model_pred, create_model_pred(predict_inp, predict_model)),
  tar_target(predict_model_gof, create_model_gof(predict_model_pred)),
  tar_target(predict_model_ranef, {
    as_tibble(ranef(predict_model)$huc8, rownames = "huc8") |>
      pivot_longer(-huc8)
  }),
  tar_target(predict_model_ranef_map, {
    gis_huc8_poly |>
      inner_join(
        predict_model_ranef,
        by = "huc8"
      ) |>
      st_transform("EPSG:4326") |>
      ggplot() +
      geom_sf(aes(fill = value)) +
      geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
      scale_fill_viridis_c() +
      facet_wrap(vars(name)) +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
      )
  }),
  tar_target(predict_data, {
    huc_catchment |>
      select(featureid, huc8) |>
      left_join(cov_all, by = "featureid") |>
      left_join(temp_model, by = "featureid") |>
      filter(
        AreaSqKM <= 200,
        n_day_temp_gt_18 < 300
      ) |>
      select(featureid, huc8, starts_with("mean_jul_temp")) |>
      rename(mean_jul_temp_air0 = mean_jul_temp) |>
      pivot_longer(-c(featureid, huc8), values_to = "mean_jul_temp") |>
      mutate(
        air = as.numeric(str_sub(name, 18, 19))
      ) |>
      select(air, featureid, huc8, mean_jul_temp)
  }),
  tar_target(predict_prob, {
    predict_data |>
      mutate(
        prob = boot::inv.logit(predict(predict_model, predict_data, allow.new.levels = TRUE))
      ) |>
      select(air, featureid, prob)
  }),
  tar_target(predict_max, {
    predict_prob |>
      arrange(featureid, air) |>
      nest_by(featureid) |>
      mutate(
        max_air_occ30 = approx(data$prob, data$air, xout = 0.3, yleft = 6, yright = 0)$y,
        max_air_occ50 = approx(data$prob, data$air, xout = 0.5, yleft = 6, yright = 0)$y,
        max_air_occ70 = approx(data$prob, data$air, xout = 0.7, yleft = 6, yright = 0)$y
      ) |>
      select(-data) |>
      ungroup()
  }),
  tar_target(predict_pred, {
    huc_catchment |>
      select(featureid) |>
      left_join(
        predict_prob |>
          mutate(
            name = case_when(
              air == 0 ~ "occ_current",
              TRUE ~ str_c("occ_air_", air)
            )
          ) |>
          select(-air) |>
          pivot_wider(values_from = "prob"),
        by = "featureid"
      ) |>
      full_join(predict_max, by = "featureid")
  }),
  tar_target(predict_pred_plot_hist, {
    predict_pred |>
      pivot_longer(-featureid, values_drop_na = TRUE) |>
      ggplot(aes(value)) +
      geom_histogram() +
      facet_wrap(vars(name), scales = "free")
  }),
  tar_target(predict_pred_map_prob, {
    x <- predict_pred |>
      filter(!is.na(occ_current)) |>
      select(featureid, starts_with("occ_")) |>
      pivot_longer(-featureid) |>
      mutate(name = fct_inorder(name))
    gis_catchments |>
      sample_frac(0.1) |>
      inner_join(x, by = "featureid") |>
      ggplot() +
      geom_sf(aes(color = value), size = 0.25) +
      geom_sf(data = select(gis_states, -name), fill = NA, size = 0.5, color = "grey10") +
      scale_color_viridis_c("Occupancy\nProbability", limits = c(0, 1)) +
      facet_wrap(vars(name), ncol = 2, labeller = labeller(
        name = c(
          "occ_current" = "Historical",
          "occ_air_2" = "+2 degC Air Temp",
          "occ_air_4" = "+4 degC Air Temp",
          "occ_air_6" = "+6 degC Air Temp"
        )
      )) +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
      )
  }),
  tar_target(predict_pred_map_max, {
    x <- predict_pred |>
      filter(!is.na(occ_current)) |>
      select(featureid, starts_with("max_")) |>
      pivot_longer(-featureid) |>
      mutate(name = fct_inorder(name))
    gis_catchments |>
      sample_frac(0.1) |>
      inner_join(x, by = "featureid") |>
      ggplot() +
      geom_sf(aes(color = value), size = 0.25) +
      geom_sf(data = select(gis_states, -name), fill = NA, size = 0.5, color = "grey10") +
      scale_color_viridis_c("Max Air Temp\nIncrease (degC)", limits = c(0, 6)) +
      facet_wrap(vars(name), ncol = 2, labeller = labeller(
        name = c(
          "max_air_occ30" = "To Achieve 30% Occupancy",
          "max_air_occ50" = "To Achieve 50% Occupancy",
          "max_air_occ70" = "To Achieve 70% Occupancy"
        )
      )) +
      labs(caption = "Note: Max Air Temp. Increases limited to a maximum value of 6.") +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
      )
  }),
  tar_target(predict_model_confusion, predict_model_gof$cm[[1]]),
  tar_target(predict_model_gof_compare, {
    x_predict <- tibble(
      name = "pred",
      stat = c("Accuracy", names(predict_model_confusion$byClass)),
      value = c(predict_model_confusion$overall["Accuracy"], predict_model_confusion$byClass)
    )
    map_df(c("calib", "valid"), function (split) {
      x <- model_confusion[[split]]
      tibble(
        name = split,
        stat = c("Accuracy", names(x$byClass)),
        value = c(x$overall["Accuracy"], x$byClass)
      )
    }) |>
      bind_rows(
        x_predict
      ) |>
      mutate(value = round(value, 3)) |>
      pivot_wider()
  })
)
