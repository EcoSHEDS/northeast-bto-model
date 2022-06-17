tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv", "sjPlot", "lme4"))

targets_predict <- list(
  tar_target(predict_inp_all, {
    obs_presence %>%
      left_join(huc_catchment, by = "featureid") %>%
      left_join(cov_all, by = "featureid") %>%
      left_join(temp_model, by = "featureid") %>%
      mutate_at(vars(starts_with("huc")), as.factor)
  }),
  tar_target(predict_inp_complete, {
    predict_inp_all %>%
      filter(complete.cases(.))
  }),
  tar_target(predict_inp_variable_std, {
    predict_inp_complete %>%
      pivot_longer(-c(featureid, presence, starts_with("huc"))) %>%
      group_by(name) %>%
      summarize(
        mean = mean(value),
        sd = sd(value),
        .groups = "drop"
      )
  }),
  tar_target(predict_inp_std, {
    predict_inp_complete %>%
      pivot_longer(-c(featureid, presence, starts_with("huc"))) %>%
      left_join(predict_inp_variable_std, by = "name") %>%
      mutate(
        value = (value - mean) / sd
      ) %>%
      select(-mean, -sd) %>%
      pivot_wider() %>%
      arrange(huc8, featureid) %>%
      mutate(partition = "calib", .before = 1)
  }),
  tar_target(predict_model, {
    glmer(
      model_formula,
      family = binomial(link = "logit"),
      data = predict_inp_std,
      control = glmerControl(optimizer = "bobyqa")
    )
  }),

  tar_target(predict_model_eff_names, {
    setdiff(names(predict_model@frame), c("presence", "huc8"))
  }),
  tar_target(predict_model_gm_plot_est, {
    plot_model(predict_model, sort.est = TRUE, show.values = TRUE, value.offset = 0.3)
  }),
  tar_target(predict_model_plot_eff, {
    p <- map(predict_model_eff_names, function (v) {
      plot_model(predict_model, type = "eff", terms = glue("{v} [all]")) +
        labs(title = v)
    })
    wrap_plots(p)
  }),

  tar_target(predict_model_pred, create_model_pred(predict_inp_std, predict_model)),
  tar_target(predict_model_gof, create_model_gof(predict_model_pred)),

  tar_target(predict_data, {
    x <- huc_catchment %>%
      select(featureid, huc8) %>%
      left_join(cov_all, by = "featureid") %>%
      left_join(temp_model, by = "featureid") %>%
      mutate_at(vars(starts_with("huc")), as.factor) %>%
      filter(
        complete.cases(.),
        AreaSqKM <= 200,
        n_day_temp_gt_18 < 300
      )
    x_air <- x %>%
      select(featureid, starts_with("mean_jul_temp")) %>%
      rename(mean_jul_temp_air0 = mean_jul_temp) %>%
      pivot_longer(-featureid, values_to = "mean_jul_temp") %>%
      mutate(
        air = as.numeric(str_sub(name, 18, 19))
      ) %>%
      select(air, featureid, mean_jul_temp)
    x %>%
      select(-starts_with("mean_jul_temp"), -mean_summer_temp, -n_day_temp_gt_18) %>%
      full_join(
        x_air,
        by = "featureid"
      ) %>%
      relocate(air)
  }),
  tar_target(predict_data_std, {
    predict_data %>%
      pivot_longer(-c(air, featureid, starts_with("huc"))) %>%
      left_join(predict_inp_variable_std, by = "name") %>%
      mutate(
        value = (value - mean) / sd
      ) %>%
      select(-mean, -sd) %>%
      pivot_wider()
  }),
  tar_target(predict_prob, {
    predict_data_std %>%
      mutate(
        prob = boot::inv.logit(predict(predict_model, predict_data_std, allow.new.levels = TRUE))
      ) %>%
      select(air, featureid, prob)
  }),
  tar_target(predict_max, {
    predict_prob %>%
      arrange(featureid, air) %>%
      nest_by(featureid) %>%
      mutate(
        max_air_occ30 = approx(data$prob, data$air, xout = 0.3, yleft = 6, yright = 0)$y,
        max_air_occ50 = approx(data$prob, data$air, xout = 0.5, yleft = 6, yright = 0)$y,
        max_air_occ70 = approx(data$prob, data$air, xout = 0.7, yleft = 6, yright = 0)$y
      ) %>%
      select(-data) %>%
      ungroup()
  }),
  tar_target(predict_pred, {
    huc_catchment %>%
      select(featureid) %>%
      left_join(
        predict_prob %>%
          mutate(
            name = case_when(
              air == 0 ~ "occ_current",
              TRUE ~ str_c("occ_air_", air)
            )
          ) %>%
          select(-air) %>%
          pivot_wider(values_from = "prob"),
        by = "featureid"
      ) %>%
      full_join(predict_max, by = "featureid")
  }),
  tar_target(predict_pred_plot_hist, {
    predict_pred %>%
      pivot_longer(-featureid, values_drop_na = TRUE) %>%
      ggplot(aes(value)) +
      geom_histogram() +
      facet_wrap(vars(name), scales = "free")
  }),
  tar_target(predict_pred_map_prob, {
    gis_catchments %>%
      left_join(select(predict_pred, featureid, occ_current), by = "featureid") %>%
      filter(!is.na(occ_current)) %>%
      sample_frac(0.1) %>%
      ggplot() +
      geom_sf(aes(color = occ_current), size = 1) +
      scale_color_viridis_c(limits = c(0, 1)) +
      labs(title = "Current Occupancy Probability")
  })
)