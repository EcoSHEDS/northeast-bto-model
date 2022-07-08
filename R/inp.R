# model inputs

targets_inp <- list(
  tar_target(inp_all, {
    obs_presence |>
      left_join(huc_catchment, by = "featureid") |>
      left_join(cov_all, by = "featureid") |>
      left_join(temp_model, by = "featureid") |>
      mutate_at(vars(starts_with("huc")), as.factor)
  }),
  tar_target(inp_complete, {
    inp_all |>
      filter(!is.na(mean_jul_temp))
  }),
  tar_target(inp_split_frac_calib, 0.8),
  tar_target(inp_split, {
    # split by huc10

    huc10s <- unique(inp_complete$huc10)
    n_calib_huc10 <- floor(length(huc10s) * inp_split_frac_calib)

    set.seed(20220708)
    calib_huc10 <- sample(huc10s, n_calib_huc10, replace = FALSE)
    valid_huc10 <- setdiff(huc10s, calib_huc10)

    inp_complete |>
      mutate(
        partition = case_when(
          huc10 %in% calib_huc10 ~ "calib",
          huc10 %in% valid_huc10 ~ "valid",
          TRUE ~ NA_character_
        ),
        .before = 1
      )
  }),

  tar_target(inp_select_cov, {
    c(
      "AreaSqKM",
      "agriculture",
      "devel_hi",
      "forest",
      "allonnet",
      "summer_prcp_mm",
      "mean_jul_temp"
    )
  }),
  tar_target(inp_map_split, {
    gis_catchments |>
      inner_join(inp_split, by = "featureid") |>
      ggplot() +
      geom_sf(aes(color = factor(presence)), size = 0.5) +
      geom_sf(data = gis_states, fill = NA, size = 0.5) +
      scale_color_manual(
        NULL,
        values = c("0" = "orangered", "1" = "deepskyblue"),
        labels = c("0" = "Absence", "1" = "Presence")
      ) +
      facet_wrap(vars(partition), nrow = 1, labeller = labeller(
        partition = c(
          calib = "Calibration",
          valid = "Validation"
        )
      )) +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()
      )
  }),
  tar_target(inp_plot_hist_cov, {
    inp_split |>
      select(partition, featureid, all_of(inp_select_cov)) |>
      pivot_longer(-c(partition, featureid)) |>
      ggplot(aes(value)) +
      geom_histogram() +
      facet_grid(vars(partition), vars(name), scales = "free") +
      labs(x = NULL, y = "# Catchments")
  }),
  tar_target(inp_map_cov, {
    p <- map(c(inp_select_cov, "presence"), function (v) {
      gis_catchments |>
        inner_join(inp_split, by = "featureid") |>
        ggplot() +
        geom_sf(aes_string(color = v), size = 0.5) +
        geom_sf(data = gis_states, fill = NA, size = 0.5) +
        scale_color_viridis_c() +
        labs(title = v)
    })
    wrap_plots(p)
  }),
  tar_target(inp_plot_cov_splot, {
    inp_split |>
      select(presence, all_of(inp_select_cov)) |>
      pivot_longer(-presence) |>
      ggplot(aes(value, presence)) +
      geom_jitter(width = 0, height = 0.01, alpha = 0.1) +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      facet_wrap(vars(name), scales = "free_x", strip.position = "bottom") +
      labs(x = NULL, y = "prob(presence)") +
      theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))
  }),
  tar_target(inp_plot_cov_box, {
    inp_split |>
      select(presence, all_of(inp_select_cov)) |>
      pivot_longer(-presence) |>
      ggplot(aes(factor(presence), value)) +
      geom_boxplot() +
      stat_summary(fun = mean, color = "red") +
      facet_wrap(vars(name), scales = "free_y", strip.position = "left") +
      labs(x = "Presence/Absence", y = NULL) +
      theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))
  }),
  tar_target(inp_plot_cov_pairs_presence, {
    inp_split |>
      select(all_of(inp_select_cov), presence) |>
      GGally::ggpairs(columns = inp_select_cov, mapping = aes(alpha = 0.5, color = factor(presence))) +
      scale_fill_manual(
        NULL,
        values = c("0" = "orangered", "1" = "deepskyblue"),
        labels = c("0" = "Absence", "1" = "Presence")
      )
  }),
  tar_target(inp_plot_cov_pairs, {
    inp_split |>
      select(all_of(inp_select_cov)) |>
      GGally::ggpairs(mapping = aes(alpha = 0.25))
  })
)
