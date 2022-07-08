tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv", "sjPlot", "lme4"))

targets_model <- list(
  tar_target(model_formula, presence ~ mean_jul_temp + (1 | huc8)),
  tar_target(model_gm, {
    model_data <- filter(inp_split, partition == "calib")
    glmer(
      model_formula,
      family = binomial(link = "logit"),
      data = model_data,
      control = glmerControl(optimizer = "bobyqa")
    )
  }),
  tar_target(model_eff_names, {
    setdiff(names(model_gm@frame), c("presence", "huc8"))
  }),
  tar_target(model_gm_plot_est, {
    plot_model(model_gm, sort.est = TRUE, show.values = TRUE, value.offset = 0.3)
  }),
  tar_target(model_gm_plot_eff, {
    p <- map(model_eff_names, function (v) {
      plot_model(model_gm, type = "eff", terms = glue("{v} [all]")) +
        labs(title = v)
    })
    wrap_plots(p)
  }),
  tar_target(model_gm_plot_re, {
    plot_model(model_gm, type = "re", sort.est = "(Intercept)")
  }),

  tar_target(model_ranef, {
    as_tibble(ranef(model_gm)$huc8, rownames = "huc8") |>
      pivot_longer(-huc8)
  }),
  tar_target(model_ranef_plot, {
    model_ranef |>
      ggplot(aes(value)) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_histogram() +
      facet_wrap(vars(name)) +
      labs(x = NULL, y = "# HUC8s", title = "Distributions of HUC8 Random Effects") +
      theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))
  }),
  tar_target(model_ranef_map, {
    gis_huc8_poly |>
      inner_join(
        model_ranef,
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

  tar_target(model_pred, create_model_pred(inp_split, model_gm)),
  tar_target(model_pred_map, {
    gis_catchments |>
      inner_join(
        model_pred |>
          select(
            partition,
            featureid,
            presence,
            pred
          ),
        by = "featureid"
      ) |>
      ggplot() +
      geom_sf(aes(color = pred), alpha = 0.5, size = 1) +
      geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
      scale_color_viridis_c(limits = c(0, 1)) +
      facet_wrap(vars(partition))
  }),

  tar_target(model_gof, create_model_gof(model_pred)),
  tar_target(model_confusion, set_names(model_gof$cm, model_gof$partition)),
  tar_target(model_gof_plot_roc_curves, {
    auc <- set_names(model_gof$auc, model_gof$partition)
    model_gof |>
      select(partition, roc) |>
      unnest(roc) |>
      ggplot(aes(fpr, tpr)) +
      geom_abline(linetype = "dashed") +
      geom_line(aes(color = partition)) +
      scale_color_brewer("Split", palette = "Set1", labels = c(
        "calib" = "Calibration",
        "valid" = "Validation"
      )) +
      coord_fixed() +
      labs(
        x = "False Positive Rate (FPR)",
        y = "True Positive Rate (TPR)",
        title = "ROC Curves",
        subtitle = glue("AUC: Calibration = {round(auc[['calib']], 3)}, Validation = {round(auc[['valid']], 3)}")
      )
  }),
  tar_target(model_gof_map_partition_TFNP, {
    gis_catchments |>
      inner_join(
        model_gof |>
          select(
            partition,
            data
          ) |>
          unnest(data),
        by = "featureid"
      ) |>
      mutate(
        result = ordered(result, levels = c("TP", "TN", "FP", "FN"))
      ) |>
      ggplot() +
      geom_sf(aes(color = result), alpha = 1, size = 0.5) +
      geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
      scale_color_brewer(type = "qual", palette = 2) +
      facet_grid(vars(result), vars(partition), labeller = labeller(result = c(
        TP = "True Positive",
        TN = "True Negative",
        FP = "False Positive",
        FN = "False Negative"
      )))
  }),

  tar_target(model_gof_state, {
    # gof by state
    state_catchment <- gis_catchments |>
      filter(featureid %in% model_pred$featureid) |>
      st_intersection(gis_states) |>
      as_tibble() |>
      select(featureid, state_abbr)
    model_gof |>
      ungroup() |>
      select(partition, data) |>
      unnest(data) |>
      left_join(state_catchment, by = "featureid") |>
      nest_by(partition, state_abbr) |>
      mutate(
        n = nrow(data),
        cm = list({
          caret::confusionMatrix(
            data = factor(1 * (data$pred > 0.5), levels = c("0", "1")),
            reference = factor(data$presence, levels = c("0", "1")),
            mode = "sens_spec",
            positive = "1"
          )
        }),
        accuracy = cm$overall[['Accuracy']],
        accuracy_pval = cm$overall[['AccuracyPValue']],
        stats = list({
          tibble(
            TP = sum(data$result == "TP"),
            TN = sum(data$result == "TN"),
            FP = sum(data$result == "FP"),
            FN = sum(data$result == "FN")
          )
        })
      ) |>
      unnest(stats)
  }),
  tar_target(model_gof_state_plot, {
    model_gof_state |>
      ggplot(aes(state_abbr)) +
      geom_point(
        aes(y = accuracy, shape = accuracy_pval <= 0.1, color = partition),
        size = 2
      ) +
      geom_hline(
        data = model_gof |>
          transmute(
            partition,
            accuracy = map_dbl(cm, ~ .$overall[['Accuracy']])
          ),
        aes(yintercept = accuracy, color = partition, linetype = "Global")
      ) +
      scale_shape_manual("Accuracy > NIR", values = c("TRUE" = 16, "FALSE" = 1), labels = c("TRUE" = "p <= 0.1", "FALSE" = "p > 0.1")) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = "State", y = "Accuracy", linetype = NULL)
  })
)
