tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv", "sjPlot"))

targets_model <- list(
  tar_target(model_gm, {
    model_data <- filter(inp_split_std, partition == "calib")
    lme4::glmer(
      presence ~ AreaSqKM * summer_prcp_mm +
        mean_jul_temp +
        forest +
        allonnet +
        devel_hi +
        agriculture +
        mean_jul_temp * forest +
        summer_prcp_mm * forest +
        (1 + AreaSqKM + agriculture + summer_prcp_mm + mean_jul_temp | huc10),
      family = binomial(link = "logit"),
      data = model_data,
      control = lme4::glmerControl(optimizer = "bobyqa")
    )
  }),
  tar_target(model_eff_names, {
    setdiff(names(model_gm@frame), c("presence", "huc10"))
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
  tar_target(model_gm_plot_eff_temp_agriculture, {
    plot_model(model_gm, type = "eff", terms = c("mean_jul_temp [all]", "forest [-1, 0, 1]", "agriculture [-1, 0, 1]"))
  }),
  tar_target(model_gm_plot_re, {
    plot_model(model_gm, type = "re")
  }),

  tar_target(model_ranef, {
    as_tibble(lme4::ranef(model_gm)$huc10, rownames = "huc10") %>%
      pivot_longer(-huc10)
  }),
  tar_target(model_ranef_plot, {
    model_ranef %>%
      ggplot(aes(value)) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_histogram() +
      facet_wrap(vars(name)) +
      labs(x = NULL, y = "# HUC10s", title = "Distributions of HUC10 Random Effects") +
      theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))
  }),
  tar_target(model_ranef_map, {
    gis_huc10 %>%
      left_join(
        model_ranef,
        by = "huc10"
      ) %>%
      filter(name != "(Intercept)") %>%
      ggplot() +
      geom_sf(aes(color = value), size = 1) +
      geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
      scale_color_viridis_c() +
      facet_wrap(vars(name))
  }),
  tar_target(model_ranef_map_intercept, {
    gis_huc10 %>%
      left_join(
        model_ranef,
        by = "huc10"
      ) %>%
      filter(name == "(Intercept)") %>%
      ggplot() +
      geom_sf(aes(color = value), size = 1) +
      geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
      scale_color_viridis_c() +
      facet_wrap(vars(name))
  }),

  tar_target(model_pred, create_model_pred(inp_split_std, model_gm)),
  tar_target(model_pred_map, {
    gis_catchments %>%
      inner_join(
        model_pred %>%
          select(
            partition,
            featureid,
            presence,
            pred
          ),
        by = "featureid"
      ) %>%
      ggplot() +
      geom_sf(aes(color = pred), alpha = 0.5, size = 1) +
      geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
      scale_color_viridis_c(limits = c(0, 1)) +
      facet_wrap(vars(partition))
  }),

  tar_target(model_gof, create_model_gof(model_pred)),
  tar_target(model_gof_plot_roc_curves, {
    auc <- set_names(model_gof$auc, model_gof$partition)
    model_gof %>%
      select(partition, roc) %>%
      unnest(roc) %>%
      ggplot(aes(fpr, tpr)) +
      geom_abline(linetype = "dashed") +
      geom_line(aes(color = partition)) +
      scale_color_brewer(palette = "Set1") +
      coord_fixed() +
      labs(
        x = "FPR",
        y = "TPR",
        title = "ROC Curves",
        subtitle = glue("AUC = {round(auc[['calib']], 3)} (calib), {round(auc[['valid']], 3)} (valid)")
      )
  }),
  tar_target(model_gof_map_partition_TFNP, {
    gis_catchments %>%
      inner_join(
        model_gof %>%
          select(
            partition,
            data
          ) %>%
          unnest(data),
        by = "featureid"
      ) %>%
      mutate(
        result = ordered(result, levels = c("TP", "TN", "FP", "FN"))
      ) %>%
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
    state_catchment <- gis_catchments %>%
      filter(featureid %in% model_pred$featureid) %>%
      st_intersection(gis_states) %>%
      as_tibble() %>%
      select(featureid, state_abbr)
    model_gof %>%
      ungroup() %>%
      select(partition, data) %>%
      unnest(data) %>%
      left_join(state_catchment, by = "featureid") %>%
      nest_by(partition, state_abbr) %>%
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
      ) %>%
      unnest(stats)
  }),
  tar_target(model_gof_state_plot, {
    model_gof_state %>%
      ggplot(aes(state_abbr)) +
      geom_point(
        aes(y = accuracy, shape = accuracy_pval <= 0.1, color = partition),
        size = 2
      ) +
      geom_hline(
        data = model_gof %>%
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
