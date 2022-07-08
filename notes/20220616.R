library(tidyverse)
library(lme4)
library(targets)

gis_catchments <- tar_read(gis_catchments)
inp_split_std <- tar_read(inp_split_std)

source("R/functions.R")

# alternative models ------------------------------------------------------

model_data <- filter(inp_split_std, partition == "calib")

gm_full <- glmer(
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
  control = glmerControl(optimizer = "bobyqa")
)

gm_temp <- glmer(
  presence ~ mean_jul_temp +
    (1 | huc10),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

gm_temp_ranef <- glmer(
  presence ~ mean_jul_temp +
    (1 + mean_jul_temp | huc10),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

logistic <- glm(presence ~ mean_jul_temp, family = binomial(link = "logit"), data = model_data)

df_gm <- tibble(
  name = c("full", "temp", "temp_ranef", "logistic"),
  model = list(gm_full, gm_temp, gm_temp_ranef, logistic)
) |>
  rowwise() |>
  mutate(
    pred = list(create_model_pred(inp_split_std, model)),
    gof = list(create_model_gof(pred))
  ) |>
  unnest(gof) |>
  select(-pred) |>
  rowwise() |>
  mutate(
    accuracy = list(pivot_wider(enframe(cm$overall)))
  ) |>
  unnest(accuracy) |>
  rowwise() |>
  mutate(
    by_class = list(pivot_wider(enframe(cm$byClass)))
  ) |>
  unnest(by_class) |>
  clean_names()

df_gm |>
  arrange(partition, name) |>
  select(name, partition, auc, accuracy, sensitivity, specificity)

anova(gm_temp_ranef, gm_temp)

df_pred <- df_gm |>
  select(partition, name, data) |>
  unnest(data) |>
  select(name, partition, featureid, presence, pred) |>
  pivot_wider(values_from = "pred") |>
  mutate(`temp_ranef-temp` = temp_ranef - temp)

gis_catchments |>
  inner_join(df_pred, by = "featureid") |>
  filter(abs(`temp_ranef-temp`) > 0.2) |>
  ggplot() +
  geom_sf(aes(color = `temp_ranef-temp`)) +
  geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(vars(partition))

gis_catchments |>
  inner_join(df_pred, by = "featureid") |>
  mutate(across(c(temp, temp_ranef), ~ if_else(.x > 0.5, "T", "F"))) |>
  filter(temp != temp_ranef) |>
  ggplot() +
  geom_sf(aes(color = str_c(temp, temp_ranef))) +
  geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(partition))

gis_catchments |>
  inner_join(df_pred, by = "featureid") |>
  mutate(across(c(full, temp), ~ if_else(.x > 0.5, "T", "F"))) |>
  filter(full != temp) |>
  ggplot() +
  geom_sf(aes(color = str_c(full, temp))) +
  geom_sf(data = rename(gis_states, name_ = name), fill = NA, size = 0.5) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(partition))

df_gm |>
  filter(name == "temp", partition == "calib") |>
  pull(cm)

# roc curves
df_gm |>
  select(name, partition, roc) |>
  unnest(roc) |>
  ggplot(aes(fpr, tpr)) +
  geom_abline(linetype = "dashed") +
  geom_line(aes(color = name)) +
  scale_color_brewer(palette = "Set1") +
  coord_fixed() +
  facet_wrap(vars(partition)) +
  labs(
    x = "FPR",
    y = "TPR",
    title = "ROC Curves",
    subtitle = glue("AUC = {round(auc[['calib']], 3)} (calib), {round(auc[['valid']], 3)} (valid)")
  )

# fixed effects
df_gm |>
  filter(partition == "calib") |>
  rowwise() |>
  mutate(
    plot = list({
      plot_model(model, type = "eff", terms = glue("mean_jul_temp [all]")) +
        ggtitle(name)
    })
  ) |>
  pull(plot) |>
  wrap_plots()

# T/F classes
gis_catchments |>
  inner_join(
    df_gm |>
      select(
        name,
        partition,
        data
      ) |>
      filter(name == "full") |>
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

