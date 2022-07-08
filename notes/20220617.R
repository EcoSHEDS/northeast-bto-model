library(tidyverse)
library(lme4)
library(targets)

gis_catchments <- tar_read(gis_catchments)
inp_split_std <- tar_read(inp_split_std) |>
  mutate(huc6 = str_sub(huc12, 1, 6))

source("R/functions.R")

# alternative huc levels ------------------------------------------------------

model_data <- filter(inp_split_std, partition == "calib")

gm_huc4 <- glmer(
  presence ~ mean_jul_temp + (1 | huc4),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

gm_huc6 <- glmer(
  presence ~ mean_jul_temp + (1 | huc6),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

gm_huc8 <- glmer(
  presence ~ mean_jul_temp + (1 | huc8),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

gm_huc10 <- glmer(
  presence ~ mean_jul_temp + (1 | huc10),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

gm_huc12 <- glmer(
  presence ~ mean_jul_temp + (1 | huc12),
  family = binomial(link = "logit"),
  data = model_data,
  control = glmerControl(optimizer = "bobyqa")
)

df_gm <- tibble(
  name = c("huc4", "huc6", "huc8", "huc10", "huc12"),
  model = list(gm_huc4, gm_huc6, gm_huc8, gm_huc10, gm_huc12)
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
  mutate(name = factor(name, levels = str_c("huc", c(4, 6, 8, 10, 12)))) |>
  arrange(partition, name) |>
  select(name, partition, auc, accuracy, sensitivity, specificity) |>
  pivot_longer(-c(name, partition), names_to = "stat") |>
  ggplot(aes(name, value, color = partition)) +
  geom_line(aes(group = partition)) +
  geom_point() +
  ylim(0.5, 1) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(stat))


