library(tidyverse)
library(janitor)
library(lme4)
library(targets)

source("R/functions.R")

# alternative splits ------------------------------------------------------

inp_split_std <- tar_read(inp_split_std) |>
  mutate(huc6 = str_sub(huc8, 1, 6))

huc8_calib <- inp_split_std |>
  distinct(huc8) |>
  sample_frac(tar_read(inp_split_frac_calib)) |>
  pull(huc8)
inp_huc8 <- inp_split_std |>
  mutate(
    partition = if_else(huc8 %in% huc8_calib, "calib", "valid")
  )
inp_huc8 |>
  tabyl(partition)

featureid_calib <- inp_split_std |>
  distinct(featureid) |>
  sample_frac(tar_read(inp_split_frac_calib)) |>
  pull(featureid)
inp_featureid <- inp_split_std |>
  mutate(
    partition = if_else(featureid %in% featureid_calib, "calib", "valid")
  )
inp_featureid |>
  tabyl(partition)

df_gm <- tibble(
  name = c("huc8", "featureid"),
  inp = list(inp_huc8, inp_featureid)
) |>
  rowwise() |>
  mutate(
    model = list({
      x <- filter(inp, partition == "calib")
      glmer(
        presence ~ mean_jul_temp + (1 | huc8),
        family = binomial(link = "logit"),
        data = x,
        control = glmerControl(optimizer = "bobyqa")
      )
    }),
    pred = list(create_model_pred(inp, model)),
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
  select(name, partition, auc, accuracy, sensitivity, specificity) |>
  pivot_longer(-c(name, partition), names_to = "stat") |>
  ggplot(aes(name, value, color = partition)) +
  geom_line(aes(group = partition)) +
  geom_point() +
  ylim(0.5, 1) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(stat))


# huc levels split by validation --------------------------------------------------------------

df_gm2 <- tibble(
  name = fct_inorder(c("huc4", "huc6", "huc8", "huc10", "huc12")),
) |>
  rowwise() |>
  mutate(
    inp = list(inp_featureid),
    formula = list(as.formula(glue::glue("presence ~ mean_jul_temp + (1 | {name})"))),
    model = list({
      x <- filter(inp, partition == "calib")
      glmer(
        formula,
        family = binomial(link = "logit"),
        data = x,
        control = glmerControl(optimizer = "bobyqa")
      )
    }),
    pred = list(create_model_pred(inp, model)),
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


df_gm2 |>
  arrange(partition, name) |>
  select(name, partition, auc, accuracy, sensitivity, specificity) |>
  pivot_longer(-c(name, partition), names_to = "stat") |>
  ggplot(aes(name, value, color = partition)) +
  geom_line(aes(group = partition)) +
  geom_point() +
  ylim(0.5, 1) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(stat))
