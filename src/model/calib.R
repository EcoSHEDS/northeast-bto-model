# fit model to calibration dataset
# <- {wd}/model-input.rds
# -> {wd}/model-calib.rds

library(lme4)
library(AUC)
library(boot)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(sf)
library(sjPlot)
library(caret)

source("src/functions.R")

config <- load_config()

# load --------------------------------------------------------------------

gis <- read_rds(file.path(config$wd, "gis.rds"))
inp <- read_rds(file.path(config$wd, "model-input.rds"))

# calibration dataset with standardized covariates
df <- inp$data_std %>%
  filter(partition == "calib")


# dataset summary ---------------------------------------------------------

df %>%
  group_by(huc12) %>%
  summarise(
    n = n(),
    presence = mean(presence)
  ) %>%
  arrange(desc(n))


# fit model ---------------------------------------------------------------

glmm <- glmer(
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
  data = df,
  control = glmerControl(optimizer = "bobyqa")
)

summary(glmm)


# diagnostics -------------------------------------------------------------

covariates <- c(
  "AreaSqKM",
  "agriculture",
  "devel_hi",
  "forest",
  "allonnet",
  "summer_prcp_mm",
  "mean_jul_temp"
)

# fixed effects
plot_model(glmm, show.values = TRUE, value.offset = 0.3)
plot_model(glmm, sort.est = TRUE)

p <- map(covariates, function (v) {
  plot_model(glmm, type = "eff", terms = glue("{v} [all]")) +
    labs(title = v)
})

cowplot::plot_grid(plotlist = p)
plot_model(glmm, type = "eff", terms = c("mean_jul_temp [all]", "forest [-1, 0, 1]", "agriculture [-1, 0, 1]"))

# random effects
plot_model(glmm, type = "re")
df_ranef <- as_tibble(ranef(glmm)$huc10, rownames = "HUC10") %>%
  pivot_longer(-HUC10)

df_ranef %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(vars(name)) +
  labs(x = NULL, y = "# HUC10s", title = "Distributions of HUC10 Random Effects") +
  theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size = 10))

gis$huc10 %>%
  left_join(
    df_ranef,
    by = "HUC10"
  ) %>%
  filter(name != "(Intercept)") %>%
  ggplot() +
  geom_sf(aes(color = value), size = 1) +
  geom_sf(data = rename(gis$states, name_ = name), fill = NA, size = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(vars(name))

gis$huc10 %>%
  left_join(
    df_ranef,
    by = "HUC10"
  ) %>%
  filter(name == "(Intercept)") %>%
  ggplot() +
  geom_sf(aes(color = value), size = 2) +
  geom_sf(data = rename(gis$states, name_ = name), fill = NA, size = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(vars(name))


# predictions -------------------------------------------------------------

df_pred <- inp$data_std %>%
  mutate(
    pred = inv.logit(predict(glmm, inp$data_std, allow.new.levels = TRUE))
  )

gis$catchments %>%
  inner_join(
    df_pred %>%
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
  geom_sf(data = rename(gis$states, name_ = name), fill = NA, size = 0.5) +
  scale_color_viridis_c(limits = c(0, 1)) +
  facet_wrap(vars(partition))

# goodness of fit
df_gof <- df_pred %>%
  select(partition, featureid, pred, presence) %>%
  mutate(
   result = case_when(
     presence == 0 & pred < 0.5 ~ "TN",
     presence == 0 & pred >= 0.5 ~ "FP",
     presence == 1 & pred < 0.5 ~ "FN",
     TRUE ~ "TP"
   )
  ) %>%
  nest(data = c(featureid, pred, presence, result)) %>%
  mutate(
    roc = map(data, function(x) {
      r <- roc(x$pred, as.factor(x$presence))
      tibble(
        cutoff = r[[1]],
        fpr = r[[2]],
        tpr = r[[3]]
      )
    }),
    cm = map(data, function (x) {
      confusionMatrix(data = factor(1 * (x$pred > 0.5)), reference = factor(x$presence), mode = "sens_spec", positive = "1")
    }),
    stats = map(data, function (x) {
      tibble(
        n = nrow(x),
        TP = sum(x$result == "TP"),
        TN = sum(x$result == "TN"),
        FP = sum(x$result == "FP"),
        FN = sum(x$result == "FN"),
        auc = auc(roc(x$pred, as.factor(x$presence)))
      )
    })
  ) %>%
  unnest(stats)

df_gof

df_gof$cm[[1]] # calib
df_gof$cm[[2]] # valid

# ROC curves
df_gof %>%
  select(partition, roc) %>%
  unnest(roc) %>%
  ggplot(aes(fpr, tpr)) +
  geom_line(aes(color = partition)) +
  coord_fixed() +
  labs(x = "FPR", y = "TPR", title = "ROC Curves", subtitle = "AUC = 0.955 (calib), 0.825 (valid)")

# gof by state
df_catch_state <- gis$catchments %>%
  filter(featureid %in% df_pred$featureid) %>%
  st_intersection(gis$states) %>%
  as_tibble() %>%
  select(featureid, state_abbr)

df_gof_state <- df_gof %>%
  select(partition, data) %>%
  unnest(data) %>%
  left_join(df_catch_state, by = "featureid") %>%
  nest(data = c(featureid, pred, presence, result)) %>%
  rowwise() %>%
  mutate(n = nrow(data)) %>%
  filter(n > 50) %>%
  ungroup() %>%
  mutate(
    cm = map(data, function (x) {
      confusionMatrix(data = factor(1 * (x$pred > 0.5), levels = c("0", "1")), reference = factor(x$presence, levels = c("0", "1")), mode = "sens_spec", positive = "1")
    }),
    accuracy = map_dbl(cm, ~ .$overall[['Accuracy']]),
    accuracy_pval = map_dbl(cm, ~ .$overall[['AccuracyPValue']]),
    stats = map(data, function (x) {
      tibble(
        TP = sum(x$result == "TP"),
        TN = sum(x$result == "TN"),
        FP = sum(x$result == "FP"),
        FN = sum(x$result == "FN")
        # auc = auc(roc(x$pred, as.factor(x$presence)))
      )
    })
  ) %>%
  unnest(stats)

df_gof_state %>%
  ggplot(aes(state_abbr)) +
  geom_point(aes(y = accuracy, shape = accuracy_pval <= 0.1, color = partition), size = 2) +
  geom_hline(
    data = df_gof %>%
      transmute(
        partition,
        accuracy = map_dbl(cm, ~ .$overall[['Accuracy']])
      ),
    aes(yintercept = accuracy, color = partition, linetype = "Global")
  ) +
  scale_shape_manual("Accuracy > NIR", values = c("TRUE" = 16, "FALSE" = 1), labels = c("TRUE" = "p <= 0.1", "FALSE" = "p > 0.1")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "State", y = "Accuracy", linetype = NULL)

gis$catchments %>%
  inner_join(
    df_gof %>%
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
  geom_sf(data = rename(gis$states, name_ = name), fill = NA, size = 0.5) +
  scale_color_brewer(type = "qual", palette = 2) +
  facet_grid(vars(result), vars(partition), labeller = labeller(result = c(
    TP = "True Positive",
    TN = "True Negative",
    FP = "False Positive",
    FN = "False Negative"
  )))

sf_catch %>%
  inner_join(
    df %>%
      transmute(
        featureid,
        presence,
        pred = fitted(glmm)
      ),
    by = "featureid"
  ) %>%
  mutate(
    class = case_when(
      presence == 0 & pred < 0.5 ~ "TN",
      presence == 0 & pred >= 0.5 ~ "FP",
      presence == 1 & pred < 0.5 ~ "FN",
      TRUE ~ "TP"
    )
  ) %>%
  ggplot() +
  geom_sf(aes(color = pred)) +
  scale_color_viridis_c()

sf_catch %>%
  inner_join(
    df %>%
      transmute(
        featureid,
        presence,
        pred = fitted(glmm)
      ),
    by = "featureid"
  ) %>%
  mutate(
    class = case_when(
      presence == 0 & pred < 0.5 ~ "TN",
      presence == 0 & pred >= 0.5 ~ "FP",
      presence == 1 & pred < 0.5 ~ "FN",
      TRUE ~ "TP"
    )
  ) %>%
  ggplot() +
  geom_sf(aes(color = pred)) +
  scale_color_viridis_c()

sf_huc10 %>%
  left_join(
    as_tibble(ranef(glmm)$huc10, rownames = "HUC10") %>%
      pivot_longer(-HUC10),
    by = "HUC10"
  ) %>%
  filter(name == "(Intercept)") %>%
  ggplot() +
  geom_sf(aes(color = value), size = 2) +
  scale_color_viridis_c() +
  facet_wrap(vars(name))

as_tibble(ranef(glmm)$huc10, .rows = )
glmm1 <- glmer(
  presence ~ mean_jul_temp +
  (1 | huc10),
  family = binomial(link = "logit"),
  data = df,
  control = glmerControl(optimizer = "bobyqa")
)


df %>%
  ggplot(aes(mean_jul_temp, forest)) +
  geom_point()

cor(df$mean_jul_temp, df$forest)

plot(df$huc10, resid(glmm), xlab = "HUC 10 basin", ylab = "Residuals")

plot_model(glmm, type = "re")

df_pred <- df
df_pred$pred <- as.numeric(inv.logit(predict(glmm, df_pred, allow.new.levels = TRUE))) > 0.5

df_pred %>%
  tabyl(presence, pred)

# map of predictions
# summary of covariates for missed predictions

# gof stats ---------------------------------------------------------------

pred <- model_pred(glmm, df)

cat(
  "summary stats--------------\n",
  "  n = ", scales::comma(pred$stats$n), "\n",
  "  % obs pos = ", scales::percent(mean(pred$y_obs)), "\n",
  "  % pred pos = ", scales::percent(mean(pred$y_pred)), "\n",
  "  sensitivity = ", sprintf("%.3f", pred$stats$sens), "\n",
  "  specificity = ", sprintf("%.3f", pred$stats$spec), "\n",
  "  accuracy = ", sprintf("%.3f", pred$stats$acc), "\n",
  "  auc = ", sprintf("%.3f", pred$stats$auc), "\n",
  "  error rate = ", sprintf("%.3f", pred$stats$err), "\n",
  "  false pos rate = ", sprintf("%.3f", pred$stats$fpr), "\n",
  "  false neg rate = ", sprintf("%.3f", pred$stats$fnr), "\n",
  sep = ""
)


# export ------------------------------------------------------------------

list(
  data = df,
  model = glmm,
  pred = pred
) %>%
  write_rds(file.path(config$wd, "model-calib.rds"))


# compare -----------------------------------------------------------------
#
# inp_old <- read_rds(file.path("/Users/jeff/Projects/sheds/data/bto-model//1.3.0", "model-input.rds"))
# calib_old <- read_rds(file.path("/Users/jeff/Projects/sheds/data/bto-model//1.3.0", "model-calib.rds"))
# predict_old <- read_rds(file.path("/Users/jeff/Projects/sheds/data/bto-model//1.3.0", "model-predict.rds"))
#
# predict_new <- read_rds(file.path("/Users/jeff/Projects/sheds/data/bto-model//1.4.0", "model-predict.rds"))
#
# glmm_new <- glmm
# glmm_old <- calib_old$model
#
# df_new <- inp$data_std %>%
#   filter(partition == "calib")
# df_old <- inp_old$calib$data_std
#
# summary(glmm_new)
# summary(glmm_old)
#
# pred_new <- model_pred(glmm_new, df_new)
# pred_old <- model_pred(glmm_old, df_old)
#
#
# predict_new %>%
#   select(featureid, huc12, new = occ_current) %>%
#   left_join(
#     predict_old %>%
#       select(featureid, huc12, old = occ_current),
#     by = c("featureid", "huc12")
#   ) %>%
#   sample_frac(size = 0.01) %>%
#   ggplot(aes(old, new)) +
#   geom_point(alpha = 0.5, size = 0.5)
#
#
# df_current_std_new <- read_rds(file.path("/Users/jeff/Projects/sheds/data/bto-model//1.4.0", "pred-current-std.rds"))
# df_current_std_old <- read_rds(file.path("/Users/jeff/Projects/sheds/data/bto-model//1.3.0", "pred-current-std.rds"))
#
# df_current_std <- bind_rows(
#   df_current_std_new %>%
#     mutate(source = "new"),
#   df_current_std_old %>%
#     mutate(source = "old")
# )
#
# x <- df_current_std %>%
#   filter(huc8 == "01050003")
#
# x %>%
#   select(featureid, source, prob) %>%
#   spread(source, prob) %>%
#   ggplot(aes(new, old)) +
#   geom_point(size = 0.5, alpha = 0.5)
#
# x %>%
#   filter(featureid %in% x_f) %>%
#   select(featureid, huc8, prob, source, all_of(covariates)) %>%
#   summary
#
# x %>%
#   select(featureid, source, prob, all_of(covariates)) %>%
#   pivot_longer(-c(featureid, source)) %>%
#   pivot_wider(names_from = "source") %>%
#   ggplot(aes(new, old)) +
#   geom_point(size = 0.5, alpha = 0.5) +
#   facet_wrap(vars(name), scales = "free")
#
# x2 <- df_current_std_new %>%
#   filter(huc8 == "01050003") %>%
#   select(featureid, huc10, huc12, all_of(covariates), new = prob) %>%
#   left_join(
#     df_current_std_old %>%
#       select(featureid, old = prob),
#     by = "featureid"
#   )
#
# x2 %>%
#   pivot_longer(-c(featureid, huc10, huc12, new, old)) %>%
#   ggplot(aes(new, old, color = pmin(pmax(value, -2), 2))) +
#   geom_point(aes(), size = 0.5, alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_wrap(vars(name))
#
# x2 %>%
#   ggplot(aes(new, old)) +
#   geom_abline() +
#   geom_point(size = 0.5, alpha = 0.5) +
#   facet_wrap(vars(huc10))
#
#
# df_ranef_new <- as_tibble(ranef(glmm_new)$huc10, rownames = "huc10")
# df_ranef_old <- as_tibble(ranef(glmm_old)$huc10, rownames = "huc10")
# df_ranef <- bind_rows(
#   df_ranef_new %>%
#     mutate(source = "new"),
#   df_ranef_old %>%
#     mutate(source = "old")
# )
#
# df_ranef %>%
#   filter(huc10 == "0105000304")
#
# inp$data %>%
#   filter(huc10 == "0105000304")
# inp_old$calib$data %>%
#   filter(huc10 == "0105000304")
# inp_old$valid$data %>%
#   filter(huc10 == "0105000304")
