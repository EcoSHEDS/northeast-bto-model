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

source("src/functions.R")

config <- load_config()

# load --------------------------------------------------------------------

inp <- readRDS(file.path(config$wd, "model-input.rds"))

# calibration dataset with standardized covariates
df <- inp$calib$data_std


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
  presence ~ mean_jul_temp +
    # forest +
    allonnet +
    devel_hi +
    agriculture +
    AreaSqKM * summer_prcp_mm +
    # mean_jul_temp * forest +
    # summer_prcp_mm * forest +
    (1 + AreaSqKM + agriculture + summer_prcp_mm + mean_jul_temp | huc10),
  family = binomial,
  data = df,
  control = glmerControl(optimizer = "bobyqa")
)
summary(glmm)
confint(glmm)



sf_huc10_all <- st_read("~/Projects/data/gis/WBD_National_GDB/WBD_National_GDB.gdb/", layer = "WBDHU10")
sf_huc10 <- sf_huc10_all %>%
  filter(HUC10 %in% rownames(ranef(glmm)$huc10)) %>%
  st_point_on_surface()
sf_huc10 %>%
  left_join(
    as_tibble(ranef(glmm)$huc10, rownames = "HUC10") %>%
      pivot_longer(-HUC10),
    by = "HUC10"
  ) %>%
  # filter(name != "(Intercept)") %>%
  ggplot() +
  geom_sf(aes(color = value), size = 2) +
  scale_color_viridis_c() +
  facet_wrap(vars(name))



con <- db_connect()

sf_catch <- st_read(con, query = "select featureid, geom_pour as geom from truncated_flowlines;")

DBI::dbDisconnect(con)


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
  geom_sf(aes(color = class), alpha = 0.5) +
  facet_wrap(vars(class))

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

# diagnostics
summary(glmm)

library(sjPlot)
plot_model(glmm, show.values = TRUE, value.offset = 0.3)
plot_model(glmm, sort.est = TRUE)
plot_model(glmm, type = "eff", terms = "mean_jul_temp [all]")
plot_model(glmm, type = "eff", terms = "agriculture [all]")
plot_model(glmm, type = "eff", terms = "allonnet [all]")
plot_model(glmm, type = "eff", terms = "devel_hi [all]")
plot_model(glmm, type = "eff", terms = "forest [all]")
plot_model(glmm, type = "eff", terms = "AreaSqKM [all]")
plot_model(glmm, type = "eff", terms = "summer_prcp_mm [all]")

# interaction
plot_model(glmm, type = "eff", terms = c("mean_jul_temp [all]", "forest [-1, 0, 1]"))
plot_model(glmm, type = "eff", terms = c("summer_prcp_mm [all]", "forest [-1, 0, 1]"))
plot_model(glmm, type = "eff", terms = c("summer_prcp_mm [all]", "AreaSqKM [-1, 0, 1]"))

tab_model(glmm)

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
  saveRDS(file.path(config$wd, "model-calib.rds"))
