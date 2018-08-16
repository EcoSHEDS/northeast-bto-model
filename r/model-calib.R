# fit model to calibration dataset
# <- {wd}/model-input.rds
# -> {wd}/model-calib.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-calib: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(AUC))
suppressPackageStartupMessages(library(boot))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading model-input...")
inp <- readRDS(file.path(config$wd, "model-input.rds"))
cat("done\n")

# calibration dataset with standardized covariates
df <- inp$calib$data_std

# fit model ---------------------------------------------------------------

cat("fitting model using glmer...")
glmm <- glmer(
  presence ~ AreaSqKM * summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + meanJulyTemp * forest + summer_prcp_mm * forest + (1 + AreaSqKM + agriculture + summer_prcp_mm + meanJulyTemp | huc10),
  family = binomial(link = "logit"),
  data = df,
  control = glmerControl(optimizer="bobyqa")
)
cat("done\n")

# diagnostics
# library(sjPlot)
# plot_model(glmm)
# plot_model(glmm, type = "eff", terms = "forest")

# gof stats ---------------------------------------------------------------

cat("computing predictions and GOF stats...")
pred <- model_pred(glmm, df)
cat("done\n")

cat(
  "summary stats--------------\n",
  "  n = ", scales::comma(pred$n), "\n",
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

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished model-calib: ", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n", sep = "")

