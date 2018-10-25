# model validation
# <- {wd}/model-input.rds
# <- {wd}/model-calib.rds
# -> {wd}/model-valid.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-valid: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(AUC))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(boot))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(lme4))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading model-input...")
inp <- readRDS(file.path(config$wd, "model-input.rds"))
cat("done\n")

cat("loading model-calib...")
calib <- readRDS(file.path(config$wd, "model-calib.rds"))
cat("done\n")

# fitted model
glmm <- calib$model

# validation dataset with standardized covariates
df <- inp$valid$data_std

# calculate ---------------------------------------------------------------

cat("computing predictions and GOF stats...")
pred <- model_pred(glmm, df)
cat("done\n")

cat(
  "summary stats--------------\n",
  "  n = ", scales::comma(length(pred$y_pred)), "\n",
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
  pred = pred
) %>%
  saveRDS(file.path(config$wd, "model-valid.rds"))

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished model-valid: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed, digits = 1), " sec)\n", sep = "")
