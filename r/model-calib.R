# fit model to calibration dataset
# <- {wd}/model-input.rds
# -> {wd}/model-calib.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-calib:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()


# load --------------------------------------------------------------------

cat("loading model input from model-input.rds...")
inp <- readRDS(file.path(config$wd, "model-input.rds"))
cat("done\n")

# calibration dataset with standardized covariates
df <- inp$calib$data_std


# fit model ---------------------------------------------------------------

cat("fitting model using glmer (optimizer = bobyqa)...")
glmm <- glmer(presence ~ AreaSqKM * summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + meanJulyTemp * forest + summer_prcp_mm * forest + (1 + AreaSqKM + agriculture + summer_prcp_mm + meanJulyTemp | huc10), family = binomial(link = "logit"), data = df, control = glmerControl(optimizer="bobyqa"))
cat("done")

# export ------------------------------------------------------------------

# export
list(
  data = df_calib,
  model = glmm
) %>%
  saveRDS(file.path(config$wd, "model-calib.rds"))

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished model-calib:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")

