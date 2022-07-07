library(targets)

# load all targets
invisible(sapply(list.files("R", pattern = ".R$", full.names = TRUE), source))

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv", "sjPlot", "lme4"))

# load packages into session
if (interactive()) {
  sapply(tar_option_get("packages"), require, character.only = TRUE)
}

list(
  tar_target(bto_version, "2.0.0-dev"),
  tar_target(bto_wd, file.path(Sys.getenv("BTO_WD_ROOT"), bto_version), cue = tar_cue(mode = "always")),

  targets_huc,
  targets_gis,
  targets_obs,
  targets_cov,
  targets_temp,
  targets_inp,
  targets_model,
  targets_predict,
  targets_export
)
