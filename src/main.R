config <- config::get()

# process raw MA DFW dataset
# only need to run once, results saved to data/obs/madfw-ebt.csv and tracked by git
source("src/data/obs-madfw.R")

# import observation (presence/absence) dataset
source("src/data/obs.R")

# fetch huc dataset
source("src/data/huc.R")

# fetch covariates dataset
source("src/data/covariates.R")

# fetch temp-model dataset
source("src/data/temp-model.R")

# prepare model input dataset
source("src/model/input.R")

# fit model to calibration dataset
source("src/model/calib.R")

# check model validation
source("src/model/valid.R")

# generate model predictions
source("src/model/predict.R")

# save derived metrics to database
source("src/export/db.R")

# save derived metrics to csv
source("src/export/csv.R")
