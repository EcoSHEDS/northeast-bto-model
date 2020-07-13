#!/bin/bash
# Run the brook trout occupancy model from start to finish
# usage: ./run-model.sh

set -eu

./version.sh
LOG=/data/jeff/bto-model/$SHEDS_BTO_VERSION/bto-model.log

cd ./r

# process raw MA DFW dataset
# only need to run once, results saved to data/obs/madfw-ebt.csv and tracked by git
# Rscript data-obs-madfw.R

# import observation (presence/absence) dataset
Rscript data-obs.R > $LOG 2>&1

# fetch huc dataset
Rscript data-huc.R >> $LOG 2>&1

# fetch covariates dataset
Rscript data-covariates.R >> $LOG 2>&1

# fetch temp-model dataset
Rscript data-temp.R >> $LOG 2>&1

# prepare model input dataset
Rscript model-input.R >> $LOG 2>&1

# fit model to calibration dataset
Rscript model-calib.R >> $LOG 2>&1

# check model validation
Rscript model-valid.R >> $LOG 2>&1

# generate model predictions
Rscript model-predict.R >> $LOG 2>&1

# save derived metrics to database
Rscript export-db.R >> $LOG 2>&1   # -> db{bto_model}

# save derived metrics to csv
Rscript export-csv.R >> $LOG 2>&1  # -> csv/sheds-bto-model-v{VERSION}.csv

# save session info
Rscript session.R >> $LOG 2>&1