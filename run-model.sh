#!/bin/bash
# Run the brook trout occupancy model from start to finish
# usage: ./run-model.sh

set -eu

cd ./r

# import observational (presence/absence) dataset
Rscript data-obs.R

# fetch huc dataset
Rscript data-huc.R

# fetch covariates dataset
Rscript data-covariates.R

# fetch temp-model dataset
Rscript data-temp.R

# prepare model input dataset
Rscript model-input.R

# fit model to calibration dataset
Rscript model-calib.R

# check model validation
Rscript model-valid.R
