EcoSHEDS Northeast Brook Trout Occupancy Model
==============================================

This model is developed and maintained as part of [EcoSHEDS](https://www.usgs.gov/apps/ecosheds/) by:

Jeffrey D. Walker, PhD  
[Walker Environmental Research LLC](https://walkerenvres.com/)

Benjamin Letcher, PhD  
[USGS](https://www.usgs.gov/staff-profiles/benjamin-h-letcher)

## About

This repo contains the source code and documentation for the [EcoSHEDS Northeast Brook Trout Occupancy Model](https://ecosheds.github.io/northeast-bto-model).

## Overview

The brook trout occupancy model is developed using an orchestrated data pipeline based on the [{targets}](https://books.ropensci.org/targets/) package. The entry point for this pipeline is the `_targets.R` file. The individual objects (data frames, model objects, plots) of this pipeline are defined in a series of scripts within the `./R` directory. These components are then loaded into the main targets pipeline within `_targets.R`. See the [{targets} book](https://books.ropensci.org/targets/) for more information about how to run and access each object.

## Configuration

Before running the data pipeline, configuration variables must be defined within a file named `.env` located within the project root directory. This file is not tracked by git, and therefore must be created manually. The configuration variables are automatically loaded using the [{dotenv}](https://github.com/gaborcsardi/dotenv) package. The `.env` file must contain the following variables.

```
BTO_WD_ROOT="/path/to/bto-model"

BTO_TEMP_MODEL="<temp model version>"

# EcoSHEDS database connection parameters
BTO_DB_HOST=
BTO_DB_PORT=
BTO_DB_DBNAME=
BTO_DB_USER=
BTO_DB_PASSWORD=
```

The `BTO_WD_ROOT` variable sets the root directory of the BTO model data files. This directory contains one sub-folder for each model version. For example, version 2.0.0 of the BTO model will export files to `${BTO_WD_ROOT}/2.0.0/`.

The `BTO_TEMP_MODEL` variable defines the stream temperature model version that should be used for importing estimated stream temperatures. This variable should usually be set to the most recent stream temperature model version.

Lastly, the `BTO_DB_*` variables define the connection parameters to the EcoSHEDS database for importing the temperature model predictions, and various GIS layers.

## Model Version

When updating the BTO model, first change the version number defined by the `bto_version` target in `_targets.R`. Official model versions should follow semantic versioning (see [Versioning](https://ecosheds.github.io/northeast-bto-model/history.html#versioning) for guidelines). However, modifications can be used during development (e.g. `2.0.0-dev`).

Next, create a new sub-folder within the `BTO_WD_ROOT` folder with a name identical to the new version. The new model files will be saved to this folder.

## Targets Pipeline

Once the configuration file (`.env`) and model version have been set, run the following commands to execute the targets pipeline. This command will automatically fetch the input and observation data, calibrate the model, generate predictions, create figures, and export the results.

```r
source("_targets.R")
tar_make()
```

To update only one target, specify the name to the make function:

```r
tar_make(obs_presence) # make the object
tar_read(obs_presence) # view the object
tar_load(obs_presence) # load the object into current environment
```

## Documentation

After finishing the model update, rebuild the documentation using the [{bookdown}](https://bookdown.org/) package. The documentation source files (Rmarkdown files) are located in the `./book` directory. The output files will then be generated and saved to the `./docs` directory. To rebuild the documentation run the following command or use the `Build Book` button in the Rstudio Build pane.

```r
rmarkdown::render_site(input = "book", encoding = 'UTF-8')
```

To publish the updated documentation, commit the `./docs` directory to git and push to GitHub. The new documentation will then be accessible via GitHub Pages at https://ecosheds.github.io/northeast-bto-model/.

## Model Release

Finally, after updating the documentation, create a new [release](https://github.com/EcoSHEDS/northeast-bto-model/releases) in the GithHub repo with a tag named `vX.Y.Z` and release name `vX.Y.Z (Jan 1, 2022)`. Provide a brief description of the changes and updates.

## License

See `LICENSE` file.
