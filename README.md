SHEDS Brook Trout Occupancy Model
=================================

[Jeffrey D Walker, PhD](https://walkerenvres.com)

[Ben Letcher, PhD](https://www.lsc.usgs.gov/?q=cafb-ben-letcher)

[Dan Hocking, PhD](http://hockinglab.weebly.com/)

*Adapted from*: [Conte-Ecology/Northeast_Bkt_Occupancy](https://github.com/Conte-Ecology/Northeast_Bkt_Occupancy).


## Configuration

Most of the scripts in this repo rely on configuration variables set within the `config.sh` file. Because some of these variables contain sensitive information (e.g. database passwords), the `config.sh` file is not tracked in git. 

The configuration includes database connection parameters, and the local path to the model data directory.

However, a template (`config.template.sh`) is provided, with which a new `config.sh` can be set up. 

```bash
cp config.template.sh config.sh
nano config.sh
```

The current (or active) model version is set within the `version.sh` script, which is tracked by git and should be consistent with the repo tags.

The `version.sh` script sets the environmental variable `SHEDS_STM_VERSION` to the **minor** version only, and **does not include the patch number**. In other words, it is only of the form `X.Y` (see Model Versioning above) and does not include a `v` prefix.
