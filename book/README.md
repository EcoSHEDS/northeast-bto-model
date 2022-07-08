EcoSHEDS Northeast Brook Trout Occupancy Model
==============================================

Jeffrey D Walker, PhD  
[Walker Environmental Research, LLC](https://walkerenvres.com)

## About

This folder contains source code for generating the documentation for the EcoSHEDS Northeast Brook Trout Occupancy Model. This documentation is updated with each new version of the model.

The documentation website is built using **bookdown** (https://github.com/rstudio/bookdown). See the page "[Get Started](https://bookdown.org/yihui/bookdown/get-started.html)" at https://bookdown.org/yihui/bookdown/ for how to compile this code, which was adapted from the "getting started" demo at https://bookdown.org/yihui/bookdown-demo/.

## Compiling

Open the R project file (`bto-model.Rproj`) within the R Studio IDE.

Click the `Build Book` button under the `Build` menu.

The output files will be generated in the `dovs/_book` sub-directory.

## Updating

When a new version of the model is released, follow this checklist to update the documentation.

- [ ] Update version number and date in the YAML frontmatter of index.Rmd
- [ ] Update Change Log in index.Rmd
- [ ] Make any necessary changes to the underlying theory, processing, etc.
- [ ] Regenerate all pages to update the tables and figures
- [ ] Assign new tag on Github after final push

## Deploying

After completing an update, deploy the compiled files to ecosheds.org.

- [ ] Deploy the output (`_book/`) to the web server under the new version number
```txt
scp -r _book/* user@ecosheds.org:/path/to/<vX.Y.Z>/
```
- [ ] Update the model homepage to list the newest version
- [ ] Update the `/latest` redirect in the web server to point to the newest version

