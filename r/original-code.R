# original model scripts from Conte-Ecology/Northeast_Bkt_Occupancy repo

library(tidyverse)
library(lubridate)
library(RPostgreSQL)
library(jsonlite)

library(lme4)
library(arm)
library(boot)
library(AUC)

theme_set(theme_bw())

source("functions.R")

config <- load_config()

old_wd <- "~/Projects/sheds/_archive/Northeast_Bkt_Occupancy/"

# retrieve covariates -----------------------------------------------------
# based on retrieve.R

# get list of featureids with observed presence/absence data
df_pa <- read.csv(file.path(old_wd, "Data/regional_occupancy_data.csv"), header = TRUE, stringsAsFactors = FALSE)
df_pa <- df_pa %>%
  mutate(Occupancy = ifelse(catch > 0, 1, catch)) %>%
  select(featureid, Occupancy)
df_pa <- df_pa %>%
  filter(!is.na(Occupancy))
featureids <- unique(df_pa$featureid)

# fetch covariates from db
db <- src_postgres(
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

tbl_covariates <- tbl(db, "covariates") %>%
  filter(featureid %in% featureids)
df_covariates_long <- collect(tbl_covariates)

df_covariates <- df_covariates_long %>%
  spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

# need to organize covariates into upstream or local by featureid
upstream <- df_covariates %>%
  group_by(featureid) %>%
  filter(
    zone == "upstream",
    is.na(riparian_distance_ft)
  ) %>%
  rename(forest_all = forest)

# Get upstream riparian forest
riparian_200 <- df_covariates %>%
  group_by(featureid) %>%
  select(featureid, forest, zone, riparian_distance_ft) %>%
  filter(
    zone == "upstream",
    riparian_distance_ft == 200
  )

# create covariateData input dataset
covariateData <- riparian_200 %>%
  select(-riparian_distance_ft) %>%
  left_join(upstream, by = c("featureid", "zone"))

# get average annual precip from monthly
covariateData <- covariateData %>%
  group_by(featureid) %>%
  mutate(
    ann_prcp = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm + apr_prcp_mm +
      may_prcp_mm + jun_prcp_mm + jul_prcp_mm + aug_prcp_mm + sep_prcp_mm +
      oct_prcp_mm + nov_prcp_mm + dec_prcp_mm,
    winter_prcp_mm = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm,
    spring_prcp_mm = apr_prcp_mm + may_prcp_mm + jun_prcp_mm,
    summer_prcp_mm = jul_prcp_mm + aug_prcp_mm + sep_prcp_mm,
    fall_prcp_mm = oct_prcp_mm + nov_prcp_mm + dec_prcp_mm
  )

# huc ids
tbl_huc12 <- tbl(db, "catchment_huc12") %>%
  filter(featureid %in% featureids) %>%
  collect()

featureid_huc8 <- tbl_huc12 %>%
  mutate(
    huc4 = str_sub(huc12, 1, 4),
    huc8 = str_sub(huc12, 1, 8),
    huc10 = str_sub(huc12, 1, 10)
  )

# merge covariates and huc ids
df_covariates <- covariateData %>%
  left_join(featureid_huc8, by = c("featureid"))

# save
saveRDS(df_covariates, file = file.path(config$wd, "covariates.rds"))

# prepare data ------------------------------------------------------------

df_covariates <- file.path(config$wd, "covariates.rds")

# temp-model derived metrics
df_metrics <- read.table(file.path(old_wd, "Data/derived_site_metrics.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
summary(df_metrics)

# merge covariates and temp-model derived metrics
df_covariates <- df_covariates %>%
  left_join(df_metrics, by = "featureid") %>%
  select(-totalObs, -meanDays.20, -yearsMaxTemp.18, -yearsMaxTemp.20, -yearsMaxTemp.22)

# rename area, prcp
df_covariates <- df_covariates %>%
  rename(
    area = AreaSqKM,
    prcp = ann_prcp
  )

summary(df_covariates)

# load bt presence/absence
df_pa <- read.csv(file.path(old_wd, "Data/regional_occupancy_data.csv"), header = TRUE, stringsAsFactors = FALSE)
summary(df_pa)

df_pa <- df_pa %>%
  mutate(pres = ifelse(catch > 0, 1, catch)) %>%
  select(featureid, pres) %>%
  filter(!is.na(pres))

df <- df_pa %>%
  left_join(df_covariates, by = "featureid")

str(df)
nrow(df)

# filter to sites with water temperature predictions
df <- df %>%
  filter(!is.na(meanJulyTemp))
nrow(df)

# deciding upper range of watershed size for occupance analysis, to remove large watersheds (i.e. outliers)
hist(df[df$pres == 1, ]$area)
df2 <- df[df$area <= 200, ]
hist(df2[df2$pres == 1, ]$area)

## Clean up unusual & outliers
summary(df2)

df2 <- df2 %>%
  select(-meanRMSE, -meanDays.22)

df2 <- df2 %>%
  filter(meanDays.18 < 300)

summary(df2)

# save cleaned dataset:
#   area <= 200 km2
#   mean days per year > 18 deg C < 300
#   !is.na(temp-model)
saveRDS(df2, file = file.path(config$wd, "data-clean.rds"))

# standardize continuous covariates
vars <- c("agriculture", "allonnet", "area", "devel_hi", "elevation", "forest", "surfcoarse", "prcp", "meanJulyTemp", "meanSummerTemp", "meanDays.18", "winter_prcp_mm", "spring_prcp_mm", "summer_prcp_mm", "fall_prcp_mm")
saveRDS(vars, file = file.path(config$wd, "vars.rds"))

stdFitCovs <- function(x, var.names){
  x2 <- dplyr::select(x, featureid)
  for(i in 1:length(var.names)){
    x2[ , var.names[i]] <- (x[ , var.names[i]] - mean(x[ , var.names[i]], na.rm = T)) / sd(x[ , var.names[i]], na.rm = T)
  }
  return(x2)
}

df.std <- stdFitCovs(df2, vars)
means <- NULL
stds <- NULL
for(i in 1:length(vars)) {
  means[i] <- mean(df2[ , vars[i]], na.rm = T)
  stds[i] <- sd(df2[ , vars[i]], na.rm = T)
}
means_stds <- data.frame(cbind(vars, means, stds))

data.fit.std <- df2 %>%
  mutate(
    fhuc8 = as.factor(huc8),
    fhuc10 = as.factor(huc10)
  ) %>%
  select(featureid, huc8, huc10, huc12, fhuc8, fhuc10, latitude, longitude, pres)
data.fit.std <- left_join(data.fit.std, df.std, by = c("featureid"))

summary(data.fit.std)

# check variable correlation
pairs(data.fit.std[ , vars])

#>>> summer precip isn't overly correlated with any 1 variable but moderately correlated with multiple (forest, temp, precip) so probably unnecessary to use in the models

# consider whether want a quadratic effect of area or if the data is just sparse when have small drainages
plot(data.fit.std$area, data.fit.std$pres)
lines(smooth.spline(data.fit.std$area, data.fit.std$pres), col = "red")
hist(data.fit.std$area)
#>>> probably no need to have quadratic area effect.

#>>> elevation is the only variable that can't include b/c of colinearity and can only use tmin or tmax
#>>> might not be able to use slope because moderate collinarity with multiple variables
#>>> also probably just use rising slope of air-water relationship. Rising and falling are correlated but rising has a bit more variability


# 01080203 = deerfield
# How many HUC 8, 10, and 12 for leaving out as validation set
length(unique(data.fit.std$huc8))
length(unique(data.fit.std$huc10))
length(unique(data.fit.std$huc12))
length(data.fit.std$huc10) # number total catchments

summary(data.fit.std)

# drop rows with any NA's
data.fit.std <- na.omit(data.fit.std)

# split calibration (80%) and validation (20%) by huc10
p.valid <- 0.2
n.fit <- floor(length(unique(data.fit.std$huc10)) * (1 - p.valid))

set.seed(24744)
huc.fit <- sample(unique(data.fit.std$huc10), n.fit, replace = FALSE) # select HUCs to hold back for testing
df.valid <- subset(data.fit.std, !huc10 %in% huc.fit)
df.fit <- subset(data.fit.std, huc10 %in% huc.fit)

# check if deerfield in fit set
stopifnot("01080203" %in% unique(df.fit$huc8))
stopifnot("01080203" %in% unique(df.valid$huc8))

# calibration size
n.fit        # huc10s
nrow(df.fit) # catchments
# validation size
length(unique(df.valid$huc10)) # huc10s
nrow(df.valid)                 # catchments

str(df.fit)
str(df.valid)

# save
saveRDS(df.fit, file = file.path(config$wd, "fit.rds"))     # calibration dataset
saveRDS(df.valid, file = file.path(config$wd, "valid.rds")) # validation dataset

# standardization values (mean, sd by var)
saveRDS(means_stds, file = file.path(config$wd, "means.rds"))


# fit model ---------------------------------------------------------------
# fit_occupancy_models.Rmd

df.fit <- readRDS(file = file.path(old_wd, "Data/fit.rds"))
df.valid <- readRDS(file = file.path(old_wd, "Data/valid.rds"))
df.means <- readRDS(file = file.path(old_wd, "Data/means.rds"))


## GLMM
## Global model with all covariates and check the optimizer:
## (http://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4)[http://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4]

library(optimx)
library(nloptr)

# varying intercept: all covariates
g0.bobyqa <- glmer(pres ~ area:prcp + meanJulyTemp + forest + allonnet + (1|fhuc10), family = binomial(link = "logit"), data = df.fit, control=glmerControl(optimizer="bobyqa", check.conv.singular="warning")) # no warning
summary(g0.bobyqa)


# COMPARE OPTIMIZERS
## from https://github.com/lme4/lme4/issues/98:
defaultControl <- list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 1e-6, maxeval = 1e5)
nloptwrap2 <- function(fn, par, lower, upper, control = list(), ...) {
  for (n in names(defaultControl))
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
    res <- nloptr(x0 = par, eval_f = fn, lb = lower, ub = upper, opts = control, ...)
    with(res,
      list(
        par = solution,
        fval = objective,
        feval = iterations,
        conv = if(status>0) 0 else status,
        message = message
      )
    )
}

g0.NM <- update(g0.bobyqa, control = glmerControl(optimizer = "Nelder_Mead")) # warning
g0.nlminb <- update(g0.bobyqa, control = glmerControl(optimizer="optimx", optCtrl = list(method = "nlminb"))) # no warning
g0.LBFGSB <- update(g0.bobyqa, control = glmerControl(optimizer="optimx", optCtrl = list(method = "L-BFGS-B"))) # warning
g0.bobyqa2 <- update(g0.bobyqa, control = glmerControl(optimizer = nloptwrap2)) # no warning
g0.NM2 <- update(g0.bobyqa, control = glmerControl(optimizer = nloptwrap2, optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD"))) # no warning

# list of all models
getpar <- function(x) c(getME(x, c("theta")), fixef(x))
modList <- list(
  bobyqa = g0.bobyqa,
  NM = g0.NM,
  nlminb = g0.nlminb,
  bobyqa2 = g0.bobyqa2,
  NM2 = g0.NM2,
  LBFGSB = g0.LBFGSB
)
ctab <- sapply(modList,getpar)
mtab <- reshape2::melt(ctab)

ggplot(mtab, aes(x = Var2, y = value, colour = Var2))+
  geom_point() +
  facet_wrap(~ Var1, scale = "free")

mtab %>%
  subset(Var2 %in% c("NM", "LBFGSB", "NM2","bobyqa","bobyqa2")) %>%
  ggplot(aes(x = Var2, y = value, colour = Var2))+
  geom_point() +
  facet_wrap(~ Var1, scale = "free")

# END: COMPARE OPTIMIZERS

#>>> Use the built-in bobyqa optimizer

# Global model wih varying intercept
glmm.M01 <- glmer(
  pres ~ meanJulyTemp + agriculture + devel_hi + forest + allonnet +
    area*summer_prcp_mm + agriculture*meanJulyTemp + forest*meanJulyTemp + forest*summer_prcp_mm +
    (1|fhuc10),
  family = binomial(link = "logit"),
  data = df.fit,
  control = glmerControl(optimizer = "bobyqa"))
summary(glmm.M01)

# plot residuals of glmm.M1 by HUC10 basin
plot(df.fit$fhuc10, resid(glmm.M01), xlab = "HUC 10 basin", ylab = "Residuals")

# varying intercept & flow
glmm.M02 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + summer_prcp_mm|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))
summary(glmm.M02)

# varying intercept & forest
glmm.M03 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))
summary(glmm.M03)

# varying intercept & area
glmm.M04 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))
summary(glmm.M04)

# varying intercept & tmax.stream
glmm.M05 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & devel
glmm.M06 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + devel_hi|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & ag
glmm.M07 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + agriculture|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & allonnet
glmm.M08 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + allonnet|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# compare models using AIC
AIC(glmm.M01, glmm.M02, glmm.M03, glmm.M04, glmm.M05, glmm.M06, glmm.M07, glmm.M08)
#>>> M04
## The model (M04) with varying-intercept & area, with all covariates is better than intercept-only model

# plot residuals by HUC10 basin
plot(df.fit$fhuc10, resid(glmm.M04), xlab = "HUC 10 basin", ylab = "Residuals")


## Add more random effects to M04
# varying intercept & area & flow
glmm.M11 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + summer_prcp_mm|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & rise.slope
glmm.M12 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & tmax.stream
glmm.M13 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & surfC
# glmm.M14 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area |fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & wet
glmm.M15 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + allonnet|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & wet
glmm.M16 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & wet
glmm.M17 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + devel_hi|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# compare models using AIC
# AIC(glmm.M04, glmm.M11, glmm.M12, glmm.M13, glmm.M14, glmm.M15, glmm.M16, glmm.M17)
AIC(glmm.M04, glmm.M11, glmm.M12, glmm.M13, glmm.M15, glmm.M16, glmm.M17)
#>>> M16


## Add more random effects to varying-intercept, area, agriculture model
# varying intercept & area & agriculture & flow
glmm.M21 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & rise.slope
glmm.M22 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & tmax.stream
glmm.M23 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & surfC
# glmm.M24 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture |fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & wet
glmm.M25 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + allonnet|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & wet
#glmm.M16 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & wet
glmm.M27 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + devel_hi|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# compare models using AIC
# AIC(glmm.M16, glmm.M21, glmm.M22, glmm.M23, glmm.M24, glmm.M25, glmm.M27)
AIC(glmm.M16, glmm.M21, glmm.M22, glmm.M23, glmm.M25, glmm.M27)
#>>> M21 (+ summer_prcp_mm)


## Add more random effects to varying-intercept & area & agriculture & summer_prcp_mm model

# varying intercept & area & agriculture & summer_prcp_mm & forest
glmm.M22c <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & summer_prcp_mm & meanJulyTemp
glmm.M23c <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & summer_prcp_mm & surfC
# glmm.M24c <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm |fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & summer_prcp_mm & allonnet
glmm.M25c <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + allonnet|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & summer_prcp_mm & wet
#glmm.M16 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & summer_prcp_mm & devel_hi
glmm.M27c <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + devel_hi|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# varying intercept & area & agriculture & summer_prcp_mm & forest + meanJulyTemp
glmm.M28c <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + forest + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa")) # warning that too many parameters

# compare models using AIC
# AIC(glmm.M21, glmm.M22c, glmm.M23c, glmm.M24c, glmm.M25c, glmm.M27c, glmm.M28c)
AIC(glmm.M21, glmm.M22c, glmm.M23c, glmm.M25c, glmm.M27c, glmm.M28c)
#>>> M23c (+ meanJulyTemp)

#>>> Final Random Effects: 1 + area + agriculture + summer_prcp_mm + meanJulyTemp | fhuc10

## Now that random effects are set, compare reduced models

## remove interactions
# remove forest*meanJulyTemp
glmm.M31 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# remove agriculture*meanJulyTemp
glmm.M32 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# remove summer_prcp_mm*forest
glmm.M33 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + agriculture*meanJulyTemp + meanJulyTemp*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

AIC(glmm.M23c, glmm.M31, glmm.M32, glmm.M33)
# removing each interaction was better or no different so remove all

# no interactions
glmm.M34 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

AIC(glmm.M23c, glmm.M31, glmm.M32, glmm.M33, glmm.M34)
#>>> M32 (- agriculture*meanJulyTemp)

#>>> remove covariates
# No agriculture
# glmm.M41 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + (1 + forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# No impoundment
glmm.M42 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest  + devel_hi + agriculture + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# No hydrologic/soil/bedrock characteristics
glmm.M43 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# Simple model
glmm.M44 <- glmer(pres ~ area*summer_prcp_mm + meanJulyTemp + forest + agriculture + meanJulyTemp*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))

# compare models using AIC
AIC(glmm.M32, glmm.M42, glmm.M43, glmm.M44) # M32 still the best model

(S32 <- summary(glmm.M32))

# plot residuals by HUC10 basin
plot(df.fit$fhuc10, resid(glmm.M32), xlab = "HUC 10 basin", ylab = "Residuals")

plot(glmm.M32)
summary(glmm.M32)

#>>> FINAL MODEL: M32 (area*summer_prcp_mm + meanJulyTemp + forest + allonnet + devel_hi + agriculture + meanJulyTemp*forest + summer_prcp_mm*forest + (1 + area + agriculture + summer_prcp_mm + meanJulyTemp|fhuc10))

### Check VIF for potential multicolliniarity issues
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

vif.lme(glmm.M32)

# export
list(
  data = df.fit,
  model = glmm.M32
) %>%
  saveRDS(file.path(config$wd, "fit_data_out.rds"))


# validation --------------------------------------------------------------
# validation.Rmd

df.fit <- readRDS(file.path(config$wd, "fit.rds"))
df.valid <- readRDS(file.path(config$wd, "valid.rds"))
df.means <- readRDS(file.path(config$wd, "means.rds"))
mout <- readRDS(file.path(config$wd, "fit_data_out.rds"))
df.fit <- mout$data
glmm.M32 <- mout$model
data.fit2 <- readRDS(file.path(config$wd, "data-clean.rds"))


# get variables used in model
vars <- readRDS(file.path(config$wd, "vars.rds"))
vars <- vars[which(vars != "prcp" & vars != "elevation")]

# get means and sd used for standardization
df.means <- readRDS(file.path(config$wd, "means.rds"))

# Helper functions
# make standardization function
stdFitCovs <- function(x, var.names, means_stds){
  x <- dplyr::ungroup(x)
  x2 <- dplyr::select(x, featureid)
  for(i in 1:length(var.names)){
    x2[ , var.names[i]] <- (x[ , var.names[i]] - as.numeric(as.character(means_stds[which(means_stds$vars == var.names[i]), "means"]))) / as.numeric(as.character(means_stds[which(means_stds$vars == var.names[i]), "stds"]))
  }
  return(x2)
}

## Check to see HUC-level temperature is correlated with intercept values
# huc.fit <- data.frame(row.names(coef(glmm.M32)$fhuc10), coef(glmm.M32)$fhuc10[,1])
# names(huc.fit) <- c("huc10", "intercept")
#
# huc.int.temp <- merge(huc.fit, df.huc, by = "huc10")
#
# plot(
#   huc.int.temp$tmax.huc,
#   huc.int.temp$intercept,
#   main="HUC10-level air temperature", xlab="tmax_ann", ylab="Intercept values in glmer")
# lines(smooth.spline(huc.int.temp$tmax.huc, huc.int.temp$intercept))
#>>> HUC-level temperature not correlated to random intercepts

## AUC
pred.fit <- inv.logit(predict(glmm.M32, df.fit, allow.new.levels = TRUE))

fit.sensitivity <- auc(sensitivity(pred.fit, as.factor(df.fit$pres)))
fit.specificity <- auc(specificity(pred.fit, as.factor(df.fit$pres)))
fit.accuracy <- auc(accuracy(pred.fit, as.factor(df.fit$pres)))
fit.auc <- auc(roc(pred.fit, as.factor(df.fit$pres)))

# error rates
fit.roc <- roc(pred.fit, as.factor(df.fit$pres))
df.fit.roc <- data.frame(cutoff = fit.roc[[1]], fpr = fit.roc[[2]], tpr = fit.roc[[3]])
dplyr::filter(df.fit.roc, cutoff > 0.395 & cutoff < 0.405)
error.rate <- mean((pred.fit >0.5 & df.fit$pres==0) | (pred.fit<0.5 & df.fit$pres==1))
false.pos <- mean((pred.fit > 0.5 & df.fit$pres==0))
false.pos <- mean((pred.fit > 0.4 & df.fit$pres==0))

# png(file = "Output/AUC_plots.png", width = 10, height = 8, units = "in", res = 150)
par(mfrow = c(2,2))
plot(sensitivity(pred.fit, as.factor(df.fit$pres)), main = "True Positives")
text(x = 0.15, y = 0.1, labels = paste0("Sensitivity = ", round(fit.sensitivity, digits = 2)))
plot(specificity(pred.fit, as.factor(df.fit$pres)), main = "True Negatives")
text(x = 0.8, y = 0.1, labels = paste0("Specificity = ", round(fit.specificity, digits = 2)))
plot(accuracy(pred.fit, as.factor(df.fit$pres)), main = "Accuracy (True Positives & Negatives)")
text(x = 0.11, y = 0.1, labels = paste0("Accuracy = ", round(fit.accuracy, digits = 2)))
plot(roc(pred.fit, as.factor(df.fit$pres)), main = "ROC AUC")
text(x = 0.8, y = 0.1, labels = paste0("AUC = ", round(fit.auc, digits = 2)))
par(mfrow = c(1,1))
# dev.off()

## Validation
pred.valid <- inv.logit(predict(glmm.M32, df.valid, allow.new.levels = TRUE))

valid.sensitivity <- auc(sensitivity(pred.valid, as.factor(df.valid$pres))) # 0.608
valid.specificity <- auc(specificity(pred.valid, as.factor(df.valid$pres))) # 0.638
valid.accuracy <- auc(accuracy(pred.valid, as.factor(df.valid$pres)))    # 0.621
valid.auc <- auc(roc(pred.valid, as.factor(df.valid$pres)))         # 0.746

# Error rates
valid.roc <- roc(pred.valid, as.factor(df.valid$pres))
df.valid.roc <- data.frame(cutoff = valid.roc[[1]], fpr = valid.roc[[2]], tpr = valid.roc[[3]])
# dplyr::filter(df.valid.roc, cutoff > 0.395 & cutoff < 0.405)

error.rate <- mean((pred.valid > 0.5 & df.valid$pres==0) | (pred.valid<0.5 & df.valid$pres==1))
false.pos <- mean((pred.valid > 0.5 & df.valid$pres==0))
false.pos <- mean((pred.valid > 0.4 & df.valid$pres==0))

tpr <- function(observed, predicted, threshold = NULL) {
  if(!is.null(threshold)) {
    if(threshold < 0 | threshold > 1) {
      stop("threshold should be between 0 and 1")
    }
    predicted <- ifelse(predicted >= threshold, 1, 0)
  }

  tp <- sum(observed == 1 & predicted == 1)
  fn <- sum(observed == 1 & predicted == 0)
  tpr <- tp / sum(observed == 1)

  return(tpr)
}

fpr <- function(observed, predicted, threshold = NULL) {
  if(!is.null(threshold)) {
    if(threshold < 0 | threshold > 1) {
      stop("threshold should be between 0 and 1")
    }
    predicted <- ifelse(predicted >= threshold, 1, 0)
  }

  tp <- sum(observed == 1 & predicted == 1)
  fp <- sum(observed == 0 & predicted == 1)
  fn <- sum(observed == 1 & predicted == 0)
  tn <- sum(observed == 0 & predicted == 0)
  fpr <- fp / sum(observed == 0)

  return(fpr)
}

fnr <- function(observed, predicted, threshold = NULL) {
  if(!is.null(threshold)) {
    if(threshold < 0 | threshold > 1) {
      stop("threshold should be between 0 and 1")
    }
    predicted <- ifelse(predicted >= threshold, 1, 0)
  }

  tp <- sum(observed == 1 & predicted == 1)
  fp <- sum(observed == 0 & predicted == 1)
  fn <- sum(observed == 1 & predicted == 0)
  tn <- sum(observed == 0 & predicted == 0)
  fnr <- fn / sum(observed == 1)

  return(fnr)
}

tnr <- function(observed, predicted, threshold = NULL) {
  if(!is.null(threshold)) {
    if(threshold < 0 | threshold > 1) {
      stop("threshold should be between 0 and 1")
    }
    predicted <- ifelse(predicted >= threshold, 1, 0)
  }

  tn <- sum(observed == 0 & predicted == 0)
  fp <- sum(observed == 0 & predicted == 1)
  tnr <- tn / sum(observed == 0)

  return(tnr)
}

roc_rates <- function(observed, predicted, threshold = NULL) {
  if(!is.null(threshold)) {
    if(threshold < 0 | threshold > 1) {
      stop("threshold should be between 0 and 1")
    }
    predicted <- ifelse(predicted >= threshold, 1, 0)
  }

  tp <- sum(observed == 1 & predicted == 1)
  fp <- sum(observed == 0 & predicted == 1)
  fn <- sum(observed == 1 & predicted == 0)
  tn <- sum(observed == 0 & predicted == 0)

  tpr <- tp / sum(observed == 1)
  fpr <- fp / sum(observed == 0)
  fnr <- fn / sum(observed == 1)
  tnr <- tn / sum(observed == 0)

  err <- data.frame(threshold, fpr, fnr, tpr, tnr)
  return(err)
}

# tpr.valid <- tpr(df.valid$pres, pred.valid, threshold = 0.4)
# 1 - tpr.valid
# fnr(df.valid$pres, pred.valid, threshold = 0.4)
# tnr.valid <- tnr(df.valid$pres, pred.valid, threshold = 0.4)
# 1 - tnr.valid
# fpr(df.valid$pres, pred.valid, threshold = 0.4)
# # total.error.valid <- fnr.valid + fpr.valid

# roc_rates(observed = as.integer(df.valid$pres), predicted = pred.valid, threshold = 0.1994309)

# determine threshold where sensitivity = specificity
plot(sensitivity(pred.valid, as.factor(df.valid$pres)))
plot(specificity(pred.valid, as.factor(df.valid$pres)), add = TRUE)

# sensitivity.valid <- sensitivity(pred.valid, as.factor(as.integer(df.valid$pres)))
# specificity.valid <- specificity(pred.valid, as.factor(df.valid$pres))
#
# valid.trues <- data.frame(threshold = sensitivity.valid$cutoffs, sensitivity = sensitivity.valid$measure, specificity = specificity.valid$measure)

valid.trues <- data.frame(thresholds = c(seq(from = 0, to = 1, by = 0.0001))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sensitivity = tpr(df.valid$pres, pred.valid, threshold = thresholds),
                specificity = tnr(df.valid$pres, pred.valid, threshold = thresholds),
                FNR = 1 - sensitivity,
                FPR = 1 - specificity)

equal.err <- dplyr::summarise_all(
  valid.trues[which(round(valid.trues$sensitivity, digits = 3) == round(valid.trues$specificity, digits = 3)), ], funs(mean)) # 0.478

# find threshold with fpr = 10%
fpr.10.valid <- dplyr::summarise_all(valid.trues[which(round(valid.trues$FPR, digits = 3) == 0.100), ], funs(mean)) # 0.2

# find threshold with fnr = 10%
fnr.10.valid <- dplyr::summarise_all(valid.trues[which(round(valid.trues$FNR, digits = 3) == 0.100), ], funs(mean)) # 0.72

# Threshold, Justif, FalsePos, FalseNeg, TotalError

# justifications
justification <- c("FNR = 10%", "Compare w/DSS", "Compare w/DeWeber", "Equal error rates", "Fitted data prevalence", "FPR = 10%") # equal error rates the same as the compare with DeWeber

# thresholds for each justification
thresholds <- c(round(fnr.10.valid$thresholds, digits = 2), 0.4, 0.46, round(equal.err$thresholds, digits = 2), round(mean(df.fit$pres), digits = 2), round(fpr.10.valid$thresholds, digits = 2))

# Make data frame with thresholds
valid.rates <- data.frame(justification, thresholds)

# calculate rates for each threshold
valid.rates <- valid.rates %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sensitivity = round(tpr(df.valid$pres, pred.valid, threshold = thresholds), digits = 2),
                specificity = round(tnr(df.valid$pres, pred.valid, threshold = thresholds), digits = 2),
                FNR = 1 - sensitivity,
                FPR = 1 - specificity,
                total_error_rate = FNR + FPR) # doesn't match above and seems exceedingly high. Something wrong.

saveRDS(valid.rates, file = file.path(config$wd, "validation_error_table.rds"))

# make ROC plots
# png(file = "Output/AUC_valid_plots.png", width = 10, height = 8, units = "in", res = 150)
par(mfrow = c(2,2))
plot(sensitivity(pred.valid, as.factor(df.valid$pres)), main = "True Positives")
text(x = 0.15, y = 0.1, labels = paste0("Sensitivity = ", round(valid.sensitivity, digits = 2)))
plot(specificity(pred.valid, as.factor(df.valid$pres)), main = "True Negatives")
text(x = 0.8, y = 0.1, labels = paste0("Specificity = ", round(valid.specificity, digits = 2)))
plot(accuracy(pred.valid, as.factor(df.valid$pres)), main = "Accuracy (True Positives & Negatives)")
text(x = 0.11, y = 0.1, labels = paste0("Accuracy = ", round(valid.accuracy, digits = 2)))
plot(roc(pred.valid, as.factor(df.valid$pres)), main = "ROC AUC")
text(x = 0.8, y = 0.1, labels = paste0("AUC = ", round(valid.auc, digits = 2)))
par(mfrow = c(1,1))
# dev.off()

library(ROCR)
pred <- prediction(pred.valid, labels = df.valid$pres)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col=rainbow(10))

mean(perf@alpha.values[[1]][which(round(perf@x.values[[1]], digits = 3) == 0.100)])

mean(perf@alpha.values[[1]][which(round(1 - perf@y.values[[1]], digits = 3) == 0.100)])

### Effect of forest
# simulate coef values
n.sims=1000
simCoef <- as.data.frame(fixef(sim(glmm.M32, n.sims=n.sims)))
names(simCoef) <- names(fixef(glmm.M32))

# Plot effect of catchment forest on occurrence prob at a typical HUC10 basin # Gelman p. 44
eff.forest <- data.frame(forest.raw=seq(0,100,length.out=100)
                         , meanJulyTemp=rep(0,100)
                         , prcp=rep(0,100)
                         , surfcoarse=rep(0,100)
                         , devel_hi=rep(0,100)
                         , agriculture=rep(0,100)
)

eff.forest$forest <- (eff.forest$forest.raw - mean(data.fit2$forest, na.rm=T))/sd(data.fit2$forest, na.rm=T)

sim.prob.forest <- matrix(NA, nrow=nrow(eff.forest), ncol=n.sims)
for (i in 1:n.sims){
  sim.prob.forest[,i] <- exp(simCoef[i,1] + simCoef[i,"forest"]*eff.forest$forest) / (1 + exp(simCoef[i,1] + simCoef[i,"forest"]*eff.forest$forest))
}
sim.prob.forest <- as.data.frame(sim.prob.forest)

eff.forest$mean <- apply(sim.prob.forest[,1:n.sims], 1, mean)
eff.forest$lower <- apply(sim.prob.forest[,1:n.sims], 1, quantile, probs=c(0.025))
eff.forest$upper <- apply(sim.prob.forest[,1:n.sims], 1, quantile, probs=c(0.975))

ggplot(eff.forest, aes(x = forest.raw, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill="grey") +
  geom_line(colour = "black", size = 2) +
  #labs(title = "Occupancy in CT, MA, NH & NY") +
  xlab("Percent forest cover") +
  ylab("Occupancy probability") +
  theme_bw() +
  ylim(0, 1) +
  theme(axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=15),
        axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, angle=90, face="bold"),
        plot.title = element_text(size=20))

# ggsave(filename = "Output/Forest_Effects.png")

### Effect of july temp
# simulate coef values
n.sims=1000
simCoef <- as.data.frame(fixef(sim(glmm.M32, n.sims=n.sims)))
names(simCoef) <- names(fixef(glmm.M32))

# Plot effect of catchment meanJulyTemp on occurrence prob at a typical HUC10 basin # Gelman p. 44
eff.meanJulyTemp <- data.frame(meanJulyTemp.raw=seq(12,30,length.out=100), forest=rep(0,100), flow=rep(0,100), meanJulyTemp=rep(0,100))
eff.meanJulyTemp$meanJulyTemp <- (eff.meanJulyTemp$meanJulyTemp.raw - mean(data.fit2$meanJulyTemp, na.rm=T))/sd(data.fit2$meanJulyTemp, na.rm=T)

sim.prob.meanJulyTemp <- matrix(NA, nrow=nrow(eff.meanJulyTemp), ncol=n.sims)
for (i in 1:n.sims){
  sim.prob.meanJulyTemp[,i] <- exp(simCoef[i,1] + simCoef[i,"meanJulyTemp"]*eff.meanJulyTemp$meanJulyTemp) / (1 + exp(simCoef[i,1] + simCoef[i,"meanJulyTemp"]*eff.meanJulyTemp$meanJulyTemp))
}
sim.prob.meanJulyTemp <- as.data.frame(sim.prob.meanJulyTemp)

eff.meanJulyTemp$mean <- apply(sim.prob.meanJulyTemp[,1:n.sims], 1, mean)
eff.meanJulyTemp$lower <- apply(sim.prob.meanJulyTemp[,1:n.sims], 1, quantile, probs=c(0.025))
eff.meanJulyTemp$upper <- apply(sim.prob.meanJulyTemp[,1:n.sims], 1, quantile, probs=c(0.975))

ggplot(eff.meanJulyTemp, aes(x = meanJulyTemp.raw, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill="grey") +
  geom_line(colour = "black", size = 2) +
  #labs(title = "Occupancy in CT, MA, NH & NY") +
  xlab("Mean July stream temperature") +
  ylab("Occupancy probability") +
  #scale_x_reverse() +
  theme_bw() +
  ylim(0, 1) +
  theme(axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=15),
        axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, angle=90, face="bold"),
        plot.title = element_text(size=20))

# ggsave("Output/July_Temp_Effect.png")



# predictions -------------------------------------------------------------
# validation.Rmd

## Get all catchment data for predictions

# connect to database source
db <- src_postgres(
  dbname = config$db$dbname,
  host = config$db$host,
  port = config$db$port,
  user = config$db$user,
  password = config$db$password
)

# fetch covariates
# featureid |  variable  | value | zone  | riparian_distance_ft
cov_fetch <- c("agriculture", "allonnet", "AreaSqKM", "devel_hi", "forest", "surfcoarse", "jan_prcp_mm",
               "feb_prcp_mm",
               "mar_prcp_mm",
               "apr_prcp_mm",
               "may_prcp_mm",
               "jun_prcp_mm",
               "jul_prcp_mm",
               "aug_prcp_mm",
               "sep_prcp_mm",
               "oct_prcp_mm",
               "nov_prcp_mm",
               "dec_prcp_mm",
               "ann_tmax_c",
               "ann_tmin_c")

tbl_covariates <- tbl(db, 'covariates') %>%
  filter(variable %in% cov_fetch)

df_covariates_long <- collect(tbl_covariates)

df_covariates <- df_covariates_long %>%
  spread(variable, value)

summary(df_covariates)

# need to organize covariates into upstream or local by featureid
upstream <- df_covariates %>%
  group_by(featureid) %>%
  filter(
    zone == "upstream",
    is.na(riparian_distance_ft)
  ) %>%
  rename(forest_all = forest)

# Get upstream riparian forest
riparian_200 <- df_covariates %>%
  group_by(featureid) %>%
  select(featureid, forest, zone, riparian_distance_ft) %>%
  filter(
    zone == "upstream",
    riparian_distance_ft == 200
  )

# merge riparian and non-riparian covariates
covariateData <- riparian_200 %>%
  select(-riparian_distance_ft) %>%
  left_join(upstream, c("featureid", "zone"))

# seasonal total precip
covariateData <- covariateData %>%
  group_by(featureid) %>%
  mutate(
    winter_prcp_mm = jan_prcp_mm + feb_prcp_mm + mar_prcp_mm,
    spring_prcp_mm = apr_prcp_mm + may_prcp_mm + jun_prcp_mm,
    summer_prcp_mm = jul_prcp_mm + aug_prcp_mm + sep_prcp_mm,
    fall_prcp_mm = oct_prcp_mm + nov_prcp_mm + dec_prcp_mm
  )

# add huc info
tbl_huc12 <- tbl(db, 'catchment_huc12') %>%
  collect()

featureid_huc8 <- tbl_huc12 %>%
  mutate(
    huc4 = str_sub(huc12, 1, 4),
    huc8 = str_sub(huc12, 1, 8),
    huc10 = str_sub(huc12, 1, 10)
  )

df_covariates <- covariateData %>%
  left_join(featureid_huc8, by = "featureid")

# add meanJulyTemp from derived metrics
df_metrics <- read.table(file.path(old_wd, "Data/derived_site_metrics.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)

# add derived metrics to covariates
df_covariates <- df_covariates %>%
  left_join(df_metrics, by = "featureid") %>%
  dplyr::select(-totalObs, -meanDays.20, -yearsMaxTemp.18, -yearsMaxTemp.20, -yearsMaxTemp.22)

df_covariates <- df_covariates %>%
  dplyr::rename(area = AreaSqKM)






# TODO: FINISH PREDICTIONS (missing covariates??)







## SCENARIOS
## 0, +2, +4, +6 C scenarios

# link huc factors with actual hucs
df_hucs <- df.fit %>%
  dplyr::select(huc10, fhuc10) %>%
  distinct() #%>%
#dplyr::mutate(fhuc10 = as.numeric(fhuc10))

length(unique(df.fit$huc10) %in% unique(df_covariates$huc10))

df <- df_covariates %>%
  dplyr::left_join(df_hucs, by = "huc10")

# limit to 200 km2 drainages
df <- df %>%
  dplyr::filter(area <= 200)

# set temperature scenarios
temp_scenarios <- c(0, 2, 4, 6)

# make new dataframe on original scale for predictions
df <- dplyr::select(df, featureid, huc8, huc10, huc12, fhuc10, one_of(vars))
# df <- na.omit(df) # need to keep NA in fhuc10
df <- df %>%
  dplyr::filter(
    !is.na(agriculture),
    !is.na(forest),
    !is.na(meanJulyTemp),
    !is.na(summer_prcp_mm)
  )

# add new temp scenarios to the dataframe, then standardize, and predict
pred.temps <- data.frame(matrix(NA, dim(df)[1], length(temp_scenarios)))
names(pred.temps) <- c("current", "plus2", "plus4", "plus6")
for(j in 1:length(temp_scenarios)) {
  df <- dplyr::ungroup(df)
  # add scenario
  df_warming <- df %>%
    dplyr::mutate(meanJulyTemp = meanJulyTemp + temp_scenarios[j])

  # standardize
  df.warming.std <- stdFitCovs(df_warming, vars, df.means)

  data.warming.std <- df_warming %>%
    dplyr::select(featureid, huc8, huc10, huc12, fhuc10)
  data.warming.std <- dplyr::left_join(data.warming.std, df.warming.std)

  # make predictions
  pred.temps[ , j] <- inv.logit(predict(glmm.M32, data.warming.std, allow.new.levels = TRUE))
  rm(df_warming)
  rm(df.warming.std)
  rm(data.warming.std)
}

pred.temps$featureid <- df$featureid

df <- df %>%
  dplyr::left_join(pred.temps)

# link to all featureid in the database
df_featureid <- df_metrics %>%
  dplyr::select(featureid)

df_sheds <- df_featureid %>%
  dplyr::left_join(df) %>%
  dplyr::select(featureid, fhuc10, current, plus2, plus4, plus6)

# clip to catchments within 10 km of the known brook trout range
range_id <- read.csv("Data/bkt_range_10km_featureids.csv", header = T, stringsAsFactors = FALSE) %>%
  dplyr::rename(featureid = FEATUREID)

df_sheds <- df_sheds %>%
  dplyr::filter(featureid %in% unique(range_id$featureid))

# fill back out with NA for remaining catchments
df_sheds <- df_metrics %>%
  dplyr::select(featureid) %>%
  dplyr::left_join(df_sheds) %>%
  dplyr::mutate(featureid = as.integer(featureid))


# output for SHEDS
write.csv(df_sheds, file = "Output/sheds_trout_predictions.csv", row.names = FALSE)
write.csv(df, file = "Output/trout_predictions_and_covs.csv", row.names = FALSE)


# output derived metrics for Kevin's Designing Sustainable Landscapes Project
df_metrics_dsl <- df_metrics %>%
  dplyr::select(featureid, meanJulyTemp, mean30DayMax, meanResist, TS) %>%
  dplyr::mutate(featureid = as.integer(featureid))

write.csv(df_metrics_dsl, file = "Output/dsl_metrics.csv", row.names = FALSE)


## relationship between forest and meanJulyTemp
lm1 <- lm(meanJulyTemp ~ forest, data = df)
summary(lm1)
lm1$coefficients["forest"]


## Forest thresholds
## temp and forest thresholds
forest_change <- seq(-20, 20, by = 0.5)

#df <- dplyr::select(df, featureid, huc8, huc10, huc12, fhuc8, fhuc10, latitude, longitude, one_of(vars))
#df <- na.omit(df)

df <- dplyr::ungroup(df)

for(j in 1:length(forest_change)) {
  df_warming <- df %>%
    dplyr::mutate(forest = forest + forest_change[j],
                  forest = ifelse(forest > 100, 100, forest),
                  forest = ifelse(forest < 0, 0, forest),
                  meanJulyTemp = meanJulyTemp + lm1$coefficients["forest"]*forest_change[j])

  df.warming.std <- stdFitCovs(df_warming, vars, df.means)

  data.warming.std <- df_warming %>%
    dplyr::select(featureid, huc8, huc10, huc12, fhuc10)
  data.warming.std <- dplyr::left_join(data.warming.std, df.warming.std, by = c("featureid"))

  data.warming.std <- data.warming.std %>%
    dplyr::filter(!is.na(forest))

  pred.forest <- data.warming.std %>%
    dplyr::select(featureid)

  pred.forest$occ <- inv.logit(predict(glmm.M32, data.warming.std, allow.new.levels = TRUE))

  pred.forest$forest_scenario <- forest_change[j]

  if(!exists("df_forest_scenarios")) {
    df_forest_scenarios <- pred.forest
  } else {
    df_forest_scenarios <- rbind(df_forest_scenarios, pred.forest)
  }
  rm(df_warming)
  rm(df.warming.std)
  rm(data.warming.std)
}

threshold <- c(0.3, 0.5, 0.7)
for(i in 1:length(threshold)) {
  foo <- df_forest_scenarios %>%
    dplyr::filter(occ >= threshold[i])

  bar <- foo %>%
    dplyr::group_by(featureid) %>%
    dplyr::summarise(min_forest_change = min(forest_scenario, na.rm=T))

  names(bar) <- c("featureid", paste0("min_forest_", threshold[i]))

  df <- df %>%
    dplyr::left_join(bar)
}

## temp and forest thresholds
temps <- seq(0, 6, by = 0.5)

#df <- dplyr::select(df, featureid, huc8, huc10, huc12, fhuc8, fhuc10, latitude, longitude, one_of(vars))
#df <- na.omit(df)

df <- dplyr::ungroup(df)

rm(pred.temps)

rm(df_temp_scenarios)

for(j in 1:length(temps)) {
  pred.temps <- df %>%
    dplyr::select(featureid)

  df_warming <- df %>%
    dplyr::mutate(meanJulyTemp = meanJulyTemp + temps[j])

  df.warming.std <- stdFitCovs(df_warming, vars, df.means)

  data.warming.std <- df_warming %>%
    dplyr::select(featureid, huc8, huc10, huc12, fhuc10)
  data.warming.std <- dplyr::left_join(data.warming.std, df.warming.std, by = c("featureid"))

  pred.temps$occ <- inv.logit(predict(glmm.M32, data.warming.std, allow.new.levels = TRUE))

  pred.temps$temp_scenario <- temps[j]

  if(!exists("df_temp_scenarios")) {
    df_temp_scenarios <- pred.temps
  } else {
    df_temp_scenarios <- rbind(df_temp_scenarios, pred.temps)
  }
  rm(df_warming)
  rm(df.warming.std)
  rm(data.warming.std)
}

threshold <- c(0.3, 0.5, 0.7)
for(i in 1:length(threshold)) {
  foo <- df_temp_scenarios %>%
    dplyr::filter(occ >= threshold[i])

  bar <- foo %>%
    dplyr::group_by(featureid) %>%
    dplyr::summarise(max_temp = max(temp_scenario))

  names(bar) <- c("featureid", paste0("max_temp_", threshold[i]))

  df <- df %>%
    dplyr::left_join(bar)
}

rm(foo)
rm(bar)
gc()
df_predictions <- dplyr::select(df_covariates, featureid) %>%
  dplyr::left_join(dplyr::select(df, featureid, current, plus2, plus4, plus6, max_temp_0.3, max_temp_0.5, max_temp_0.7,  min_forest_0.3, min_forest_0.5, min_forest_0.7)) %>%
  dplyr::rename(occ_current = current)

saveRDS(df_predictions, file = "Output/Occupancy_Predictions.RData")
write.csv(df_predictions, file = "Output/Occupancy_Predictions.csv", row.names = FALSE)




## Add State Information
# set connection to database
db <- src_postgres(dbname='sheds', host='ecosheds.org', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

# table connection
tbl_states <- tbl(db, 'catchment_state')

df_states <- dplyr::collect(tbl_states) %>%
  dplyr::rename(state = stusps)
str(df_states)

df <- dplyr::left_join(df, df_states)

saveRDS(df, file = "Output/Threshold_Predictions.RData")


## Quick Map to see if results make sense
library(ggmap)
library(akima)
library(scales)

ne_coords <- c(-80.73, 39.95, -66, 47.5)

ne_map <- get_map(ne_coords, source = "google", maptype = c("terrain"), crop = TRUE)

foo <- df_sheds %>%
  left_join(dplyr::select(df_metrics, featureid, latitude, longitude))

g <- ggmap(ne_map) +
  geom_point(aes(longitude, latitude, color = current), size = 0.5,
             data = foo) +
  scale_colour_gradientn("Occupancy\nProbability", na.value = "grey20",
                         colours = c("#660000", "#f9f3c2"))
g
ggsave(g, file = "Output/occupancy_map.png")

df_predictions <- df_predictions %>%
  left_join(dplyr::select(df_metrics, featureid, latitude, longitude))

g <- ggmap(ne_map) +
  geom_point(aes(longitude, latitude, color = min_forest_0.5), size = 0.5,
             data = df_predictions) +
  scale_colour_gradientn("Forest Change\nfor 50% Occupancy", na.value = "grey20",
                         colours = c("#f9f3c2", "#660000"))
g
ggsave(g, file = "Output/occupancy_dForest_50pct_map.png")


# ---------------end working code--------------


## temporal trend in occupancy probability among catchments

## add current & future occupancy probability to the main df
data.pred2$current <- data.pred.std$occ.prob
data.pred2$plus2 <- data.pred.std.plus2$occ.prob
data.pred2$plus4 <- data.pred.std.plus4$occ.prob
data.pred2$plus6 <- data.pred.std.plus6$occ.prob

## make a long df for ggplot2
occ.prob.long <- melt(data.pred2[c("FEATUREID","state","fhuc10","current","plus2","plus4","plus6")],
                      id.vars=c("FEATUREID","state","fhuc10"),
                      variable.name="scenario",
                      value.name="occ.prob")

## plot temporal trend by state
ggplot(data=occ.prob.long[occ.prob.long$state %in% c('Connecticut','Massachusetts','New Hampshire','New York'), ],
       aes(x=scenario,y=occ.prob,fill=state)) + geom_boxplot() +
  ggtitle("Distribution of mean predicted occupancy probability \nunder current and future temperature increase scenarios") +
  xlab("Scenarios") + ylab("Probability of occupancy") +
  theme(axis.text.y = element_text(size=15, colour="black"),
        axis.text.x = element_text(size=15, colour="black"),
        axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, angle=90, face="bold"),
        plot.title = element_text(size=20),
        legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size=16))

## plot temporal trend by state (including ME, VT & RI)
ggplot(data=occ.prob.long[occ.prob.long$state %in% c('Connecticut','Massachusetts','New Hampshire','New York','Vermont','Maine','Rhode Island'), ],
       aes(x=scenario,y=occ.prob,fill=state)) + geom_boxplot() +
  ggtitle("Distribution of mean predicted occupancy probability \nunder current and future temperature increase scenarios") +
  xlab("Scenarios") + ylab("Probability of occupancy") +
  theme(axis.text.y = element_text(size=15, colour="black"),
        axis.text.x = element_text(size=15, colour="black"),
        axis.title.x = element_text(size=17, face="bold"),
        axis.title.y = element_text(size=17, angle=90, face="bold"),
        plot.title = element_text(size=20),
        legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size=16))







## make three-dimensional array of HUC10 basin specific coef values from simulation

### create blank array
n.sims=100; n.param=length(fixef(final.model)); n.huc=nrow(AllHuc10)  # n.param includes intercept
coefArray <- array(NA, dim = c(n.sims, n.param, n.huc),
                   dimnames=list(1:n.sims, c("intercept","log.area","asin.forest","sqrt.slope","precip",
                                             "tmin","drainC","log.area*asin.forest",
                                             "sqrt.slope*tmin"), 1:n.huc))

### simulate coef values for each Huc10
final.model.sim <- sim(final.model, n.sims=n.sims)
coef.final.model.sim <- coef(final.model.sim)

### populate coef for fixed effects in the blank array
for (i in 1:n.huc){
  coefArray[ ,6:n.param, i] <- coef.final.model.sim$fixef[, 6:n.param]
}
coefArray[,,1:3]  # check to make sure that all are populated except first columns

### now populate random-effect columns
for (i in 1:n.huc){
  coefArray[,1:5,i] <- coef.final.model.sim$ranef$fhuc10[,i,1:5]
}
coefArray[,,1:3]  # check to make sure

### add one more array for fish data in unmodeled HUC10 basins
coefUnmod <- coef.final.model.sim$fixef

### merge this to coef
library(abind)
coefArray <- abind(coefArray, coefUnmod)
str(coefArray)


# 1.Prediction at current condition
## prepare df for prediction

### make another df so as not write over
envDfPred <- envDf15
envDfPred <- merge(envDfPred, AllHuc10, all.x=TRUE)
envDfPred$fhuc10[ is.na(envDfPred$fhuc10) ] <- n.huc + 1  # +1 to indicate a newly added group

### standardize covariates
envDfPred$stdArea <- (envDfPred$TotDASqKM - mean(envDf15Sampled$TotDASqKM))/sd(envDf15Sampled$TotDASqKM)
envDfPred$stdForest <- (envDfPred$forest - mean(envDf15Sampled$forest))/sd(envDf15Sampled$forest)
envDfPred$stdSlope <- (envDfPred$slope - mean(envDf15Sampled$slope))/sd(envDf15Sampled$slope)
envDfPred$stdSurf_coarse <- (envDfPred$surf_coarse - mean(envDf15Sampled$surf_coarse, na.rm=TRUE))/sd(envDf15Sampled$surf_coarse, na.rm=TRUE)
# replace NA with mean values for surf_coarse
envDfPred$stdSurf_coarse[is.na(envDfPred$stdSurf_coarse)]  <- mean(envDf15Sampled$surf_coarse, na.rm=TRUE)
envDfPred$stdAirTemp <- (envDfPred$tmin - mean(envDf15Sampled$tmin))/sd(envDf15Sampled$tmin)
envDfPred$stdPrecip <- (envDfPred$precip_annual - mean(envDf15Sampled$precip_annual))/sd(envDf15Sampled$precip_annual)


## prediction

# fill an empty array with prediction
logitPredArray <- array(NA, dim = c(nrow(envDfPred), n.sims))  # empty array
fHuc <- envDfPred$fhuc10   # this is needed for subsetting below

for (i in 1:nrow(envDfPred)){
  for (j in 1:n.sims){
    logitPredArray[i,j] <- coefArray[j,1,fHuc[i]] +   # basin-specific intercept
      sum(envDfPred[i,56:61]*coefArray[j, 2:n.param, fHuc[i]])   # coef: make sure numbers are right
  }
}

# transform to probabilit scale
predArray <- inv.logit(logitPredArray)

# mean & 95% CI of occ. prob.
envDfPred$probMean <- apply(predArray,1,mean)
envDfPred$probLow <- apply(predArray,1,quantile,probs=c(0.025),na.rm=TRUE)
envDfPred$probHigh <- apply(predArray,1,quantile,probs=c(0.975),na.rm=TRUE)

write.csv(envDfPred, file="envDfPred1.csv", row.names=FALSE)



# 2.Prediction when air temp increases by 4C
## prepare df for prediction

### make another df so as not write over
envDfPred2 <- envDf15
envDfPred2 <- merge(envDfPred2, AllHuc10, all.x=TRUE)
envDfPred2$fhuc10[ is.na(envDfPred2$fhuc10) ] <- n.huc + 1  # +1 to indicate a newly added group

### 4C to current tmin values
envDfPred2$tminC <- (envDfPred2$tmin - 32)*5/9
envDfPred2$tminFuture <- envDfPred2$tminC + 4

### standardize covariates
envDfPred2$stdArea <- (envDfPred2$TotDASqKM - mean(envDf15Sampled$TotDASqKM))/sd(envDf15Sampled$TotDASqKM)
envDfPred2$stdForest <- (envDfPred2$forest - mean(envDf15Sampled$forest))/sd(envDf15Sampled$forest)
envDfPred2$stdSlope <- (envDfPred2$slope - mean(envDf15Sampled$slope))/sd(envDf15Sampled$slope)
envDfPred2$stdSurf_coarse <- (envDfPred2$surf_coarse - mean(envDf15Sampled$surf_coarse, na.rm=TRUE))/sd(envDf15Sampled$surf_coarse, na.rm=TRUE)
# replace NA with mean values for surf_coarse
envDfPred2$stdSurf_coarse[is.na(envDfPred2$stdSurf_coarse)]  <- mean(envDf15Sampled$surf_coarse, na.rm=TRUE)
# F to C in air temp
envDfPred2$stdAirTemp <- (envDfPred2$tminFuture - mean((envDf15Sampled$tmin-32)*5/9))/(sd(envDf15Sampled$tmin-32)*5/9)
envDfPred2$stdPrecip <- (envDfPred2$precip_annual - mean(envDf15Sampled$precip_annual))/sd(envDf15Sampled$precip_annual)


## prediction

# fill an empty array with prediction
logitPredArray2 <- array(NA, dim = c(nrow(envDfPred2), n.sims))  # empty array
fHuc <- envDfPred2$fhuc10   # this is needed for subsetting below

for (i in 1:nrow(envDfPred2)){
  for (j in 1:n.sims){
    logitPredArray2[i,j] <- coefArray[j,1,fHuc[i]] +   # basin-specific intercept
      sum(envDfPred2[i,58:63]*coefArray[j, 2:n.param, fHuc[i]])   # coef: make sure numbers are right
  }
}

# transform to probability scale
predArray2 <- inv.logit(logitPredArray2)

# mean & 95% CI of occ. prob.
envDfPred2$probMean <- apply(predArray2,1,mean)
envDfPred2$probLow <- apply(predArray2,1,quantile,probs=c(0.025),na.rm=TRUE)
envDfPred2$probHigh <- apply(predArray2,1,quantile,probs=c(0.975),na.rm=TRUE)

write.csv(envDfPred2, file="envDfPred2.csv", row.names=FALSE)



# 3.Prediction when precipitation decreases by 25%
## prepare df for prediction

### make another df so as not write over
envDfPred3 <- envDf15
envDfPred3 <- merge(envDfPred3, AllHuc10, all.x=TRUE)
envDfPred3$fhuc10[ is.na(envDfPred3$fhuc10) ] <- n.huc + 1  # +1 to indicate a newly added group

### 75% of current precipitation
envDfPred3$precipFuture <- envDfPred3$precip_annual*0.75

### standardize covariates
envDfPred3$stdArea <- (envDfPred3$TotDASqKM - mean(envDf15Sampled$TotDASqKM))/sd(envDf15Sampled$TotDASqKM)
envDfPred3$stdForest <- (envDfPred3$forest - mean(envDf15Sampled$forest))/sd(envDf15Sampled$forest)
envDfPred3$stdSlope <- (envDfPred3$slope - mean(envDf15Sampled$slope))/sd(envDf15Sampled$slope)
envDfPred3$stdSurf_coarse <- (envDfPred3$surf_coarse - mean(envDf15Sampled$surf_coarse, na.rm=TRUE))/sd(envDf15Sampled$surf_coarse, na.rm=TRUE)
# replace NA with mean values for surf_coarse
envDfPred3$stdSurf_coarse[is.na(envDfPred3$stdSurf_coarse)]  <- mean(envDf15Sampled$surf_coarse, na.rm=TRUE)
envDfPred3$stdAirTemp <- (envDfPred3$tmin - mean(envDf15Sampled$tmin))/sd(envDf15Sampled$tmin)
envDfPred3$stdPrecip <- (envDfPred3$precipFuture - mean(envDf15Sampled$precip_annual))/sd(envDf15Sampled$precip_annual)


## prediction

# fill an empty array with prediction
logitPredArray3 <- array(NA, dim = c(nrow(envDfPred3), n.sims))  # empty array
fHuc <- envDfPred3$fhuc10   # this is needed for subsetting below

for (i in 1:nrow(envDfPred3)){
  for (j in 1:n.sims){
    logitPredArray3[i,j] <- coefArray[j,1,fHuc[i]] +   # basin-specific intercept
      sum(envDfPred3[i,57:62]*coefArray[j, 2:n.param, fHuc[i]])   # coef: make sure numbers are right
  }
}

# transform to probability scale
predArray3 <- inv.logit(logitPredArray3)

# mean & 95% CI of occ. prob.
envDfPred3$probMean <- apply(predArray3,1,mean)
envDfPred3$probLow <- apply(predArray3,1,quantile,probs=c(0.025),na.rm=TRUE)
envDfPred3$probHigh <- apply(predArray3,1,quantile,probs=c(0.975),na.rm=TRUE)

write.csv(envDfPred3, file="envDfPred3.csv", row.names=FALSE)



# 4.Prediction when forest cover decreases by 25%
## prepare df for prediction

### make another df so as not write over
envDfPred4 <- envDf15
envDfPred4 <- merge(envDfPred4, AllHuc10, all.x=TRUE)
envDfPred4$fhuc10[ is.na(envDfPred4$fhuc10) ] <- n.huc + 1  # +1 to indicate a newly added group

### 75% of current forest
envDfPred4$forestFuture <- envDfPred4$forest*0.75

### standardize covariates
envDfPred4$stdArea <- (envDfPred4$TotDASqKM - mean(envDf15Sampled$TotDASqKM))/sd(envDf15Sampled$TotDASqKM)
envDfPred4$stdForest <- (envDfPred4$forestFuture - mean(envDf15Sampled$forest))/sd(envDf15Sampled$forest)
envDfPred4$stdSlope <- (envDfPred4$slope - mean(envDf15Sampled$slope))/sd(envDf15Sampled$slope)
envDfPred4$stdSurf_coarse <- (envDfPred4$surf_coarse - mean(envDf15Sampled$surf_coarse, na.rm=TRUE))/sd(envDf15Sampled$surf_coarse, na.rm=TRUE)
# replace NA with mean values for surf_coarse
envDfPred4$stdSurf_coarse[is.na(envDfPred4$stdSurf_coarse)]  <- mean(envDf15Sampled$surf_coarse, na.rm=TRUE)
envDfPred4$stdAirTemp <- (envDfPred4$tmin - mean(envDf15Sampled$tmin))/sd(envDf15Sampled$tmin)
envDfPred4$stdPrecip <- (envDfPred4$precip_annual - mean(envDf15Sampled$precip_annual))/sd(envDf15Sampled$precip_annual)


## prediction

# fill an empty array with prediction
logitPredArray4 <- array(NA, dim = c(nrow(envDfPred4), n.sims))  # empty array
fHuc <- envDfPred4$fhuc10   # this is needed for subsetting below

for (i in 1:nrow(envDfPred4)){
  for (j in 1:n.sims){
    logitPredArray4[i,j] <- coefArray[j,1,fHuc[i]] +   # basin-specific intercept
      sum(envDfPred4[i,57:62]*coefArray[j, 2:n.param, fHuc[i]])   # coef: make sure numbers are right
  }
}

# transform to probability scale
predArray4 <- inv.logit(logitPredArray4)

# mean & 95% CI of occ. prob.
envDfPred4$probMean <- apply(predArray4,1,mean)
envDfPred4$probLow <- apply(predArray4,1,quantile,probs=c(0.025),na.rm=TRUE)
envDfPred4$probHigh <- apply(predArray4,1,quantile,probs=c(0.975),na.rm=TRUE)

write.csv(envDfPred4, file="envDfPred4.csv", row.names=FALSE)


## Combine the mean response from the four scenarios

envDfPredMeanRes <- cbind(envDfPred[,c("FEATUREID","TotDASqKM","probMean")],
                          envDfPred2$probMean, envDfPred3$probMean, envDfPred4$probMean)
names(envDfPredMeanRes)[names(envDfPredMeanRes)=="probMean"] <- "probMean1"
names(envDfPredMeanRes)[names(envDfPredMeanRes)=="envDfPred2$probMean"] <- "probMean2"
names(envDfPredMeanRes)[names(envDfPredMeanRes)=="envDfPred3$probMean"] <- "probMean3"
names(envDfPredMeanRes)[names(envDfPredMeanRes)=="envDfPred4$probMean"] <- "probMean4"

# select catchment <= 15 km2, so as to merge with > 15 km2 analysis
envDfPredMeanResUnder15sqkm <- subset(envDfPredMeanRes, TotDASqKM <= 15)

write.csv(envDfPredMeanRes, file="envDfPredMeanResUnder15sqkm.csv", row.names=FALSE)


### Wenger et al. (2013) Global Change Biology
nreps <- 5

for(j in 1:nreps){
  predtable <- matrix(rep(0,nreps*nrow(data.pred.std)),ncol=nreps) # create empty matrix for preds
  randparm <- mvrnorm(n=10,mu=fixef(final.model),Sigma=vcov(final.model))
  mm <- model.matrix(terms(final.model),data.fit.std)
  ### use known random effect value where available
  randeff <- ranef(final.model)$fhuc10[match(as.character(data.pred.std$fhuc10),row.names(ranef(final.model)$fhuc10[1])),1]
  ### For other hucs, generate random value and apply it to each record
  randhuc8 <- rnorm(length(unique(data.pred.std$fhuc10)),0,as.numeric(VarCorr(final.model)[1])^.5)
  names(randhuc8) <- as.character(unique(data.pred.std$fhuc10))
  randeff[is.na(randeff)] <- randhuc8[match(as.character(data.pred.std$fhuc10),names(randhuc8))]
  predtable[,j] <- round(plogis(0,location=((mm%*%randparm)+randeff),scale=1,lower.tail=F),3)
}

mySumm <- function(.) { s <- sigma(.)
c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }

shinobu <- bootMer(final.model, mySumm, nsim=2)



# For Ana

## mean & 95% CI occ. prob.
#webOccProb <- envDfPred[ , c("FEATUREID","HUC_10","probMean","probLow","probHigh")]

## fit data set
#env.fit.df <- data.fit3

## prediction data set
env.pred.df <- data.pred

## prediction values
occ.pred <-

  ## coef
  coef.array <- coefArray

## HUC10 ID
Huc10.df <- AllHuc10

## Mean & SD for standardization
value = c("mean","sd")
log.area = c(mean(data.fit3$log.area), sd(data.fit3$log.area))
asin.forest = c(mean(data.fit3$asin.forest), sd(data.fit3$asin.forest))
sqrt.slope = c(mean(data.fit3$sqrt.slope), sd(data.fit3$sqrt.slope))
precip = c(mean(data.fit3$precip), sd(data.fit3$precip))
tmin = c(mean(data.fit3$tmin), sd(data.fit3$tmin))
drainC = c(mean(data.fit3$drainC, na.rm=TRUE), sd(data.fit3$drainC, na.rm=TRUE))
standardized.df <- data.frame(value, log.area, asin.forest, sqrt.slope, precip, tmin, drainC)

## packaging the above objects for Ana
library(gdata)
keep('env.pred.df', 'coef.array', 'Huc10.df', 'standardized.df', sure=TRUE)




## ROC AUC


#####################################
aucFun <- function(.){
  library(boot)
  library(ROCR)
  pred <- inv.logit(predict(.))
  pROCR <- prediction(pred, .@resp$y)
  AUC <- unlist(performance(pROCR, "auc")@y.values)
  ROC <- unlist(performance(pROCR, "tpr", "fpr")@y.values)
  return(AUC)
}

rocFun <- function(.){
  library(boot)
  library(ROCR)
  pred <- inv.logit(predict(.))
  pROCR <- prediction(pred, .@resp$y)
  ROC <- unlist(performance(pROCR, "tpr", "fpr")@y.values)
  return(ROC)
}

coefFun <- function(fit){
  return(c(fixef(fit),unlist(VarCorr(fit))))
}

test <- aucFun(glmm.M32)

system.time(auc.boot <- bootMer(glmm.M32, aucFun, nsim=4, use.u=F, seed=3453, parallel="multicore", ncpus=4))
(bCI <- boot.ci(auc.boot, index =c(1,1), type="norm"))
hist(auc.boot$t)
abline(v = auc.boot$t0, col='red', lwd=3)

system.time(roc.boot <- bootMer(glmm.M32, rocFun, nsim=4, use.u=F, seed=3453, parallel="multicore", ncpus=4))
(bCI <- boot.ci(roc.boot, index =c(1,1), type="norm"))
hist(roc.boot$t)
abline(v = roc.boot$t0, col='red', lwd=3)

system.time(coef.boot <- bootMer(glmm.M32, coefFun, nsim=4, use.u=F, seed=3453, parallel="multicore", ncpus=4))
coef.boot
(bCI <- boot.ci(coef.boot, index =c(1,1), type="norm"))
hist(auc.boot$t)
abline(v = auc.boot$t0, col='red', lwd=3)

glmm.M32v <- glmer(formula(glmm.M32), family = binomial(link = "logit"), data = df.valid, control = glmerControl(optimizer="bobyqa"))
system.time(auc.boot.M32v <- bootMer(glmm.M32v, aucFun, nsim=4, use.u=F, seed=3453, parallel="multicore", ncpus=4))
(bCI.M32v <- boot.ci(auc.boot.M32v, index =c(1,1), type="norm"))
hist(auc.boot.M32v$t)
abline(v = auc.boot.M32v$t0, col='red', lwd=3)

system.time(roc.boot.M32 <- bootMer(glmm.M32, rocFun, nsim=1000, use.u=F, seed=3453, parallel="multicore", ncpus=4))
system.time(roc.boot.M32v <- bootMer(glmm.M34v, rocFun, nsim=1000, use.u=F, seed=3453, parallel="multicore", ncpus=4))

system.time(auc.boot.M32 <- bootMer(glmm.M32, aucFun, nsim=1000, use.u=F, seed=3453, parallel="multicore", ncpus=4))
system.time(auc.boot.M32v <- bootMer(glmm.M32v, aucFun, nsim=1000, use.u=F, seed=3453, parallel="multicore", ncpus=4))


aucSummary <- function(auc.boot, Mean=T, CI=c(0.025, 0.975), SE=F){
  library(AUC)
  auc.out <- auc.boot$t
  auc.CI <- quantile(auc.out, probs=CI, na.rm=T)
  auc.mean <- NA
  auc.se <- NA
  if(Mean == T){
    auc.mean <- mean(auc.out, na.rm=T)
  }
  if(SE == T) {
    auc.se <- sd(auc.out, na.rm=T)/sqrt(length(auc.out))
  }
  auc.summary <- data.frame(auc.boot$t0, auc.mean, auc.CI[1], auc.CI[2], auc.se)
  names(auc.summary) <- c('AUC', 'Mean', 'LCI', 'UCI', 'SE')
  row.names(auc.summary) <- deparse(substitute(auc.boot))
  return(auc.summary)
} # end function

auc32 <- aucSummary(auc.boot.M32, SE=T)
auc32v <- aucSummary(auc.boot.M32v, SE=T)

auc.comp <- data.frame(rbind(auc35, auc51, auc35v, auc51v))
auc.comp$Model <- as.factor(row.names(auc.comp))
row.names(auc.comp) <- c("Environmental", "Climate", "Environmental Validation", "Climate Validation")

se <- ggplot(auc.comp, aes(Model, Mean,
                           ymin = LCI, ymax=UCI, colour = Model))
se + geom_linerange() + geom_pointrange() + geom_point(aes(Model, AUC), colour = 'black')

########################################

par(mfrow = c(1,1))
plot(perf35v, lty=3, col="blue")
abline(0, 1)
plot(perf51v, lty=3, col="green", add=TRUE)
plot(perf35, lty=3, col="black", add=TRUE)
plot(perf51, lty=3, col="red", add=TRUE)
plot(perf35, avg="vertical", lwd=2, col="black", spread.estimate="stderror", plotCI.lwd=2, add=TRUE)
plot(perf35v, avg="vertical", lwd=2, col="blue", spread.estimate="stderror", plotCI.lwd=2, add=TRUE)
plot(perf51, avg="vertical", lwd=2, col="red", spread.estimate="stderror", plotCI.lwd=2, add=TRUE)
plot(perf51v, avg="vertical", lwd=2, col="green", spread.estimate="stderror", plotCI.lwd=2, add=TRUE)
legend(0.55, 0.4, c('Flow (Training)','Flow (Validation)', 'Climate (Training)', 'Climate (Validation)'), col=c('black','blue', 'red', 'green'), lwd=2)

rbind(auc35, auc35v, auc51, auc51v) # output table

quantile(unlist(performance(p35, "auc")@y.values), c(0.025, 0.5, 0.975))
quantile(unlist(performance(p35, "rmse")@y.values), c(0.025, 0.5, 0.975))


pred.fit <- inv.logit(predict(glmm.M35, df.fit, allow.new.levels = TRUE, interval="prediction"))
auc(sensitivity(pred.fit, as.factor(df.fit$pres))) # 0.681
auc(specificity(pred.fit, as.factor(df.fit$pres))) # 0.694
auc(accuracy(pred.fit, as.factor(df.fit$pres)))    # 0.687
auc(roc(pred.fit, as.factor(df.fit$pres)))         # 0.875
auc(roc(as.numeric(pred.list$predictions[[1]]), as.factor(pred.list$observed[[1]])))
mean(unlist(performance(p35, "auc")@y.values))

par(mfrow=c(1, 2))
hist(unlist(pred.M35$predictions), freq=F, breaks=20)
hist(pred.fit, freq=F, breaks=20)
par(mfrow=c(1,1))

par(mfrow = c(2,2))
plot(sensitivity(pred.fit, as.factor(df.fit$pres)))
plot(specificity(pred.fit, as.factor(df.fit$pres)))
plot(accuracy(pred.fit, as.factor(df.fit$pres)))
plot(roc(pred.fit, as.factor(df.fit$pres)))
par(mfrow=c(1,1))

# AUC for climate model
pred.fit2 <- inv.logit(predict(glmm.M51, df.fit, allow.new.levels = TRUE))
auc(sensitivity(pred.fit2, as.factor(df.fit$pres))) # 0.679
auc(specificity(pred.fit2, as.factor(df.fit$pres))) # 0.691
auc(accuracy(pred.fit2, as.factor(df.fit$pres)))    # 0.685
auc(roc(pred.fit2, as.factor(df.fit$pres)))         # 0.870



## Semivariograms (Torgeogram)

df.fit.all$psi.climate <- inv.logit(predict(glmm.M51, df.fit.all, allow.new.levels = TRUE))
df.fit.all$psi.stream <- inv.logit(predict(glmm.M35, df.fit.all, allow.new.levels = TRUE))

data.geo <- merge(df.fit.all[ , c("FEATUREID", "psi.stream", "psi.climate", "pres")], data[ , c("FEATUREID", "Latitude", "Longitude")], by="FEATUREID", all.x=T)
str(data.geo)
summary(data.geo)

# dists <- dist(data.geo[ , c("Latitude", "Longitude")])
# summary(dists)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 0.000266 0.822900 1.300000 1.356000 1.834000 3.975000

rm(dists) # vector length = 331,286,670 (kills laptop)

library(geoR)
df.geo <- as.geodata(data.geo, coords.col=c(5,6), data.col=c(2,3))
str(df.geo)

#plot(df.geo)
breaks = seq(0, 2, length.out = 200)
Torge <- variog(df.geo, breaks=breaks, pairs.min=10)

v1.summary <- cbind(c(1:10), Torge$v, Torge$n)
colnames(v1.summary) <- c("lag", "semi-variance", "semi-variance", "# of pairs")
v1.summary
plot(Torge, type='b', pch = c(1,2))
rm(Torge)

hawkins.cressie <- variog(df.geo, estimator.type="modulus")
plot(hawkins.cressie)
rm(hawkins.cressie)

# just look at small distances (within ~10km)
breaks = seq(0, 0.1, length.out = 50)
Torge.sm <- variog(df.geo, breaks=breaks)
v1.summary <- cbind(c(1:(length(breaks)-1)), Torge.sm$v, Torge.sm$n)
colnames(v1.summary) <- c("lag", "semi-variance", "semi-variance", "# of pairs")
v1.summary
plot(Torge.sm, type='l')
rm(Torge.sm)

# Semivariogram of P/A data

df.geo.pres <- as.geodata(data.geo, coords.col=c(5,6), data.col=c(2,3,4))
breaks = seq(0, 1, length.out = 25)
Torge <- variog(df.geo.pres, breaks=breaks, pairs.min=10)

v1.summary <- cbind(c(1:10), Torge$v, Torge$n)
colnames(v1.summary) <- c("lag", "semi-variance", "semi-variance", "# of pairs")
v1.summary
plot(Torge, type='b', pch = c(1,2,3))
rm(Torge)

# Semivariograms for observed P/A compared with predicted occupancy

# Semivariograms for predictor variables


## Prediction for all catchments in Future Climates

# changed UpstreamStats to UpstreamStatsPred in the below file before importing
load("Data/nenyUMassProjectionsTmax.Rdata")

summary(LocalStatsPred)

LocalStatsPred <- LocalStatsPred %>%
  dplyr::rename(featureid = FEATUREID)

df.fit.all <- merge(data.fit.std, LocalStatsPred, by = "featureid", all.x = TRUE)

df.fit.all$tmax <- (df.fit.all$tmax2010rcp45 - mean(df.fit.all$tmax2010rcp45, na.rm = TRUE)) / sd(df.fit.all$tmax2010rcp45, na.rm = TRUE)

# Simple model for Meeting
glmm.simple <- glmer(pres ~ log.area  + precip  + tmax + asin.forest + (1 + asin.forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))
summary(glmm.simple)

glmm.streamT.flow2 <- glmer(pres ~ log.flow + tmax.stream + rise.slope + asin.forest + (1 + asin.forest|fhuc10), family = binomial(link = "logit"), data = df.fit, control = glmerControl(optimizer="bobyqa"))
summary(glmm.streamT.flow2)


tmax.mean <- mean(df.fit.all$tmax2010rcp45, na.rm = TRUE)
tmax.sd <- sd(df.fit.all$tmax2010rcp45, na.rm = TRUE)

df.fit.all$tmax <- (df.fit.all$tmax2010rcp45 - tmax.mean) / tmax.sd
df.fit.all$pred.2010rcp45 <- inv.logit(predict(glmm.simple, df.fit.all, allow.new.levels = TRUE))

df.fit.all$tmax <- (df.fit.all$tmax2030rcp45 - tmax.mean) / tmax.sd
df.fit.all$pred.2030rcp45 <- inv.logit(predict(glmm.simple, df.fit.all, allow.new.levels = TRUE))

df.fit.all$tmax <- (df.fit.all$tmax2080rcp45 - tmax.mean) / tmax.sd
df.fit.all$pred.2080rcp45 <- inv.logit(predict(glmm.simple, df.fit.all, allow.new.levels = TRUE))

df.fit.all$tmax <- (df.fit.all$tmax2030rcp85 - tmax.mean) / tmax.sd
df.fit.all$pred.2030rcp85 <- inv.logit(predict(glmm.simple, df.fit.all, allow.new.levels = TRUE))

df.fit.all$tmax <- (df.fit.all$tmax2080rcp85 - tmax.mean) / tmax.sd
df.fit.all$pred.2080rcp85 <- inv.logit(predict(glmm.simple, df.fit.all, allow.new.levels = TRUE))


fits <- merge(UpstreamStats[ , c("FEATUREID", "Latitude", "Longitude")], df.fit.all[ , c("FEATUREID", "pred.2010rcp45", "pred.2030rcp45", "pred.2080rcp45", "pred.2030rcp85", "pred.2080rcp85")], by = "FEATUREID")

save(fits, file = "Predictions_2014-04-23.RData")


auc(roc(pred.fit.climate, as.factor(df.fit$pres))) # 0.842
auc(roc(pred.fit.stream, as.factor(df.fit$pres)))  # 0.850

deltas <- pred.fit.climate.all - pred.fit.stream.all

summary(deltas)
sd(deltas)
quantile(deltas, probs = c(0.025, 0.5, 0.975))

fits <- data.frame(FEATUREID = df.fit.all$FEATUREID, pres = df.fit.all$pres, deltas = deltas, psi.clim = pred.fit.climate.all, psi.stream = pred.fit.stream.all)

fits <- merge(fits, LocalStats[ , c("FEATUREID", "Latitude", "Longitude")], by = "FEATUREID")
names(fits) <- c("FEATUREID", "pres", "deltas", "psi.clim", "psi.stream", "lat", "lon")

fits <- merge(fits, df.fit.all, by = "FEATUREID")
fits <- merge(fits, data.fit2.all, by = "FEATUREID")

save(fits, file = "Predictions.RData")

summary(fits)
dim(fits)

ggplot(data = fits, aes(psi.clim, psi.stream)) + geom_point()

par(mar = c(3.5,3,2,1), mgp = c(2,.7,0), tck = -.02)
plot(fits$psi.clim, fits$psi.stream, xlab = "Climate Predicted Occupancy",
     ylab = "Stream Temperature/Flow Predicted Occupancy")
abline(0, 1, col = "red")

par(mfrow = c(1, 3))
plot(fits$forest, fits$psi.clim)
lines(smooth.spline(fits$forest, fits$psi.clim), col = "red")
plot(fits$asin.forest, fits$psi.clim)
lines(seq(-3, 2, by = 0.1), inv.logit(fixef(glmm.simple)[1] + fixef(glmm.simple)["asin.forest"]*(seq(-3, 2, by = 0.1))), col = "red")
plot(fits$asin.forest, fits$psi.stream)
lines(seq(-3, 2, by = 0.1), inv.logit(fixef(glmm.streamT.flow2)[1] + fixef(glmm.streamT.flow2)["asin.forest"]*(seq(-3, 2, by = 0.1))), col = "red")
par(mfrow = c(1,1))

fixed <- data.frame(c(m1[1:3], NA, m1[4], NA, m1[5]), c(m2[1], NA, NA, m2[2], m2[3:5]))
names(fixed) <- c("Climate", "Stream")
row.names(fixed) <- c("Intercept", "log.area", "precip", "log.flow", "tmax", "rise.slope", "asin.forest")
fixed


## Save objects and data for Ana

# effect size plots
# AUC boxplots
# ROC curves (validation)
# Variograms (data, 2 models)


#means <- apply(data.fit3[ ,2:18], MARGIN = 2, FUN = function(x){mean(x, na.rm = TRUE)})
#stdevs <- apply(data.fit3[ ,2:18], MARGIN = 2, FUN = function(x){sd(x, na.rm = TRUE)})

#save(data.fit2, data.fit3, means, stdevs, df.fit, glmm.simple, glmm.streamT.flow, glmm.streamT.flow2, glmm.rising, file = "Northeast_Model_Output.RData")


