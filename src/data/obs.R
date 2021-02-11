# import observational (presence/absence) data to working directory
# -> {wd}/data-obs.rds

library(tidyverse)
library(glue)
library(janitor)
library(sf)

source("src/functions.R")

config <- load_config()

gis <- read_rds(file.path(config$wd, "gis.rds"))
huc <- read_rds(file.path(config$wd, "data-huc.rds"))

# load: regional ----------------------------------------------------------

df_regional <- read_csv("data/obs/regional_occupancy_data.csv", col_types = cols(
  featureid = col_double(),
  species = col_character(),
  catch = col_double(),
  year_min = col_double(),
  year_max = col_double()
)) %>%
  mutate(presence = ifelse(catch > 0, 1, catch)) %>%
  select(featureid, presence) %>%
  filter(!is.na(presence))

sf_catch_regional <- gis$catchments %>%
  inner_join(df_regional, by = "featureid")

sf_catch_regional %>%
  ggplot() +
  geom_sf(aes(color = factor(presence)), alpha = 0.5, size = 0.5) +
  geom_sf(data = gis$states, fill = NA) +
  scale_color_manual("Presence\n/Absence", values = c("0" = "orangered", "1" = "deepskyblue")) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 1))
  )

mapview::mapview(sf_catch_regional, zcol = "presence", cex = 6, col.regions = c("orangered", "deepskyblue"))

df_regional_states <- sf_catch_regional %>%
  st_intersection(gis$states) %>%
  as_tibble() %>%
  select(-geom)

df_regional_states %>%
  ggplot(aes(ordered(state_abbr, levels = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "MD", "WV", "VA")))) +
  geom_bar(aes(fill = factor(presence))) +
  scale_fill_manual("Presence/\nAbsence", values = c("0" = "orangered", "1" = "deepskyblue")) +
  labs(x = "State", y = "# Catchments", title = "Presence/Absence Summary by State")

df_regional_states %>%
  ggplot(aes(ordered(state_abbr, levels = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "MD", "WV", "VA")))) +
  geom_bar(aes(fill = factor(presence)), position = "fill") +
  scale_fill_manual("Presence/\nAbsence", values = c("0" = "orangered", "1" = "deepskyblue")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "State", y = "% Catchments", title = "Presence/Absence Summary by State (%)")

# by huc
gis$huc10 %>%
  left_join(
    df_regional %>%
      left_join(select(huc, featureid, huc10), by = "featureid") %>%
      group_by(huc10) %>%
      summarise(
        n = n(),
        presence = mean(presence),
        .groups = "drop"
      ),
    by = c("HUC10" = "huc10")
  ) %>%
  ggplot() +
  geom_sf(aes(color = n), size = 1.5) +
  geom_sf(data = gis$states, fill = NA) +
  scale_color_viridis_c() +
  labs(title = "# Catchments Observed per HUC10")

gis$huc10 %>%
  left_join(
    df_regional %>%
      left_join(select(huc, featureid, huc10), by = "featureid") %>%
      group_by(huc10) %>%
      summarise(
        n = n(),
        presence = mean(presence),
        .groups = "drop"
      ),
    by = c("HUC10" = "huc10")
  ) %>%
  ggplot() +
  geom_sf(aes(color = presence), size = 1.5) +
  geom_sf(data = gis$states, fill = NA) +
  scale_color_viridis_c() +
  labs(title = "Mean Presence per HUC10")


# load: madfw -------------------------------------------------------------

df_madfw <- read_csv("data/obs/madfw-ebt.csv", col_types = cols(
  latitude = col_double(),
  longitude = col_double(),
  year = col_double(),
  featureid = col_double()
)) %>%
  select(featureid) %>%
  distinct() %>%
  mutate(
    presence = 1
  )

tabyl(df_madfw, presence)

sf_madfw_points <- gis$catchments %>%
  inner_join(df_madfw, by = "featureid")

sf_madfw_points %>%
  ggplot() +
  geom_sf(aes(color = factor(presence)), alpha = 1, size = 1) +
  geom_sf(data = filter(gis$states, state_abbr == "MA"), fill = NA) +
  scale_color_manual("Presence\n/Absence", values = c("0" = "orangered", "1" = "deepskyblue")) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 1))
  )


# merge -------------------------------------------------------------------

df_merge <- bind_rows(
    df_regional,
    df_madfw
  )

merge_dups <- df_merge %>%
  distinct(featureid, presence) %>%
  filter(duplicated(featureid)) %>%
  pull(featureid)

if(length(merge_dups) > 0) {
  warning(
    glue("{length(merge_dups)} featureids have duplicate observations between regional and MADFW datasets that are not equal ({str_c(merge_dups, collapse = ', ')}). Setting all to presence=1...")
  )
}

df <- df_merge %>%
  group_by(featureid) %>%
  summarise(presence = max(presence))

stopifnot(sum(duplicated(df$featureid)) == 0)


# export ------------------------------------------------------------------

saveRDS(df, file.path(config$wd, "data-obs.rds"))
