tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "patchwork", "dotenv"))

targets_obs <- list(
  tar_target(obs_madfw_file, "data/obs/madfw-ebt-raw.csv", format = "file"),
  tar_target(obs_madfw_raw, {
    # source: 20181206 - MA Wildlife Data/MassWildlife coldwater spp/Brook_Trout_nat_prod.shp
    # note: presence only
    read_csv(obs_madfw_file, col_types = cols(
      .default = col_character(),
      latitude = col_double(),
      longitude = col_double(),
      year = col_double()
    )) %>%
      select(latitude, longitude, year)
  }),
  tar_target(obs_madfw, {
    # look up featureid for each observation
    con <- db_connect()

    find_featureid <- function (latitude, longitude) {
      # helper function to find the catchment for a given lat/lon
      x <- DBI::dbGetQuery(
        con,
        "select featureid::int from catchments where st_contains(geom, st_setsrid(st_makepoint($1, $2), 4326))",
        param = list(longitude, latitude)
      )
      stopifnot(nrow(x) == 1)
      x$featureid
    }

    x <- obs_madfw_raw %>%
      rowwise() %>%
      mutate(
        featureid = find_featureid(latitude, longitude)
      ) %>%
      ungroup()

    DBI::dbDisconnect(con)

    stopifnot(all(!is.na(x$featureid)))

    x
  }),
  tar_target(obs_madfw_catchments, {
    gis_catchments %>%
      inner_join(obs_madfw, by = "featureid")
  }),
  tar_target(obs_madfw_map, {
    obs_madfw_catchments %>%
      mutate(presence = factor(1)) %>%
      ggplot() +
      geom_sf(aes(color = presence), alpha = 1, size = 1) +
      geom_sf(data = filter(gis_states, state_abbr == "MA"), fill = NA) +
      scale_color_manual(
        NULL,
        values = c("0" = "orangered", "1" = "deepskyblue"),
        labels = c("0" = "Absence", "1" = "Presence")
      ) +
      guides(
        color = guide_legend(override.aes = list(alpha = 1, size = 1))
      ) +
      labs(title = "MADFW EBT Dataset (Presence Only)")
  }),

  tar_target(obs_regional_file, "data/obs/regional_occupancy_data.csv", format = "file"),
  tar_target(obs_regional, {
    read_csv(obs_regional_file, col_types = cols(
      featureid = col_double(),
      species = col_character(),
      catch = col_double(),
      year_min = col_double(),
      year_max = col_double()
    )) %>%
      mutate(presence = ifelse(catch > 0, 1, catch)) %>%
      filter(!is.na(presence))
  }),
  tar_target(obs_regional_catchments, {
    gis_catchments %>%
      inner_join(obs_regional, by = "featureid")
  }),
  tar_target(obs_regional_map, {
    obs_regional_catchments %>%
      ggplot() +
      geom_sf(aes(color = factor(presence)), alpha = 0.5, size = 0.5) +
      geom_sf(data = gis_states, fill = NA) +
      scale_color_manual(
        NULL,
        values = c("0" = "orangered", "1" = "deepskyblue"),
        labels = c("0" = "Absence", "1" = "Presence")
      ) +
      guides(
        color = guide_legend(override.aes = list(alpha = 1, size = 1))
      ) +
      labs(title = "Regional Dataset")
  }),
  tar_target(obs_regional_states, {
    obs_regional_catchments %>%
      st_intersection(gis_states) %>%
      st_drop_geometry()
  }),
  tar_target(obs_regional_states_plot, {
    p1 <- obs_regional_states %>%
      ggplot(aes(ordered(state_abbr, levels = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "MD", "WV", "VA")))) +
      geom_bar(aes(fill = factor(presence))) +
      scale_fill_manual(
        NULL,
        values = c("0" = "orangered", "1" = "deepskyblue"),
        labels = c("0" = "Absence", "1" = "Presence")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "State", y = "# Catchments")

    p2 <- obs_regional_states %>%
      ggplot(aes(ordered(state_abbr, levels = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "MD", "WV", "VA")))) +
      geom_bar(aes(fill = factor(presence)), position = "fill") +
      scale_fill_manual(
        NULL,
        values = c("0" = "orangered", "1" = "deepskyblue"),
        labels = c("0" = "Absence", "1" = "Presence")
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::percent, expand = expansion()) +
      labs(x = "State", y = "% Catchments")

    wrap_plots(p1, p2, nrow = 1) +
      plot_annotation(title = "Presence/Absence Summary by State") +
      plot_layout(guides = "collect")
  }),
  tar_target(obs_regional_huc10, {
    gis_huc10_pnt %>%
      left_join(
        obs_regional %>%
          left_join(select(huc_catchment, featureid, huc10), by = "featureid") %>%
          group_by(huc10) %>%
          summarise(
            n = n(),
            presence = mean(presence),
            .groups = "drop"
          ),
        by = c("huc10" = "huc10")
      )
  }),
  tar_target(obs_regional_huc10_map, {
    p1 <- obs_regional_huc10 %>%
      ggplot() +
      geom_sf(aes(color = n), size = 1.5) +
      geom_sf(data = gis_states, fill = NA) +
      scale_color_viridis_c() +
      labs(title = "# Catchments Observed")
    p2 <- obs_regional_huc10 %>%
      ggplot() +
      geom_sf(aes(color = presence), size = 1.5) +
      geom_sf(data = gis_states, fill = NA) +
      scale_color_viridis_c() +
      labs(title = "Mean Presence")

    wrap_plots(p1, p2, nrow = 1) +
      plot_annotation(title = "Regional Presence Dataset by HUC10")
  }),

  tar_target(obs_presence_all, {
    bind_rows(
      madfw = obs_madfw %>%
        transmute(featureid, year_min = year, year_max = year, presence = 1),
      regional = obs_regional %>%
        select(featureid, year_min, year_max, presence),
      .id = "source"
    )
  }),
  tar_target(obs_presence_dups, {
    obs_presence_all %>%
      add_count(featureid) %>%
      filter(n > 1)
  }),
  tar_target(obs_presence, {
    # drop duplicates using max(presence)
    obs_presence_all %>%
      group_by(featureid) %>%
      summarise(presence = max(presence))
  })
)