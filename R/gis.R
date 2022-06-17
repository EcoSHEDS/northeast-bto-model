tar_option_set(packages = c("tidyverse", "sf", "dotenv"))

targets_gis <- list(
  tar_target(gis_catchments, {
    con <- db_connect()
    x <- st_read(con, query = "select featureid, geom_pour as geom from truncated_flowlines;") %>%
      mutate(featureid = as.numeric(featureid))
    DBI::dbDisconnect(con)
    x
  }),
  tar_target(gis_catchments_map, {
    gis_catchments %>%
      sample_frac(0.01) %>%
      ggplot() +
      geom_sf()
  }),

  tar_target(gis_huc8_poly, {
    con <- db_connect()
    x <- st_read(con, query = "select huc8, geom as geom from wbdhu8;")
    DBI::dbDisconnect(con)
    x %>%
      filter(huc8 %in% unique(huc_catchment$huc8)) %>%
      st_transform("EPSG:5070") %>%
      st_make_valid() %>%
      st_simplify(dTolerance = 1000)
  }),
  tar_target(gis_huc8_pnt, {
    con <- db_connect()
    x <- st_read(con, query = "select huc8, st_centroid(geom) as geom from wbdhu8;")
    DBI::dbDisconnect(con)
    x %>%
      filter(huc8 %in% unique(huc_catchment$huc8))
  }),
  tar_target(gis_huc10_pnt, {
    con <- db_connect()
    x <- st_read(con, query = "select huc10, st_centroid(geom) as geom from wbdhu10;")
    DBI::dbDisconnect(con)
    x %>%
      filter(huc10 %in% unique(huc_catchment$huc10))
  }),
  tar_target(gis_huc10_pnt_map, {
    gis_huc10_pnt %>%
      ggplot() +
      geom_sf()
  }),
  tar_target(gis_huc10_pnt_map_states, {
    gis_huc10_pnt %>%
      ggplot() +
      geom_sf(data = gis_states, fill = NA) +
      geom_sf()
  }),

  tar_target(gis_states, {
    USAboundaries::us_states() %>%
      filter(
        stusps %in% c("CT", "DC", "DE", "MA", "MD", "ME", "NH", "NJ", "NY", "PA", "RI", "VA", "VT", "WV")
      )
  }),
  tar_target(gis_states_map, {
    gis_states %>%
      ggplot() +
      geom_sf()
  })
)
