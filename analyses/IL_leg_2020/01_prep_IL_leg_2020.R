###############################################################################
# Download and prepare data for `IL_leg_2020` analysis
# © ALARM Project, July 2026
###############################################################################

suppressMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(redist)
  library(geomander)
  library(cli)
  library(here)
  library(tinytiger)
  devtools::load_all() # load utilities
})

stopifnot(utils::packageVersion("redist") >= "5.0.0.1")

# Download necessary files for analysis -----
cli_process_start("Downloading files for {.pkg IL_leg_2020}")

path_data <- download_redistricting_file("IL", "data-raw/IL", year = 2020)

# TODO other files here (as necessary). All paths should start with `path_`
# If large, consider checking to see if these files exist before downloading

cli_process_done()

# Compile raw data into a final shapefile for analysis -----
shp_path <- "data-out/IL_2020/shp_vtd.rds"
perim_path <- "data-out/IL_2020/perim.rds"

if (!file.exists(here(shp_path))) {
  cli_process_start("Preparing {.strong IL} shapefile")
  # read in redistricting data
  il_shp <- read_csv(here(path_data), col_types = cols(GEOID20 = "c")) |>
    join_vtd_shapefile(year = 2020) |>
    st_transform(EPSG$IL)  |>
    rename_with(function(x) gsub("[0-9.]", "", x), starts_with("GEOID"))

  # add municipalities
  d_muni <- make_from_baf("IL", "INCPLACE_CDP", "VTD", year = 2020)  |>
    mutate(GEOID = paste0(censable::match_fips("IL"), vtd)) |>
    select(-vtd)
  d_ssd <- make_from_baf("IL", "SLDU", "VTD", year = 2020)  |>
    transmute(GEOID = paste0(censable::match_fips("IL"), vtd),
              ssd_2010 = as.integer(sldu))
  d_shd <- make_from_baf("IL", "SLDL", "VTD", year = 2020)  |>
    transmute(GEOID = paste0(censable::match_fips("IL"), vtd),
              shd_2010 = as.integer(sldl))

  il_shp <- il_shp |>
    left_join(d_muni, by = "GEOID") |>
    left_join(d_ssd, by = "GEOID") |>
    left_join(d_shd, by = "GEOID") |>
    mutate(county_muni = if_else(is.na(muni), county, str_c(county, muni))) |>
    relocate(muni, county_muni, ssd_2010, .after = county) |>
    relocate(muni, county_muni, shd_2010, .after = county)

  # add the enacted plan
  il_shp <- il_shp |>
    left_join(y = leg_from_baf(state = "IL"), by = "GEOID")


  # TODO any additional columns or data you want to add should go here

  # Create perimeters in case shapes are simplified
  redistmetrics::prep_perims(shp = il_shp,
                             perim_path = here(perim_path)) |>
    invisible()

  # simplifies geometry for faster processing, plotting, and smaller shapefiles
  # TODO feel free to delete if this dependency isn't available
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    il_shp <- rmapshaper::ms_simplify(il_shp, keep = 0.05,
                                      keep_shapes = TRUE) |>
      suppressWarnings()
  }

  # create adjacency graph
  il_shp$adj <- adjacency(il_shp)

  # TODO any custom adjacency graph edits here

  # Fixing state senate adjacency
  # District 005
  il_shp$adj <- geomander::add_edge(il_shp$adj, 1853, 1857)
  # District 001
  il_shp$adj <- geomander::add_edge(il_shp$adj, 1006, 1027)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 1006, 1136)
  #District 008
  il_shp$adj <- geomander::add_edge(il_shp$adj, 2142, 2169)
# District 009
  il_shp$adj <- geomander::add_edge(il_shp$adj, 2572, 2578)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 2572, 2863)
  # District 011
  il_shp$adj <- geomander::add_edge(il_shp$adj, 3614, 3724)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 3605, 3626)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 2987, 3045)
  #District 012
  il_shp$adj <- geomander::add_edge(il_shp$adj, 1428, 4142)
  #District 013
  il_shp$adj <- geomander::add_edge(il_shp$adj, 690, 2261)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 946, 967)
  #District 015
  il_shp$adj <- geomander::add_edge(il_shp$adj, 2685, 2699)
  #District 016
  il_shp$adj <- geomander::add_edge(il_shp$adj, 3006, 3035)
  #District 017
  il_shp$adj <- geomander::add_edge(il_shp$adj, 6255, 9498)
  #District 021
  il_shp$adj <- geomander::add_edge(il_shp$adj, 4963, 5128)
  #District 030
  il_shp$adj <- geomander::add_edge(il_shp$adj, 6690, 6691)
  #District 031
  il_shp$adj <- geomander::add_edge(il_shp$adj, 6564, 6720)
  #District 037
  il_shp$adj <- geomander::add_edge(il_shp$adj, 6812, 6837)
  #District 038
  il_shp$adj <- geomander::add_edge(il_shp$adj, 4241, 4267)
  #District 039
  il_shp$adj <- geomander::add_edge(il_shp$adj, 3121, 4354)
  #District 043
  il_shp$adj <- geomander::add_edge(il_shp$adj, 9484, 9782)
  # District 046
  il_shp$adj <- geomander::add_edge(il_shp$adj, 8248, 8268)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 9154, 9173)
  il_shp$adj <- geomander::add_edge(il_shp$adj, 7358, 7382)

  # Fixing state house adjacency
  # District 003
  # District 004
  # District 005
  # District 006
  # District 015
  # District 026
  # District 032
  # District 034
  # District 050
  # District 051
  # District 052
  # District 059
  # District 060
  # District 062
  # District 067
  # District 079
  # District 080
  # District 091
  # District 095
  # District 096
  # District 098
  # District 104
  # District 112
  # District 114

  # check max number of connected components
  # 1 is one fully connected component, more is worse
  ccm(il_shp$adj, il_shp$ssd_2020)
  ccm(il_shp$adj, il_shp$shd_2020)

  il_shp <- il_shp |>
    fix_geo_assignment(muni)

  write_rds(il_shp, here(shp_path), compress = "gz")
  cli_process_done()
} else {
  il_shp <- read_rds(here(shp_path))
  cli_alert_success("Loaded {.strong IL} shapefile")
}

# TODO visualize the enacted maps using:
# redistio::draw(il_shp, il_shp$ssd_2020)
# redistio::draw(il_shp, il_shp$shd_2020)
