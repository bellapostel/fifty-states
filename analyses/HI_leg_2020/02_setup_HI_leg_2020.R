###############################################################################
# Set up redistricting simulation for `HI_leg_2020`
# © ALARM Project, July 2026
###############################################################################
cli_process_start("Creating {.cls redist_map} object for {.pkg HI_leg_2020}")

# TODO any pre-computation (usually not necessary)

# TODO change the below for each of the four basic island units

map_ssd <- redist_map(hi_shp, pop_tol = 0.05,
    existing_plan = ssd_2020, adj = hi_shp$adj)

map_shd <- redist_map(hi_shp, pop_tol = 0.05,
    existing_plan = shd_2020, adj = hi_shp$adj)

# TODO any filtering, cores, merging, etc.

#HAWAII BIG ISLAND
hawaii_shd <- map_shd |>
  filter(county == "Hawaii County")

hawaii_ssd <- map_ssd |>
  filter(county == "Hawaii County")

#OAHU
oahu_shd <- map_shd |>
  filter(county == "Honolulu County")

oahu_ssd <- map_ssd |>
  filter(county == "Honolulu County")

#MAUI
maui_shd <- map_shd |>
  filter(county == "Maui County" | county == "Kalawao County")

maui_ssd <- map_ssd |>
  filter(county == "Maui County" | county == "Kalawao County")

#KAUAI
kauai_shd <- map_shd |>
  filter(county == "Kauai County")

kauai_ssd <- map_ssd |>
  filter(county == "Kauai County")

# TODO remove if not necessary. Adjust pop_muni as needed to balance county/muni splits
# make pseudo counties with default settings
# BELLA EDITED TO ACCOUNT FOR SUB MAPS; replaced "map" with name of island unit
hawaii_ssd <- hawaii_ssd |>
    mutate(pseudo_county = pick_county_muni(hawaii_ssd, counties = county, munis = muni,
                                            pop_muni = get_target(hawaii_ssd)))
hawaii_shd <- hawaii_shd |>
    mutate(pseudo_county = pick_county_muni(hawaii_shd, counties = county, munis = muni,
                                            pop_muni = get_target(hawaii_shd)))
# IF MERGING CORES OR OTHER UNITS:
# make a new `map_cores` object that is merged & used for simulating. You can set `drop_geom=TRUE` for this.

# Add an analysis name attribute
attr(map_ssd, "analysis_name") <- "HI_SSD_2020"
attr(map_shd, "analysis_name") <- "HI_SHD_2020"

# BELLA edited the below output names to account for each island unit; replaced "map" with island unit
# Output the redist_map object. Do not edit this path.
# TODO edited the below path to save separate maps for each of the basic island units
write_rds(hawaii_ssd, "data-out/HI_2020/HI_leg_2020_map_ssd_HAWAII.rds", compress = "xz")
write_rds(hawaii_shd, "data-out/HI_2020/HI_leg_2020_map_shd_HAWAII.rds", compress = "xz")
cli_process_done()
