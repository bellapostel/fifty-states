###############################################################################
# Set up redistricting simulation for `IL_leg_2020`
# © ALARM Project, July 2026
###############################################################################
cli_process_start("Creating {.cls redist_map} object for {.pkg IL_leg_2020}")

# TODO any pre-computation (usually not necessary)

# Removing water precincts
# Public waters: https://dnr.illinois.gov/waterresources/publicwaters.html
# Plot to show where the removed precincts are
# library(ggplot2)
# ggplot() +
# geom_sf(data = il_shp, fill = "grey90", color = "white", linewidth = 0.05) +
# geom_sf(data = filter(il_shp, is.na(ssd_2020)), fill = "red",color = "black",
# linewidth = 0.3)

il_shp <- il_shp |>
  filter(!is.na(muni))

il_shp$ssd_2020 <- as.numeric(il_shp$ssd_2020 )

map_ssd <- redist_map(il_shp, pop_tol = 0.05,
    existing_plan = ssd_2020, adj = il_shp$adj)

map_shd <- redist_map(il_shp, pop_tol = 0.05,
    existing_plan = shd_2020, adj = il_shp$adj)

# TODO any filtering, cores, merging, etc.

# TODO remove if not necessary. Adjust pop_muni as needed to balance county/muni splits
# make pseudo counties with default settings
map_ssd <- map_ssd |>
    mutate(pseudo_county = pick_county_muni(map_ssd, counties = county, munis = muni,
                                            pop_muni = get_target(map_ssd)))
map_shd <- map_shd |>
    mutate(pseudo_county = pick_county_muni(map_shd, counties = county, munis = muni,
                                            pop_muni = get_target(map_shd)))
# IF MERGING CORES OR OTHER UNITS:
# make a new `map_cores` object that is merged & used for simulating. You can set `drop_geom=TRUE` for this.

# Add an analysis name attribute
attr(map_ssd, "analysis_name") <- "IL_SSD_2020"
attr(map_shd, "analysis_name") <- "IL_SHD_2020"

# Output the redist_map object. Do not edit this path.
write_rds(map_ssd, "data-out/IL_2020/IL_leg_2020_map_ssd.rds", compress = "xz")
write_rds(map_shd, "data-out/IL_2020/IL_leg_2020_map_shd.rds", compress = "xz")
cli_process_done()
