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
