###############################################################################
# Download and prepare data for `HI_leg_2020` analysis
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
cli_process_start("Downloading files for {.pkg HI_leg_2020}")

path_data <- download_redistricting_file("HI", "data-raw/HI", type = "block", year = 2020)

# TODO other files here (as necessary). All paths should start with `path_`
# If large, consider checking to see if these files exist before downloading

cli_process_done()

# Compile raw data into a final shapefile for analysis -----
shp_path <- "data-out/HI_2020/shp_vtd.rds"
perim_path <- "data-out/HI_2020/perim.rds"

if (!file.exists(here(shp_path))) {
    cli_process_start("Preparing {.strong HI} shapefile")
    # read in redistricting data
  hi_shp <- read_csv(here(path_data), col_types = cols(GEOID20 = "c")) |>
    left_join(y = tigris::blocks("HI", year = 2020), by  = "GEOID20") |>
    sf::st_as_sf() |>
    # TODO ASK if EPSG is used here for HI or if that is a CA thing
    st_transform(EPSG$HI) |>
    rename_with(function(x) gsub("[0-9.]", "", x), starts_with("GEOID"))

    # add municipalities

  d_muni <- PL94171::pl_get_baf("HI", "INCPLACE_CDP")[[1]] %>%
    rename(GEOID = BLOCKID, muni = PLACEFP)

  hi_shp <- left_join(hi_shp, d_muni, by = "GEOID") %>%
    mutate(county_muni = if_else(is.na(muni), county, str_c(county, muni))) %>%
    relocate(muni, county_muni, .after = county)

    # TODO any additional columns or data you want to add should go here

  # to see if n counties per tract is greater than 1
  hi_shp %>%
    mutate(tract = str_sub(GEOID, 1, 11)) %>%
    group_by(tract) %>%
    summarize(n_county_muni = n_distinct(county_muni)) %>%
    filter(n_county_muni > 1)

  #BELLA ADD --------------------------------------------------

  #hi_shp <- hi_shp |>
    #mutate(tract = str_sub(GEOID, 1, 11)) |>
    #group_by(tract) |>
    #summarize(
     # muni = Mode(muni),
      #state = unique(state),
      #county = unique(county),
    # county_muni = Mode(county_muni),
    #across(where(is.numeric), sum)
    #) |>
    #You can now refer to tract by GEOID
   # mutate(GEOID = tract)

  # -----------------------------------------------------------

 # hi_shp <- hi_shp |>
   #left_join(y = leg_from_baf(state = "HI", to = "tract"), by = "GEOID")

  sldu_for_join <- as.data.frame(baf::baf("HI", year = 2020, geographies = "SLDU")) |>
    dplyr::rename(GEOID = SLDU.BLOCKID)

  sldl_for_join <- as.data.frame(baf::baf("HI", year = 2020, geographies = "SLDL")) |>
    dplyr::rename(GEOID = SLDL.BLOCKID)

  hi_shp <- hi_shp |>
    left_join(y = sldu_for_join, by = "GEOID") |>
    rename(ssd_2020 = SLDU.DISTRICT)

  hi_shp <- hi_shp |>
    left_join(y = sldl_for_join, by = "GEOID") |>
    rename(shd_2020 = SLDL.DISTRICT)

    # Create perimeters in case shapes are simplified
    redistmetrics::prep_perims(shp = hi_shp,
                             perim_path = here(perim_path)) |>
        invisible()

    # simplifies geometry for faster processing, plotting, and smaller shapefiles
    # TODO feel free to delete if this dependency isn't available
    if (requireNamespace("rmapshaper", quietly = TRUE)) {
        hi_shp <- rmapshaper::ms_simplify(hi_shp, keep = 0.05,
                                                 keep_shapes = TRUE) |>
            suppressWarnings()
    }

    # create adjacency graph
    hi_shp$adj <- adjacency(hi_shp)

    # TODO any custom adjacency graph edits here

    # ADJACENCY RULES I FOLLOWED:
    # It is my general understanding that the only legal requirement as far as what islands are grouped together
    # is that districts do not cross the four main island units
    # Hawaii island ran fine with no custom adjacency edits
    # With Maui, however, I ran into adjacency issues
    # So, I decided to implement the least connections possible

    # First I connected Lanai with Kahoolawe because they are currently a part of the
    # same state house and senate districts according to https://experience.arcgis.com/experience/7960b9bb3b6543699d364f9001692e81/page/County-of-Maui?views=State-House-Districts-only%2CState-House-Districts-only-%2CTool-Instructions---

    # Island connection #1 Lanai and Kahoolawe
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 14718, 14730)

    # I needed to connect Molokai somehow so I unofficially looked up that the shortest
    # distance between Molokai and another Maui island was with the main Maui island
    # So I connected the precincts on the two islands that were visibly closest to each
    # other

    # Island connection #2 Molokai and Maui
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 14693, 14703)

    # BELLA Kauai island connections
    #Kaula to Niihau
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 12683, 12684)
    #Niiahu to Kauai
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 12676, 12683)
    #Niihau to Northwest island chain
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10966, 12683)
    #Below are just all of the islands in the chain connected
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10966, 10968)
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10968, 10971)
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10978, 10988)
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10978, 10979)
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10979, 10981)
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10981, 10984)
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 10963, 10984)

    # BELLA arbitrary island connections

    #Hawaii to Maui
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 4161, 14712)

    #Molokai to Oahu
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 11280, 14689)

    #Oahu to Kauai
    hi_shp$adj <- geomander::add_edge(hi_shp$adj, 11249, 12664)

    #added island connections CONG FILE...

    # Connect islands, but not for use
    #islands <- tribble(
      #~v1, ~v2,
     # 379, 413,
     # 413, 412,
      #412, 411,
      #411, 390,
      #390, 459,
      #459, 461,
      #461, 460,
      #460, 55
    #)

   # hi_shp$adj <- hi_shp$adj %>% add_edge(islands$v1, islands$v2)


    # check max number of connected components
    # 1 is one fully connected component, more is worse
    ccm(hi_shp$adj, hi_shp$ssd_2020)
    ccm(hi_shp$adj, hi_shp$shd_2020)

    hi_shp <- hi_shp |>
        fix_geo_assignment(muni)

    write_rds(hi_shp, here(shp_path), compress = "gz")
    cli_process_done()
} else {
    hi_shp <- read_rds(here(shp_path))
    cli_alert_success("Loaded {.strong HI} shapefile")
}

# TODO visualize the enacted maps using:
# redistio::draw(hi_shp, hi_shp$ssd_2020)
# redistio::draw(hi_shp, hi_shp$shd_2020)


# According to https://lrb.hawaii.gov/constitution/#articleiv, "The commission shall
# allocate the total number of members of each house of the state legislature being
# reapportioned among the four basic island units, namely:  (1) the island of Hawaii,
#(2) the islands of Maui, Lanai, Molokai and Kahoolawe, (3) the island of Oahu
# and all other islands not specifically enumerated, and (4) the islands of Kauai and Niihau"
# AND "No district shall extend beyond the boundaries of any basic island unit"

# Number of counties in Hawai'i (5)
#print(n_distinct(hi_shp$county))
# listed counties; Kalawao is a part of Molokai, according to https://www.oha.org/wp-content/uploads/RPT_Kalawao-County.pdf
#print(distinct(hi_shp, county))
