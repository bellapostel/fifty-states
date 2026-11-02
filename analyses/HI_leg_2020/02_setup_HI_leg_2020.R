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

# Manually set population target for each map based on
# https://elections.hawaii.gov/wp-content/uploads/2021-Reapportionment-Commission-Final-Report-952-pages-February-25-2022.pdf
# The deviations I just manually calculated to be +-.05

#HAWAII BIG ISLAND
hawaii_shd <- map_shd |>
  filter(county == "Hawaii County")
# Manually set pop target
attr(hawaii_shd, 'pop_bounds') <- c(23749, 24999, 26249)

# For HAWAII ssd, I changed pop_tol to 0.10 due to stalling out.
# https://elections.hawaii.gov/wp-content/uploads/Addendum-01-Template-Plans.pdf?utm_source=chatgpt.com
# See below page 7 for the pop deviations within each basic island unit
# https://elections.hawaii.gov/wp-content/uploads/2021-Reapportionment-Commission-Final-Report-952-pages-February-25-2022.pdf
# Meeting minutes: "Courts have upheld total deviation under 10 percent" Page 100
# https://elections.hawaii.gov/wp-content/uploads/Meeting-Materials-20210826-v1.pdf

hawaii_ssd <- map_ssd |>
  filter(county == "Hawaii County")
# Manually set pop target
attr(hawaii_ssd, 'pop_bounds') <- c(47498, 49998, 52498)

#OAHU
oahu_shd <- map_shd |>
  filter(county == "Honolulu County") |>
  set_pop_tol(0.05)
# Manually set pop target
attr(oahu_shd, 'pop_bounds') <- c(25441, 26780, 28119)

oahu_ssd <- map_ssd |>
  filter(county == "Honolulu County")
# Manually set pop target
attr(oahu_ssd, 'pop_bounds') <- c(50882, 53560, 56238)

#MAUI
maui_shd <- map_shd |>
  filter(county == "Maui County" | county == "Kalawao County")
# Manually set pop target
attr(maui_shd, 'pop_bounds') <- c(26052, 27423, 28794)

maui_ssd <- map_ssd |>
  filter(county == "Maui County" | county == "Kalawao County")
# Manually set pop target
attr(maui_ssd, 'pop_bounds') <- c(52104, 54846, 57588)

#KAUAI
kauai_shd <- map_shd |>
  filter(county == "Kauai County")
# Manually set pop target
attr(kauai_shd, 'pop_bounds') <- c(23118, 24335, 25552)

kauai_ssd <- map_ssd |>
  filter(county == "Kauai County")
# Manually set pop target
attr(kauai_ssd, 'pop_bounds') <- c(69354, 73004, 76654)

# Added the following Polsby-Popper constraint
constr <- redist_constr(hawaii_shd)
constr <- add_constr_polsby(constr, strength = 10, perim_df = NULL)

# Added the following Polsby-Popper constraint
#constr <- redist_constr(oahu_shd)
#constr <- add_constr_polsby(constr, strength = 1, perim_df = NULL)

# TODO remove if not necessary. Adjust pop_muni as needed to balance county/muni splits
# make pseudo counties with default settings
# BELLA EDITED TO ACCOUNT FOR SUB MAPS; replaced "map" with name of island unit
# CHANGE for each island unit
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
# Bella added SHDS
attr(hawaii_shd, "analysis_name") <- "HI_HAWAII_SHD_2020"
attr(maui_shd, "analysis_name") <- "HI_MAUI_SHD_2020"
attr(oahu_shd, "analysis_name") <- "HI_OAHU_SHD_2020"
attr(kauai_shd, "analysis_name") <- "HI_KAUAI_SHD_2020"
#SSDS
attr(hawaii_ssd, "analysis_name") <- "HI_HAWAII_SSD_2020"
attr(maui_ssd, "analysis_name") <- "HI_MAUI_SSD_2020"
attr(kauai_ssd, "analysis_name") <- "HI_KAUAI_SSD_2020"

# BELLA edited the below output names to account for each island unit; replaced "map" with island unit
# Output the redist_map object. Do not edit this path.
# TODO edited the below path to save separate maps for each of the basic island units
write_rds(hawaii_ssd, "data-out/HI_2020/HI_leg_2020_map_ssd_HAWAII.rds", compress = "xz")
write_rds(hawaii_shd, "data-out/HI_2020/HI_leg_2020_map_shd_HAWAII.rds", compress = "xz")
cli_process_done()
