# =============================================================================
# _targets.R — analysis pipeline for
# "Subways and Local Income Growth: Evidence from Brasília"
#
#   Run everything:      targets::tar_make()
#   Read a result:       targets::tar_read(<target_name>)
#
# Functions live in R/ (numbered by pipeline stage). See README.md for the map
# between targets and the tables/figures of the paper.
# =============================================================================
library(targets)

# targets options:
tar_option_set(
  packages = c("httr", "sf", "ggplot2", "geobr", "readxl", "dplyr", "readr", "units", "tidyr", "AER", "fixest", "magrittr", "here", "terra", "spdep"), # Packages.
  format = "rds",
)

tar_source(
  files = list.files("R", pattern = "\\.R$", full.names = TRUE),
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
)

# Target list:
list(
  tar_target(stations_sf, get_stations_sf()),
  tar_target(lines_sf, get_lines_sf()),
  tar_target(planned_alignment_sf, get_planned_alignment()),
  tar_target(RAs_sf, get_RAs()),
  tar_target(highways_sf, get_highways_sf()),

  # Descriptive figure of the subway system (Section "The Brasília Subway System")
  tar_target(
    subway_system_map,
    generate_subway_system_map(stations_sf, lines_sf, RAs_sf),
    format = "file"
  ),

  tar_target(census_sf_2000_full, get_census_sf_2000()),
  tar_target(census_sf_2010, get_census_sf_2010()),
  tar_target(census_tables_2000, get_census_tables_2000()),
  tar_target(census_2000_full, join_tables_sf_2000(census_sf_2000_full, census_tables_2000)),
  tar_target(census_tables_2010, get_census_tables_2010()),
  tar_target(census_2010_full, join_tables_sf_2010(census_sf_2010, census_tables_2010, census_2000_full)),
  tar_target(census_to_harmonize, prepare_for_harmonization(census_2000_full, census_2010_full)),
  tar_target(census_harmonized, harmonize_tracts(census_to_harmonize)),
  tar_target(census_with_new_variables, new_variables(census_harmonized, lines_sf, planned_alignment_sf, stations_sf, RAs_sf, highways_sf)),
  tar_target(duplicates_dropped, drop_duplicates(census_with_new_variables)),
  tar_target(census_changes, create_changes(duplicates_dropped)),
  tar_target(census_final, add_cfa_coefficients(census_changes)),
  tar_target(descriptive_table, descriptive_statistics_table(census_final)),
  tar_target(results_covariate_importance, covariate_importance_analysis(census_final)),
  tar_target(regression_sample, prepare_regression_sample(census_final)),
  
  # Main results
  tar_target(results_main,     run_regressions(regression_sample)),
  tar_target(results_ols, run_regressions_ols(regression_sample)),
  tar_target(results_robustness_sample_radius, robustness_sample_radius(regression_sample)),
  tar_target(results_robustness_threshold, robustness_threshold(regression_sample)),
  tar_target(results_robustness_intensity, robustness_intensity(regression_sample)),
  tar_target(results_robustness_clustering,  robustness_clustering(regression_sample)),
  tar_target(results_robustness_conley,      robustness_conley(regression_sample)),

  # Heterogeneity by RA (10_heterogeneity_by_RA.R)
  tar_target(results_heterogeneity_RA,  run_heterogeneity_by_RA(regression_sample)),
  tar_target(results_iv_by_RA,    run_iv_by_RA(regression_sample)),

  # Sorting: population, households, income per household, apartment share (11_sorting.R)
  tar_target(results_population,        run_regressions_population(regression_sample)),
  tar_target(results_households,       run_regressions_households(regression_sample, census_final)),
  tar_target(results_income_per_household,  run_regressions_income_per_household(regression_sample, census_final)),
  tar_target(results_apartment_share,         run_regressions_apartment_share(regression_sample, census_final)),

  # MapBiomas urbanization (4b + 12)
  tar_target(census_urbanization,              add_urban_share(census_final)),
  tar_target(urbanization_sample,            prepare_urbanization_sample(census_urbanization)),
  tar_target(results_urbanization, run_regressions_urbanization(urbanization_sample)),

  # Hypothesis 2 — Composition / gentrification effect (13)
  tar_target(composition_sample,             prepare_composition_sample(census_final)),
  tar_target(results_composition, run_regressions_composition(composition_sample)),

  # Labor-market mechanism — extensive and intensive margins (17)
  tar_target(labor_market_sample,          prepare_labor_market_sample(census_final)),
  tar_target(results_labor_market,       run_regressions_labor_market(labor_market_sample)),

  # Heterogeneity by land-use permissiveness — RA's maximum CfAM (18)
  tar_target(results_heterogeneity_cfam,           run_heterogeneity_cfam(regression_sample))
  )
