# =============================================================================
# 7_robustness_sample_radius.R — Robustness: different sample radii
# Specification: eq_iv_4 (all controls + RA FE, 1000 m threshold)
# Each column uses a different radius to define the sample of tracts
# =============================================================================

robustness_sample_radius <- function(smpl) {

  # Radii tested and corresponding dummies
  radii <- list(
    km_5    = "dummy_subway_5km",
    km_7_5  = "dummy_subway_7_5km",
    km_10   = "dummy_subway_10km",
    km_12_5 = "dummy_subway_12_5km",
    km_15   = "dummy_subway_15km",
    km_20   = "dummy_subway_20km"
  )

  lapply(radii, function(dummy) {
    feols(
      dlog_income_per_capita ~
        log(income_per_capita_2000) +
        log(dist_cbd_2000) +
        log(dist_highway_2000) +
        share_college_complete_2000 +
        share_illiterate_2000 +
        share_over_65_2000 +
        log(pop_2000)
      | ra_id
      | dummy_1000m ~ dummyp_1000m,
      data    = smpl %>% filter(.data[[dummy]] == 1),
      se      = "cluster", cluster = ~ra_id
    )
  })
}

# tar_load(results_robustness_sample_radius)                                                  
# 
# # See a specific radius
# summary(results_robustness_sample_radius$km_10)  
