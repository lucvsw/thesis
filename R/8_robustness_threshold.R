# =============================================================================
# 8_robustness_threshold.R — Robustness: different exposure dummies
# Specification: eq_iv_4 (all controls + RA FE, 10 km sample)
# Each column uses a different exposure threshold and its instrument
# =============================================================================

robustness_threshold <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  thresholds <- c(500, 1000, 1500, 2000, 2500)

  controls <- "log(income_per_capita_2000) + log(dist_cbd_2000) + log(dist_highway_2000) + share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 + log(pop_2000)"

  models <- lapply(thresholds, function(t) {
    feols(
      as.formula(paste0(
        "dlog_income_per_capita ~ ", controls,
        " | ra_id",
        " | dummy_", t, "m ~ dummyp_", t, "m"
      )),
      data = dat,
      se = "cluster", cluster = ~ra_id
    )
  })

  names(models) <- paste0("t", thresholds, "m")
  models
}

# tar_load(results_robustness_threshold)
# 
# # See a specific threshold
# summary(results_robustness_threshold$t2500m)
