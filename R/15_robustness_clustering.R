# =============================================================================
# 15_robustness_clustering.R — Robustness: clustering by census tract
# Replicates the four main specifications (eq_iv_1 to eq_iv_4) with SEs
# clustered by census tract (code_tract) instead of by RA.
# Since the sample has one observation per tract, clustering by code_tract
# is equivalent to heteroskedasticity-robust standard errors (HC1).
# =============================================================================

robustness_clustering <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  specs <- list(
    eq1 = "log(income_per_capita_2000)",
    eq2 = paste("log(income_per_capita_2000) +",
                "log(dist_cbd_2000) + log(dist_highway_2000)"),
    eq3 = paste("log(income_per_capita_2000) +",
                "share_college_complete_2000 + share_illiterate_2000 +",
                "share_over_65_2000 + log(pop_2000)"),
    eq4 = paste("log(income_per_capita_2000) +",
                "log(dist_cbd_2000) + log(dist_highway_2000) +",
                "share_college_complete_2000 + share_illiterate_2000 +",
                "share_over_65_2000 + log(pop_2000)")
  )

  lapply(specs, function(ctrl) {
    f <- as.formula(paste0(
      "dlog_income_per_capita ~ ", ctrl,
      " | ra_id | dummy_1000m ~ dummyp_1000m"
    ))
    list(
      cluster_ra    = feols(f, data = dat, se = "cluster", cluster = ~ra_id),
      cluster_tract = feols(f, data = dat, se = "cluster", cluster = ~code_tract)
    )
  })
}
