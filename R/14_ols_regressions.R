# =============================================================================
# 14_ols_regressions.R — OLS regressions analogous to the main table
# Specifications identical to those in 6_main_regressions.R, but without instrumentation.
# Used in the appendix to compare OLS vs. IV.
# =============================================================================

run_regressions_ols <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  # (1) Baseline income only + RA FE
  eq_ols_1 <- feols(
    dlog_income_per_capita ~ dummy_1000m + log(income_per_capita_2000)
    | ra_id,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) Geographic controls + RA FE
  eq_ols_2 <- feols(
    dlog_income_per_capita ~ dummy_1000m + log(income_per_capita_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) Socioeconomic controls + RA FE
  eq_ols_3 <- feols(
    dlog_income_per_capita ~ dummy_1000m + log(income_per_capita_2000) +
      share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 +
      log(pop_2000)
    | ra_id,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls + RA FE
  eq_ols_4 <- feols(
    dlog_income_per_capita ~ dummy_1000m + log(income_per_capita_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000) +
      share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 +
      log(pop_2000)
    | ra_id,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(eq_ols_1 = eq_ols_1, eq_ols_2 = eq_ols_2,
       eq_ols_3 = eq_ols_3, eq_ols_4 = eq_ols_4)
}
