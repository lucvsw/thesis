# =============================================================================
# 6_main_regressions.R — Main regressions of the paper
# =============================================================================

# (A–C) Prepare the regression sample
prepare_regression_sample <- function(census_final) {

  # (A) Keep only year 2010 and build census_var_2010
  census_var_2010 <- census_final %>%
    filter(year == 2010)

  # (B) Extract the 2000 controls
  controls_2000 <- census_final %>%
    filter(year == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           share_higher_ed,
           share_illiterate,
           share_over_65,
           dist_cbd,
           income_per_capita,
           share_college_complete,
           pop,
           dist_highway) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  # (C) Join the controls to the 2010 census
  census_var_2010 %>%
    left_join(controls_2000, by = "code_tract")
}

# (D) Run the regressions
run_regressions <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  # (1) Baseline income only + RA FE
  eq_iv_1 <- feols(
    dlog_income_per_capita ~ log(income_per_capita_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) Geographic controls + RA FE
  eq_iv_2 <- feols(
    dlog_income_per_capita ~ log(income_per_capita_2000) + log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) Socioeconomic controls + RA FE
  eq_iv_3 <- feols(
    dlog_income_per_capita ~ log(income_per_capita_2000) + share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 + log(pop_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls + RA FE
  eq_iv_4 <- feols(
    dlog_income_per_capita ~ log(income_per_capita_2000) + log(dist_cbd_2000) + log(dist_highway_2000) + share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 + log(pop_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(eq_iv_1 = eq_iv_1, eq_iv_2 = eq_iv_2, eq_iv_3 = eq_iv_3, eq_iv_4 = eq_iv_4)
}
