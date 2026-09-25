# =============================================================================
# 11_sorting.R — Sorting evidence: population, households,
#                income per household and apartment share
# =============================================================================
#
# Goal: check whether the subway caused relocation of people and/or changes
# in the composition of households in the areas near stations. If population,
# households and the apartment share do not rise in exposed tracts, the effect
# on income (6_main_regressions.R) is not driven by densification or by sorting
# of wealthier residents.
#
# Outcomes:
#   (A) dlog_pop        = log(pop_2010) - log(pop_2000)
#   (B) dlog_households = log(households_2010) - log(households_2000)
#   (C) dlog_income_per_household  = log(income_hh_2010) - log(income_hh_2000)
#          income_per_household: tests whether the subway attracted higher-income
#          households (sorting by type of residence, not by persons)
#   (D) d_share_apt = share_apt_2010 - share_apt_2000  [level, not log]
#          apartment share: tests gentrification/land-use change;
#          includes tracts that started from zero apartments
#
# Structure: 4 specifications with progressively added controls, mirroring
# 6_main_regressions.R. Baseline control = initial level of the outcome itself.
# log(income_per_capita_2000) enters only in specifications (3) and (4).
# Threshold and instrument: 1000 m. Sample: dummy_subway_10km == 1.
# =============================================================================

# -----------------------------------------------------------------------------
# (A) Population growth
# -----------------------------------------------------------------------------
run_regressions_population <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  # (1) Baseline population + RA FE
  pop_iv_1 <- feols(
    dlog_pop ~ log(pop_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) + Geographic controls
  pop_iv_2 <- feols(
    dlog_pop ~
      log(pop_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) + Socioeconomic controls (includes income_2000)
  pop_iv_3 <- feols(
    dlog_pop ~
      log(pop_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls [full specification]
  pop_iv_4 <- feols(
    dlog_pop ~
      log(pop_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(pop_iv_1 = pop_iv_1, pop_iv_2 = pop_iv_2,
       pop_iv_3 = pop_iv_3, pop_iv_4 = pop_iv_4)
}

# -----------------------------------------------------------------------------
# (B) Household growth
# -----------------------------------------------------------------------------
run_regressions_households <- function(smpl, census_final) {

  # Build dlog_households from the full panel (census_final),
  # since this change is not precomputed in create_changes()
  hh_vars <- census_final %>%
    st_drop_geometry() %>%
    filter(dummy_subway_10km == 1) %>%
    arrange(code_tract, year) %>%
    group_by(code_tract) %>%
    summarise(
      hh_2000 = first(households[year == 2000]),
      hh_2010 = first(households[year == 2010]),
      .groups  = "drop"
    ) %>%
    mutate(
      dlog_households = ifelse(
        hh_2000 > 0 & hh_2010 > 0,
        log(hh_2010) - log(hh_2000),
        NA_real_
      )
    )

  dat <- smpl %>%
    filter(dummy_subway_10km == 1) %>%
    left_join(
      hh_vars %>% select(code_tract, hh_2000, dlog_households),
      by = "code_tract"
    )

  # (1) Baseline households + RA FE
  hh_iv_1 <- feols(
    dlog_households ~ log(hh_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) + Geographic controls
  hh_iv_2 <- feols(
    dlog_households ~
      log(hh_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) + Socioeconomic controls (includes income_2000)
  hh_iv_3 <- feols(
    dlog_households ~
      log(hh_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls [full specification]
  hh_iv_4 <- feols(
    dlog_households ~
      log(hh_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(hh_iv_1 = hh_iv_1, hh_iv_2 = hh_iv_2,
       hh_iv_3 = hh_iv_3, hh_iv_4 = hh_iv_4)
}

# -----------------------------------------------------------------------------
# (C) Income per household growth
# -----------------------------------------------------------------------------
run_regressions_income_per_household <- function(smpl, census_final) {

  # Build the change and the baseline from the full panel
  income_hh_vars <- census_final %>%
    st_drop_geometry() %>%
    filter(dummy_subway_10km == 1) %>%
    group_by(code_tract) %>%
    summarise(
      income_hh_2000      = first(income_per_household[year == 2000]),
      income_hh_2010      = first(income_per_household[year == 2010]),
      .groups = "drop"
    ) %>%
    mutate(
      dlog_income_per_household = ifelse(
        income_hh_2000 > 0 & income_hh_2010 > 0,
        log(income_hh_2010) - log(income_hh_2000),
        NA_real_
      )
    )

  dat <- smpl %>%
    filter(dummy_subway_10km == 1) %>%
    left_join(
      income_hh_vars %>% select(code_tract, income_hh_2000, dlog_income_per_household),
      by = "code_tract"
    )

  # (1) Baseline income per household + RA FE
  iph_iv_1 <- feols(
    dlog_income_per_household ~ log(income_hh_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) + Geographic controls
  iph_iv_2 <- feols(
    dlog_income_per_household ~
      log(income_hh_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) + Socioeconomic controls (includes per-capita income 2000)
  iph_iv_3 <- feols(
    dlog_income_per_household ~
      log(income_hh_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls [full specification]
  iph_iv_4 <- feols(
    dlog_income_per_household ~
      log(income_hh_2000) +
      log(dist_cbd_2000) + log(dist_highway_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(iph_iv_1 = iph_iv_1, iph_iv_2 = iph_iv_2,
       iph_iv_3 = iph_iv_3, iph_iv_4 = iph_iv_4)
}

# -----------------------------------------------------------------------------
# (D) Change in the apartment share
# -----------------------------------------------------------------------------
run_regressions_apartment_share <- function(smpl, census_final) {

  # Get share_apt_2000 from the panel (it is not in prepare_regression_sample)
  share_apt_vars <- census_final %>%
    st_drop_geometry() %>%
    filter(dummy_subway_10km == 1, year == 2000) %>%
    group_by(code_tract) %>%
    summarise(share_apt_2000 = first(share_apartments), .groups = "drop")

  dat <- smpl %>%
    filter(dummy_subway_10km == 1) %>%
    left_join(share_apt_vars, by = "code_tract")

  # (1) Baseline apartment share + RA FE
  # Note: baseline in levels (not logs) because share_apt can be zero
  apt_iv_1 <- feols(
    d_share_apt ~ share_apt_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (2) + Geographic controls
  apt_iv_2 <- feols(
    d_share_apt ~
      share_apt_2000 +
      log(dist_cbd_2000) + log(dist_highway_2000)
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (3) + Socioeconomic controls (includes income 2000)
  apt_iv_3 <- feols(
    d_share_apt ~
      share_apt_2000 +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  # (4) All controls [full specification]
  apt_iv_4 <- feols(
    d_share_apt ~
      share_apt_2000 +
      log(dist_cbd_2000) + log(dist_highway_2000) +
      log(income_per_capita_2000) + share_college_complete_2000 +
      share_illiterate_2000 + share_over_65_2000
    | ra_id
    | dummy_1000m ~ dummyp_1000m,
    data = dat, se = "cluster", cluster = ~ra_id
  )

  list(apt_iv_1 = apt_iv_1, apt_iv_2 = apt_iv_2,
       apt_iv_3 = apt_iv_3, apt_iv_4 = apt_iv_4)
}

# # Read each result
# res_pop <- tar_read(results_population)
# res_hh <- tar_read(results_households)
# res_iph <- tar_read(results_income_per_household)
# res_apt <- tar_read(results_apartment_share)
# 
# # Full specification of each outcome
# # Population ---
# summary(res_pop$pop_iv_1)
# summary(res_pop$pop_iv_2)
# summary(res_pop$pop_iv_3)
# summary(res_pop$pop_iv_4)
# # Households --
# summary(res_hh$hh_iv_1)
# summary(res_hh$hh_iv_2)
# summary(res_hh$hh_iv_3)
# summary(res_hh$hh_iv_4)
# # Income per household 
# summary(res_iph$iph_iv_1)
# summary(res_iph$iph_iv_2)
# summary(res_iph$iph_iv_3)
# summary(res_iph$iph_iv_4)
# # Number of apartments
# summary(res_apt$apt_iv_1)
# summary(res_apt$apt_iv_2)
# summary(res_apt$apt_iv_3)
# summary(res_apt$apt_iv_4)
