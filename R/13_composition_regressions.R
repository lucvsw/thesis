# =============================================================================
# 13_composition_regressions.R — Hypothesis 2: Composition Effect (Gentrification)
#
# Question: did subway access change the demographic composition of tracts,
#           suggesting displacement of low-income population (gentrification)?
# DVs: Δ share of women | Δ share of large families | Δ illiteracy rate of household heads
# Strategy: 2SLS identical to 6_main_regressions.R
# =============================================================================

prepare_composition_sample <- function(census_final) {

  # 2000 controls
  controls_2000 <- census_final %>%
    filter(year == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           share_college_complete,
           share_illiterate,
           share_over_65,
           dist_cbd,
           income_per_capita,
           pop,
           dist_highway,
           share_women,
           share_large_family,
           share_illiterate_heads,
           share_high_income,
           share_working_age) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  # Base: year 2010 with demographic changes
  census_final %>%
    filter(year == 2010) %>%
    left_join(controls_2000, by = "code_tract") %>%
    mutate(
      d_share_women    = share_women    - share_women_2000,
      d_share_large_family     = share_large_family     - share_large_family_2000,
      d_share_illiterate_heads  = share_illiterate_heads  - share_illiterate_heads_2000,
      d_share_high_income = share_high_income - share_high_income_2000,
      d_share_over_65     = share_over_65     - share_over_65_2000,
      d_share_working_age   = share_working_age   - share_working_age_2000
    )
}

run_regressions_composition <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  # Helper: runs 4 IV specifications for a DV and its baseline
  run_iv4 <- function(dv, baseline) {
    geo   <- "log(dist_cbd_2000) + log(dist_highway_2000)"
    socio <- "share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 + log(pop_2000)"
    income <- "log(income_per_capita_2000)"
    iv    <- "dummy_1000m ~ dummyp_1000m"

    mk <- function(controls) {
      as.formula(paste0(dv, " ~ ", controls, " | ra_id | ", iv))
    }

    list(
      m1 = feols(mk(baseline),                                           se = "cluster", cluster = ~ra_id, data = dat),
      m2 = feols(mk(paste(baseline, geo,           sep = " + ")),        se = "cluster", cluster = ~ra_id, data = dat),
      m3 = feols(mk(paste(baseline, socio, income,  sep = " + ")),        se = "cluster", cluster = ~ra_id, data = dat),
      m4 = feols(mk(paste(baseline, geo, socio, income, sep = " + ")),    se = "cluster", cluster = ~ra_id, data = dat)
    )
  }

  list(
    women    = run_iv4("d_share_women",    "share_women_2000"),
    large_family     = run_iv4("d_share_large_family",     "share_large_family_2000"),
    illiterate_heads  = run_iv4("d_share_illiterate_heads",  "share_illiterate_heads_2000"),
    high_income = run_iv4("d_share_high_income", "share_high_income_2000"),
    over_65     = run_iv4("d_share_over_65",     "share_over_65_2000"),
    pop_working_age   = run_iv4("d_share_working_age",   "share_working_age_2000")
  )
}

# # -----------------------------------------------------------------------------
# Reading the results
# # -----------------------------------------------------------------------------
# res <- tar_read(results_composition)
# 
# # Δ Share of women
# summary(res$women$m1)
# summary(res$women$m2)
# summary(res$women$m3)
# summary(res$women$m4)
# # Δ Share of large families
# summary(res$large_family$m1)
# summary(res$large_family$m2)
# summary(res$large_family$m3)
# summary(res$large_family$m4)
# # Δ Illiteracy rate of household heads
# summary(res$illiterate_heads$m1)
# summary(res$illiterate_heads$m2)
# summary(res$illiterate_heads$m3)
# summary(res$illiterate_heads$m4)
