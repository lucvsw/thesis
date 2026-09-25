# =============================================================================
# 17_labor_market_mechanism.R — Mechanism: labor market
# Decomposes the income effect into an extensive margin (share of heads
# with positive income) and an intensive margin (mean income of those with income).
# =============================================================================

prepare_labor_market_sample <- function(census_final) {

  d <- census_final %>%
    sf::st_drop_geometry() %>%
    mutate(
      income_total      = suppressWarnings(as.numeric(income_total)),
      heads_positive_income   = suppressWarnings(as.numeric(heads_positive_income)),
      heads_total = suppressWarnings(as.numeric(heads_total))
    ) %>%
    mutate(
      share_heads_positive_income = ifelse(heads_total > 0,
                                   heads_positive_income / heads_total, NA_real_),
      mean_income_pos     = ifelse(heads_positive_income > 0,
                                   income_total / heads_positive_income, NA_real_),
      mean_income_head    = ifelse(heads_total > 0,
                                   income_total / heads_total, NA_real_)
    )

  controls_2000 <- d %>%
    filter(year == 2000) %>%
    select(code_tract,
           share_college_complete, share_illiterate, share_over_65,
           dist_cbd, income_per_capita, pop, dist_highway,
           share_heads_positive_income, mean_income_pos, mean_income_head) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  d2010 <- d %>%
    filter(year == 2010) %>%
    select(code_tract,
           share_heads_positive_income_2010 = share_heads_positive_income,
           mean_income_pos_2010     = mean_income_pos,
           mean_income_head_2010    = mean_income_head) %>%
    distinct(code_tract, .keep_all = TRUE)

  infra <- census_final %>%
    filter(year == 2010) %>%
    sf::st_drop_geometry() %>%
    select(code_tract, ra_id,
           dummy_subway_10km, dummy_1000m, dummyp_1000m) %>%
    distinct(code_tract, .keep_all = TRUE)

  infra %>%
    left_join(controls_2000, by = "code_tract") %>%
    left_join(d2010,          by = "code_tract") %>%
    mutate(
      d_share_heads_positive_income  = share_heads_positive_income_2010 - share_heads_positive_income_2000,
      dlog_mean_income_pos  = ifelse(
        mean_income_pos_2000 > 0 & mean_income_pos_2010 > 0,
        log(mean_income_pos_2010) - log(mean_income_pos_2000), NA_real_),
      dlog_mean_income_head = ifelse(
        mean_income_head_2000 > 0 & mean_income_head_2010 > 0,
        log(mean_income_head_2010) - log(mean_income_head_2000), NA_real_)
    )
}

run_regressions_labor_market <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  geo   <- "log(dist_cbd_2000) + log(dist_highway_2000)"
  socio <- paste("share_college_complete_2000 + share_illiterate_2000 +",
                 "share_over_65_2000 + log(pop_2000)")
  income <- "log(income_per_capita_2000)"
  iv    <- "dummy_1000m ~ dummyp_1000m"

  run_iv4 <- function(dv, ctrl_base) {
    mk <- function(ctrl) as.formula(paste0(dv, " ~ ", ctrl, " | ra_id | ", iv))
    list(
      m1 = feols(mk(ctrl_base),                                          se="cluster", cluster=~ra_id, data=dat),
      m2 = feols(mk(paste(ctrl_base, geo,            sep=" + ")),        se="cluster", cluster=~ra_id, data=dat),
      m3 = feols(mk(paste(ctrl_base, socio, income,   sep=" + ")),        se="cluster", cluster=~ra_id, data=dat),
      m4 = feols(mk(paste(ctrl_base, geo, socio, income, sep=" + ")),     se="cluster", cluster=~ra_id, data=dat)
    )
  }

  list(
    extensive  = run_iv4("d_share_heads_positive_income",  "share_heads_positive_income_2000"),
    intensive  = run_iv4("dlog_mean_income_pos",  "log(mean_income_pos_2000)"),
    income_head = run_iv4("dlog_mean_income_head", "log(mean_income_head_2000)")
  )
}
