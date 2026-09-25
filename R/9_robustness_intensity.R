# =============================================================================
# 9_robustness_intensity.R — Robustness: continuous exposure measures
# Specification: eq_iv_4 (all controls + RA FE, 10 km sample)
# Three functional forms, three bandwidth parameters each:
#   Logistic sigmoid: 1 / (1 + exp((dist - 1000) / sigma))
#   Exponential decay: exp(-dist / h)
#   Gaussian kernel: exp(-dist^2 / (2 * sigma^2))
# =============================================================================

robustness_intensity <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  controls <- paste(
    "log(income_per_capita_2000) + log(dist_cbd_2000) +",
    "log(dist_highway_2000) + share_college_complete_2000 +",
    "share_illiterate_2000 + share_over_65_2000 + log(pop_2000)"
  )

  run_iv <- function(d, endog, instr) {
    feols(
      as.formula(paste0(
        "dlog_income_per_capita ~ ", controls,
        " | ra_id | ", endog, " ~ ", instr
      )),
      data = d, se = "cluster", cluster = ~ra_id
    )
  }

  # ── Logistic sigmoid: T = 1000m, sigma = 50m / 200m / 400m ──────────────
  sig_models <- lapply(c(50, 200, 400), function(s) {
    d2 <- dat %>% mutate(
      sig_t = 1 / (1 + exp((dist_station - 1000) / s)),
      sig_z = 1 / (1 + exp((dist_planned  - 1000) / s))
    )
    run_iv(d2, "sig_t", "sig_z")
  })
  names(sig_models) <- paste0("sig_s", c(50, 200, 400))

  # ── Exponential decay: h = 750m / 1000m / 1500m ─────────────────────
  exp_models <- lapply(c(750, 1000, 1500), function(h) {
    d2 <- dat %>% mutate(
      exp_t = exp(-dist_station / h),
      exp_z = exp(-dist_planned  / h)
    )
    run_iv(d2, "exp_t", "exp_z")
  })
  names(exp_models) <- paste0("exp_h", c(750, 1000, 1500))

  # ── Gaussian kernel: sigma = 700m / 900m / 1200m ─────────────────────────
  gk_models <- lapply(c(700, 900, 1200), function(s) {
    d2 <- dat %>% mutate(
      gk_t = exp(-dist_station^2 / (2 * s^2)),
      gk_z = exp(-dist_planned^2  / (2 * s^2))
    )
    run_iv(d2, "gk_t", "gk_z")
  })
  names(gk_models) <- paste0("gk_s", c(700, 900, 1200))

  c(sig_models, exp_models, gk_models)
}
