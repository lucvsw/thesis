# =============================================================================
# 10_heterogeneity_by_RA.R — Heterogeneity of the subway effect by exposed RA
# =============================================================================
#
# Goal: check whether the subway effect on income differs across the RAs
# exposed to the system.
#
# Strategy:
#   (A) Single regression with RA × treatment interactions (full 10 km sample)
#       Each interaction dummy_RA_X:dummy_1000m is instrumented by
#       dummy_RA_X:dummyp_1000m — analogous to a Bartik estimator.
#       The coefficient on dummy_1000m is the base effect (non-exposed RAs),
#       and each interaction coefficient is the DIFFERENTIAL effect in that RA
#       relative to the base.
#
#   (B) Separate regressions for each exposed RA (IV subsample)
#       No RA FE (redundant within a subsample), heteroskedasticity-robust standard errors.
#       Useful for reading the effect in each RA directly.
#
# Base specification: eq_iv_4 of 6_main_regressions.R (all controls + RA FE)
# Preferred threshold: 1000 m
# =============================================================================

# All RAs defined as exposed in 4_new_variables.R
RAs_EXPOSED <- c("1", "2", "3", "8", "9", "10", "12", "15", "19", "20")

# RAs identifiable in the interaction model (diagnostics in results_heterogeneity_RA.md):
#   RA 1  excluded: cor(D,Z) = 1 — D = Z exactly for every tract
#   RA 2  excluded: n = 1
#   RA 8  excluded: no treated tracts (D = 0 for all)
#   RA 15 excluded: no treated tracts (D = 0 for all)
#   RA 19 excluded: no treated tracts (D = 0 for all)
#   RA 20 excluded: almost all tracts treated (137/143), first-stage F = 0.56
RAs_INTERACTION <- c("3", "9", "10", "12")

# -----------------------------------------------------------------------------
# (A) Single regression with RA × treatment interactions
# -----------------------------------------------------------------------------
run_heterogeneity_by_RA <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  # Endogenous terms: base treatment + interactions for the 4 identifiable RAs
  endog_terms   <- c(
    "dummy_1000m",
    paste0("dummy_RA_", RAs_INTERACTION, ":dummy_1000m")
  )

  # Corresponding instruments: base planned alignment + RA × planned-alignment interactions
  instrum_terms <- c(
    "dummyp_1000m",
    paste0("dummy_RA_", RAs_INTERACTION, ":dummyp_1000m")
  )

  fml <- as.formula(paste0(
    "dlog_income_per_capita ~ ",
    "log(income_per_capita_2000) + log(dist_cbd_2000) + log(dist_highway_2000) + ",
    "share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 + log(pop_2000)",
    " | ra_id",
    " | ", paste(endog_terms,  collapse = " + "),
    " ~ ", paste(instrum_terms, collapse = " + ")
  ))

  feols(fml, data = dat, se = "cluster", cluster = ~ra_id)
}

# -----------------------------------------------------------------------------
# (B) Separate regressions for each exposed RA (IV subsample)
# -----------------------------------------------------------------------------
run_iv_by_RA <- function(smpl) {

  dat <- smpl %>% filter(dummy_subway_10km == 1)

  results <- lapply(RAs_EXPOSED, function(ra) {

    dat_ra <- dat %>% filter(ra_id == ra)

    # Minimum checks for variation before running the IV
    has_variation_d <- length(unique(dat_ra$dummy_1000m)) > 1
    has_variation_z <- length(unique(dat_ra$dummyp_1000m)) > 1

    if (!has_variation_d || !has_variation_z) {
      message("RA ", ra, ": no variation in treatment or instrument — regression skipped.")
      return(NULL)
    }

    tryCatch(
      feols(
        dlog_income_per_capita ~
          log(income_per_capita_2000) + log(dist_cbd_2000) + log(dist_highway_2000) +
          share_college_complete_2000 + share_illiterate_2000 + share_over_65_2000 + log(pop_2000)
        | dummy_1000m ~ dummyp_1000m,
        data = dat_ra,
        se   = "hetero"
      ),
      error = function(e) {
        message("RA ", ra, ": estimation error — ", conditionMessage(e))
        NULL
      }
    )
  })

  names(results) <- paste0("RA_", RAs_EXPOSED)
  results
}

# # Model A — regression with interactions (feols object)
# m_interactions <- tar_read(results_heterogeneity_RA)
# summary(m_interactions)
# 
# # Model B — list with a separate IV per RA (one feols object per RA, or NULL if not identifiable)
# m_by_ra <- tar_read(results_iv_by_RA)
# 
# # See which RAs were estimated successfully
# names(m_by_ra)                          # all RAs attempted
# Filter(Negate(is.null), m_by_ra)        # only those that ran
# 
# # Access a specific RA
# summary(m_by_ra$RA_9)   # Ceilândia
# summary(m_by_ra$RA_3)   # Taguatinga
