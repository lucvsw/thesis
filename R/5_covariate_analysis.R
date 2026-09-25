# =============================================================================
# 5_covariate_analysis.R
# Covariate importance analysis for identifying the subway effect
# Tests all 2^6 combinations of covariates x {no FE, with FE} = 128 specs
# Canonical specification: dummy_1000m ~ dummyp_1000m, 10 km sample
# =============================================================================

covariate_importance_analysis <- function(census_final) {

  # ---------------------------------------------------------------------------
  # 1. PREPARE THE SAMPLE
  # ---------------------------------------------------------------------------
  census_var_2010 <- census_final %>%
    filter(year == 2010)

  controls_2000 <- census_final %>%
    filter(year == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           dist_cbd,
           share_college_complete,
           share_illiterate,
           share_over_65,
           income_per_capita,
           pop) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  smpl <- census_var_2010 %>%
    left_join(controls_2000, by = "code_tract") %>%
    filter(dummy_subway_10km == 1) %>%
    st_drop_geometry()

  # ---------------------------------------------------------------------------
  # 2. CANDIDATE COVARIATES
  # ---------------------------------------------------------------------------
  covariates <- c(
    dist_center = "log(dist_cbd_2000)",
    college     = "share_college_complete_2000",
    illiterate = "share_illiterate_2000",
    over_65     = "share_over_65_2000",
    income_base  = "log(income_per_capita_2000)",
    pop         = "log(pop_2000)"
  )
  cov_names <- names(covariates)

  # ---------------------------------------------------------------------------
  # 3. LOOP: 2^6 combinations x {no FE / with FE}
  # ---------------------------------------------------------------------------
  combos <- expand.grid(rep(list(0:1), length(cov_names)))
  names(combos) <- cov_names

  results <- list()
  idx <- 1

  for (fe in c(FALSE, TRUE)) {
    for (i in seq_len(nrow(combos))) {
      sel    <- which(combos[i, ] == 1)
      rhs    <- if (length(sel) > 0) paste(covariates[sel], collapse = " + ") else "1"
      fe_str <- if (fe) "ra_id" else "0"

      fml <- as.formula(paste0(
        "dlog_income_per_capita ~ ", rhs,
        " | ", fe_str,
        " | dummy_1000m ~ dummyp_1000m"
      ))

      tryCatch({
        mod    <- feols(fml, data = smpl, se = "cluster", cluster = ~ra_id,
                        warn = FALSE, notes = FALSE)
        coef_m <- coef(mod)["fit_dummy_1000m"]
        pval_m <- pvalue(mod)["fit_dummy_1000m"]

        results[[idx]] <- as.data.frame(c(
          list(fe_ra     = fe,
               n_ctrl    = length(sel),
               controls = if (length(sel) > 0) paste(cov_names[sel], collapse = "+") else "(none)"),
          setNames(as.list(combos[i, ]), paste0("inc_", cov_names)),
          list(coef    = coef_m,
               pval    = pval_m,
               pos_sig = !is.na(coef_m) & coef_m > 0 & !is.na(pval_m) & pval_m < 0.10)
        ))
      }, error = function(e) NULL)

      idx <- idx + 1
    }
  }

  bind_rows(results)
}
