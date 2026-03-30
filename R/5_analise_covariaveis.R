# =============================================================================
# 5_analise_covariaveis.R
# Análise de importância de covariáveis para identificação do efeito do metrô
# Testa todas as 2^6 combinações de covariáveis x {sem FE, com FE} = 128 specs
# Especificação canônica: dummy_1000m ~ dummyp_1000m, amostra 10km
# =============================================================================

analise_importancia_covariaveis <- function(censo_final) {

  # ---------------------------------------------------------------------------
  # 1. PREPARAR AMOSTRA
  # ---------------------------------------------------------------------------
  censo_var_2010 <- censo_final %>%
    filter(ano == 2010)

  controles_2000 <- censo_final %>%
    filter(ano == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           dist_centro_brasilia,
           prop_ens_sup_completo,
           prop_analfabetos,
           prop_over_65,
           renda_per_capita,
           pop) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  amostra <- censo_var_2010 %>%
    left_join(controles_2000, by = "code_tract") %>%
    filter(dummy_metro_10km == 1) %>%
    st_drop_geometry()

  # ---------------------------------------------------------------------------
  # 2. COVARIÁVEIS CANDIDATAS
  # ---------------------------------------------------------------------------
  covariates <- c(
    dist_centro = "log(dist_centro_brasilia_2000)",
    ens_sup     = "prop_ens_sup_completo_2000",
    analfabetos = "prop_analfabetos_2000",
    over_65     = "prop_over_65_2000",
    renda_base  = "log(renda_per_capita_2000)",
    pop         = "log(pop_2000)"
  )
  cov_names <- names(covariates)

  # ---------------------------------------------------------------------------
  # 3. LOOP: 2^6 combinações x {sem FE / com FE}
  # ---------------------------------------------------------------------------
  combos <- expand.grid(rep(list(0:1), length(cov_names)))
  names(combos) <- cov_names

  resultados <- list()
  idx <- 1

  for (fe in c(FALSE, TRUE)) {
    for (i in seq_len(nrow(combos))) {
      sel    <- which(combos[i, ] == 1)
      rhs    <- if (length(sel) > 0) paste(covariates[sel], collapse = " + ") else "1"
      fe_str <- if (fe) "ra_cira" else "0"

      fml <- as.formula(paste0(
        "var_renda_per_capita_log ~ ", rhs,
        " | ", fe_str,
        " | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1)"
      ))

      tryCatch({
        mod    <- feols(fml, data = amostra, se = "cluster", cluster = ~ra_cira,
                        warn = FALSE, notes = FALSE)
        coef_m <- coef(mod)["fit_log(dummy_1000m + 1)"]
        pval_m <- pvalue(mod)["fit_log(dummy_1000m + 1)"]

        resultados[[idx]] <- as.data.frame(c(
          list(fe_ra     = fe,
               n_ctrl    = length(sel),
               controles = if (length(sel) > 0) paste(cov_names[sel], collapse = "+") else "(nenhum)"),
          setNames(as.list(combos[i, ]), paste0("inc_", cov_names)),
          list(coef    = coef_m,
               pval    = pval_m,
               pos_sig = !is.na(coef_m) & coef_m > 0 & !is.na(pval_m) & pval_m < 0.10)
        ))
      }, error = function(e) NULL)

      idx <- idx + 1
    }
  }

  bind_rows(resultados)
}
