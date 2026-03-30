# =============================================================================
# 9_robustez_intensidade.R — Robustez: variáveis de intensidade de exposição
# Especificação: eq_iv_4 (todos os controles + FE de RA, amostra 10km)
# Substitui as dummies binárias por variáveis contínuas de intensidade
# int_Xm = max(0, 1 - dist_estacao / X), instr.: intp_Xm (projeto planejado)
# =============================================================================

robustez_intensidade <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  thresholds <- c(500, 1000, 1500, 2000, 2500)

  controles <- "log(renda_per_capita_2000) + log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) + prop_ens_sup_completo_2000 + prop_analfabetos_2000 + prop_over_65_2000 + log(pop_2000)"

  models <- lapply(thresholds, function(t) {
    feols(
      as.formula(paste0(
        "var_renda_per_capita_log ~ ", controles,
        " | ra_cira",
        " | log(int_", t, "m + 1) ~ log(intp_", t, "m + 1)"
      )),
      data = dados,
      se = "cluster", cluster = ~ra_cira
    )
  })

  names(models) <- paste0("int", thresholds, "m")
  models
}

# tar_load(resultados_robustez_intensidade)
# 
# # Ver uma threshold específico
# summary(resultados_robustez_intensidade$int2500m)
