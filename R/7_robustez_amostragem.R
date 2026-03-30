# =============================================================================
# 7_robustez_amostragem.R — Robustez: diferentes raios de amostragem
# Especificação: eq_iv_4 (todos os controles + FE de RA, threshold 1000m)
# Cada coluna usa um raio diferente para definir a amostra de setores
# =============================================================================

robustez_amostragem <- function(amostra) {

  # Raios testados e dummies correspondentes
  raios <- list(
    km_5    = "dummy_metro_5km",
    km_7_5  = "dummy_metro_7_5km",
    km_10   = "dummy_metro_10km",
    km_12_5 = "dummy_metro_12_5km",
    km_15   = "dummy_metro_15km",
    km_20   = "dummy_metro_20km"
  )

  lapply(raios, function(dummy) {
    feols(
      var_renda_per_capita_log ~
        log(renda_per_capita_2000) +
        log(dist_centro_brasilia_2000) +
        log(dist_rodovia_2000) +
        prop_ens_sup_completo_2000 +
        prop_analfabetos_2000 +
        prop_over_65_2000 +
        log(pop_2000)
      | ra_cira
      | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
      data    = amostra %>% filter(.data[[dummy]] == 1),
      se      = "cluster", cluster = ~ra_cira
    )
  })
}

# tar_load(resultados_robustez_amostragem)                                                  
# 
# # Ver uma distância específica                                 
# summary(resultados_robustez_amostragem$km_10)  
