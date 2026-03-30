# =============================================================================
# 6_regressoes.R — Regressões centrais do artigo
# =============================================================================

# (A–C) Preparar amostra de regressão
preparar_amostra_regressoes <- function(censo_final) {

  # (A) Filtrar só o ano 2010 e montar censo_var_2010
  censo_var_2010 <- censo_final %>%
    filter(ano == 2010)

  # (B) Extrair controles de 2000
  controles_2000 <- censo_final %>%
    filter(ano == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           prop_ens_sup,
           prop_analfabetos,
           prop_over_65,
           dist_centro_brasilia,
           renda_per_capita,
           prop_ens_sup_completo,
           pop,
           dist_rodovia) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  # (C) Juntar controles ao censo de 2010
  censo_var_2010 %>%
    left_join(controles_2000, by = "code_tract")
}

# (D) Rodar as regressões
rodar_regressoes <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  # (1) Só renda baseline + FE de RA
  eq_iv_1 <- feols(
    var_renda_per_capita_log ~ log(renda_per_capita_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (2) Controles geográficos + FE de RA
  eq_iv_2 <- feols(
    var_renda_per_capita_log ~ log(renda_per_capita_2000) + log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (3) Controles socioeconômicos + FE de RA
  eq_iv_3 <- feols(
    var_renda_per_capita_log ~ log(renda_per_capita_2000) + prop_ens_sup_completo_2000 + prop_analfabetos_2000 + prop_over_65_2000 + log(pop_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (4) Todos os controles + FE de RA
  eq_iv_4 <- feols(
    var_renda_per_capita_log ~ log(renda_per_capita_2000) + log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) + prop_ens_sup_completo_2000 + prop_analfabetos_2000 + prop_over_65_2000 + log(pop_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  list(eq_iv_1 = eq_iv_1, eq_iv_2 = eq_iv_2, eq_iv_3 = eq_iv_3, eq_iv_4 = eq_iv_4)
}
