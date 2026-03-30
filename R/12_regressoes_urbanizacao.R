# =============================================================================
# 12_regressoes_urbanizacao.R — Regressões com variável dependente de urbanização
#
# Pergunta: o acesso ao metrô aumentou a proporção de área urbanizada do setor?
# VD: variação na proporção de área urbana (MapBiomas) entre 2000 e 2010
# Estratégia: 2SLS idêntica ao 6_regressoes.R
# =============================================================================

preparar_amostra_urbanizacao <- function(censo_urbanizacao) {

  # Controles de 2000
  controles_2000 <- censo_urbanizacao %>%
    filter(ano == 2000) %>%
    st_drop_geometry() %>%
    select(code_tract,
           prop_ens_sup_completo,
           prop_analfabetos,
           prop_over_65,
           dist_centro_brasilia,
           renda_per_capita,
           pop,
           dist_rodovia,
           prop_urbano_mapbiomas) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  # Base: ano 2010 com variáveis de tratamento/instrumento
  censo_urbanizacao %>%
    filter(ano == 2010) %>%
    left_join(controles_2000, by = "code_tract") %>%
    mutate(
      # VD: variação na proporção urbanizada 2000 → 2010
      var_prop_urbano = prop_urbano_mapbiomas - prop_urbano_mapbiomas_2000
    )
}

rodar_regressoes_urbanizacao <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  # (1) Só prop_urbano baseline + FE de RA
  eq_urb_1 <- feols(
    var_prop_urbano ~ prop_urbano_mapbiomas_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (2) + controles geográficos
  eq_urb_2 <- feols(
    var_prop_urbano ~ prop_urbano_mapbiomas_2000 +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (3) + controles socioeconômicos
  eq_urb_3 <- feols(
    var_prop_urbano ~ prop_urbano_mapbiomas_2000 +
      prop_ens_sup_completo_2000 + prop_analfabetos_2000 +
      prop_over_65_2000 + log(pop_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (4) Todos os controles
  eq_urb_4 <- feols(
    var_prop_urbano ~ prop_urbano_mapbiomas_2000 +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) +
      prop_ens_sup_completo_2000 + prop_analfabetos_2000 +
      prop_over_65_2000 + log(pop_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  list(eq_urb_1 = eq_urb_1, eq_urb_2 = eq_urb_2,
       eq_urb_3 = eq_urb_3, eq_urb_4 = eq_urb_4)
}

# -----------------------------------------------------------------------------
# Leitura dos resultados
# -----------------------------------------------------------------------------
# res <- tar_read(resultados_regressoes_urbanizacao)
# 
# # Segundo estágio — efeito do metrô sobre variação de cobertura urbana
# etable(res$eq_urb_1, res$eq_urb_2, res$eq_urb_3, res$eq_urb_4,
#        stage = 2, headers = c("(1)", "(2)", "(3)", "(4)"))
# 
# # Primeiro estágio — diagnóstico do instrumento
# etable(res$eq_urb_1, res$eq_urb_2, res$eq_urb_3, res$eq_urb_4,
#        stage = 1, headers = c("(1)", "(2)", "(3)", "(4)"))
# 
# # Modelo específico
# summary(res$eq_urb_1)
# summary(res$eq_urb_2)
# summary(res$eq_urb_3)
# summary(res$eq_urb_4)
