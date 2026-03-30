# =============================================================================
# 11_sorting.R — Evidências de sorting: população, domicílios,
#                renda por domicílio e proporção de apartamentos
# =============================================================================
#
# Objetivo: verificar se o metrô causou realocação de pessoas e/ou mudanças
# na composição dos domicílios nas áreas próximas às estações. Resultados
# nulos reforçam que o efeito sobre renda (6_regressoes.R) reflete crescimento
# in situ, e não sorting de residentes mais ricos.
#
# Outcomes:
#   (A) var_pop_log        = log(pop_2010) - log(pop_2000)
#   (B) var_domicilios_log = log(domicilios_2010) - log(domicilios_2000)
#   (C) var_renda_dom_log  = log(renda_dom_2010) - log(renda_dom_2000)
#          renda_por_domicilio: testa se o metrô atraiu domicílios de maior
#          renda (sorting por tipo de residência, não por pessoas)
#   (D) var_prop_apt_nivel = prop_apt_2010 - prop_apt_2000  [nível, não log]
#          proporção de apartamentos: testa gentrificação/mudança de uso do
#          solo; inclui setores que partiram de zero apartamentos
#
# Estrutura: 4 especificações com adição progressiva de controles, espelhando
# 6_regressoes.R. Controle baseline = nível inicial do próprio outcome.
# log(renda_per_capita_2000) entra apenas nas especificações (3) e (4).
# Threshold e instrumento: 1000 m. Amostra: dummy_metro_10km == 1.
# =============================================================================

# -----------------------------------------------------------------------------
# (A) Crescimento populacional
# -----------------------------------------------------------------------------
rodar_regressoes_populacao <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  # (1) População baseline + FE de RA
  pop_iv_1 <- feols(
    var_pop_log ~ log(pop_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (2) + Controles geográficos
  pop_iv_2 <- feols(
    var_pop_log ~
      log(pop_2000) +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (3) + Controles socioeconômicos (inclui renda_2000)
  pop_iv_3 <- feols(
    var_pop_log ~
      log(pop_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (4) Todos os controles [especificação completa]
  pop_iv_4 <- feols(
    var_pop_log ~
      log(pop_2000) +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  list(pop_iv_1 = pop_iv_1, pop_iv_2 = pop_iv_2,
       pop_iv_3 = pop_iv_3, pop_iv_4 = pop_iv_4)
}

# -----------------------------------------------------------------------------
# (B) Crescimento de domicílios
# -----------------------------------------------------------------------------
rodar_regressoes_domicilios <- function(amostra, censo_final) {

  # Construir var_domicilios_log a partir do painel completo (censo_final),
  # pois essa variação não é pré-computada em criar_variacoes()
  dom_vars <- censo_final %>%
    st_drop_geometry() %>%
    filter(dummy_metro_10km == 1) %>%
    arrange(code_tract, ano) %>%
    group_by(code_tract) %>%
    summarise(
      dom_2000 = first(domicilios[ano == 2000]),
      dom_2010 = first(domicilios[ano == 2010]),
      .groups  = "drop"
    ) %>%
    mutate(
      var_domicilios_log = ifelse(
        dom_2000 > 0 & dom_2010 > 0,
        log(dom_2010) - log(dom_2000),
        NA_real_
      )
    )

  dados <- amostra %>%
    filter(dummy_metro_10km == 1) %>%
    left_join(
      dom_vars %>% select(code_tract, dom_2000, var_domicilios_log),
      by = "code_tract"
    )

  # (1) Domicílios baseline + FE de RA
  dom_iv_1 <- feols(
    var_domicilios_log ~ log(dom_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (2) + Controles geográficos
  dom_iv_2 <- feols(
    var_domicilios_log ~
      log(dom_2000) +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (3) + Controles socioeconômicos (inclui renda_2000)
  dom_iv_3 <- feols(
    var_domicilios_log ~
      log(dom_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (4) Todos os controles [especificação completa]
  dom_iv_4 <- feols(
    var_domicilios_log ~
      log(dom_2000) +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  list(dom_iv_1 = dom_iv_1, dom_iv_2 = dom_iv_2,
       dom_iv_3 = dom_iv_3, dom_iv_4 = dom_iv_4)
}

# -----------------------------------------------------------------------------
# (C) Crescimento da renda por domicílio
# -----------------------------------------------------------------------------
rodar_regressoes_renda_domicilio <- function(amostra, censo_final) {

  # Construir variação e baseline a partir do painel completo
  rend_dom_vars <- censo_final %>%
    st_drop_geometry() %>%
    filter(dummy_metro_10km == 1) %>%
    group_by(code_tract) %>%
    summarise(
      rdpc_dom_2000      = first(renda_por_domicilios[ano == 2000]),
      rdpc_dom_2010      = first(renda_por_domicilios[ano == 2010]),
      .groups = "drop"
    ) %>%
    mutate(
      var_renda_dom_log = ifelse(
        rdpc_dom_2000 > 0 & rdpc_dom_2010 > 0,
        log(rdpc_dom_2010) - log(rdpc_dom_2000),
        NA_real_
      )
    )

  dados <- amostra %>%
    filter(dummy_metro_10km == 1) %>%
    left_join(
      rend_dom_vars %>% select(code_tract, rdpc_dom_2000, var_renda_dom_log),
      by = "code_tract"
    )

  # (1) Renda por domicílio baseline + FE de RA
  rdm_iv_1 <- feols(
    var_renda_dom_log ~ log(rdpc_dom_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (2) + Controles geográficos
  rdm_iv_2 <- feols(
    var_renda_dom_log ~
      log(rdpc_dom_2000) +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (3) + Controles socioeconômicos (inclui renda per capita 2000)
  rdm_iv_3 <- feols(
    var_renda_dom_log ~
      log(rdpc_dom_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (4) Todos os controles [especificação completa]
  rdm_iv_4 <- feols(
    var_renda_dom_log ~
      log(rdpc_dom_2000) +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  list(rdm_iv_1 = rdm_iv_1, rdm_iv_2 = rdm_iv_2,
       rdm_iv_3 = rdm_iv_3, rdm_iv_4 = rdm_iv_4)
}

# -----------------------------------------------------------------------------
# (D) Mudança na proporção de apartamentos
# -----------------------------------------------------------------------------
rodar_regressoes_prop_apartamentos <- function(amostra, censo_final) {

  # Obter prop_apt_2000 do painel (não está em preparar_amostra_regressoes)
  prop_apt_vars <- censo_final %>%
    st_drop_geometry() %>%
    filter(dummy_metro_10km == 1, ano == 2000) %>%
    group_by(code_tract) %>%
    summarise(prop_apt_2000 = first(prop_apartamentos), .groups = "drop")

  dados <- amostra %>%
    filter(dummy_metro_10km == 1) %>%
    left_join(prop_apt_vars, by = "code_tract")

  # (1) Proporção de apartamentos baseline + FE de RA
  # Nota: baseline em nível (não log) pois prop_apt pode ser zero
  apt_iv_1 <- feols(
    var_prop_apt_nivel ~ prop_apt_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (2) + Controles geográficos
  apt_iv_2 <- feols(
    var_prop_apt_nivel ~
      prop_apt_2000 +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (3) + Controles socioeconômicos (inclui renda 2000)
  apt_iv_3 <- feols(
    var_prop_apt_nivel ~
      prop_apt_2000 +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  # (4) Todos os controles [especificação completa]
  apt_iv_4 <- feols(
    var_prop_apt_nivel ~
      prop_apt_2000 +
      log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) +
      log(renda_per_capita_2000) + prop_ens_sup_completo_2000 +
      prop_analfabetos_2000 + prop_over_65_2000
    | ra_cira
    | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
    data = dados, se = "cluster", cluster = ~ra_cira
  )

  list(apt_iv_1 = apt_iv_1, apt_iv_2 = apt_iv_2,
       apt_iv_3 = apt_iv_3, apt_iv_4 = apt_iv_4)
}

# # Ler cada resultado
# res_pop <- tar_read(resultados_populacao)
# res_dom <- tar_read(resultados_domicilios)
# res_rdm <- tar_read(resultados_renda_domicilio)
# res_apt <- tar_read(resultados_prop_apt)
# 
# # Especificação completa de cada outcome
# # População ---
# summary(res_pop$pop_iv_1)
# summary(res_pop$pop_iv_2)
# summary(res_pop$pop_iv_3)
# summary(res_pop$pop_iv_4)
# # Domicílios --
# summary(res_dom$dom_iv_1)
# summary(res_dom$dom_iv_2)
# summary(res_dom$dom_iv_3)
# summary(res_dom$dom_iv_4)
# # Renda por domicílios 
# summary(res_rdm$rdm_iv_1)
# summary(res_rdm$rdm_iv_2)
# summary(res_rdm$rdm_iv_3)
# summary(res_rdm$rdm_iv_4)
# # Número de apartamentos
# summary(res_apt$apt_iv_1)
# summary(res_apt$apt_iv_2)
# summary(res_apt$apt_iv_3)
# summary(res_apt$apt_iv_4)
