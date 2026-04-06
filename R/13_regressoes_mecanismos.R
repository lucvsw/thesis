# =============================================================================
# 13_regressoes_mecanismos.R — Hipótese 2: Efeito de Composição (Gentrificação)
#
# Pergunta: o acesso ao metrô alterou a composição demográfica dos setores,
#           sugerindo deslocamento de população de baixa renda (gentrificação)?
# VDs: Δ prop. mulheres | Δ prop. famílias numerosas | Δ taxa analfab. responsáveis
# Estratégia: 2SLS idêntica ao 6_regressoes.R
# =============================================================================

preparar_amostra_mecanismos <- function(censo_final) {

  # Controles de 2000
  controles_2000 <- censo_final %>%
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
           prop_mulheres,
           prop_fam_num,
           prop_analf_resp,
           prop_preto_pardo) %>%
    rename_with(~ paste0(.x, "_2000"), -code_tract)

  # Base: ano 2010 com variações demográficas
  censo_final %>%
    filter(ano == 2010) %>%
    left_join(controles_2000, by = "code_tract") %>%
    mutate(
      var_prop_mulheres    = prop_mulheres    - prop_mulheres_2000,
      var_prop_fam_num     = prop_fam_num     - prop_fam_num_2000,
      var_prop_analf_resp  = prop_analf_resp  - prop_analf_resp_2000,
      var_prop_preto_pardo = prop_preto_pardo - prop_preto_pardo_2000
    )
}

rodar_regressoes_mecanismos <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  # Helper: roda 4 especificações IV para uma VD e seu baseline
  run_iv4 <- function(vd, baseline) {
    geo   <- "log(dist_centro_brasilia_2000) + log(dist_rodovia_2000)"
    socio <- "prop_ens_sup_completo_2000 + prop_analfabetos_2000 + prop_over_65_2000 + log(pop_2000)"
    renda <- "log(renda_per_capita_2000)"
    iv    <- "log(dummy_1000m + 1) ~ log(dummyp_1000m + 1)"

    mk <- function(controles) {
      as.formula(paste0(vd, " ~ ", controles, " | ra_cira | ", iv))
    }

    list(
      m1 = feols(mk(baseline),                                           se = "cluster", cluster = ~ra_cira, data = dados),
      m2 = feols(mk(paste(baseline, geo,           sep = " + ")),        se = "cluster", cluster = ~ra_cira, data = dados),
      m3 = feols(mk(paste(baseline, socio, renda,  sep = " + ")),        se = "cluster", cluster = ~ra_cira, data = dados),
      m4 = feols(mk(paste(baseline, geo, socio, renda, sep = " + ")),    se = "cluster", cluster = ~ra_cira, data = dados)
    )
  }

  list(
    mulheres    = run_iv4("var_prop_mulheres",    "prop_mulheres_2000"),
    fam_num     = run_iv4("var_prop_fam_num",     "prop_fam_num_2000"),
    analf_resp  = run_iv4("var_prop_analf_resp",  "prop_analf_resp_2000"),
    preto_pardo = run_iv4("var_prop_preto_pardo", "prop_preto_pardo_2000")
  )
}

# # -----------------------------------------------------------------------------
# # Leitura dos resultados
# # -----------------------------------------------------------------------------
# res <- tar_read(resultados_regressoes_mecanismos)
# 
# # Δ Proporção de mulheres
# summary(res$mulheres$m1)
# summary(res$mulheres$m2)
# summary(res$mulheres$m3)
# summary(res$mulheres$m4)
# # Δ Proporção de famílias numerosas
# summary(res$fam_num$m1)
# summary(res$fam_num$m2)
# summary(res$fam_num$m3)
# summary(res$fam_num$m4)
# # Δ Taxa de analfabetismo dos responsáveis
# summary(res$analf_resp$m1)
# summary(res$analf_resp$m2)
# summary(res$analf_resp$m3)
# summary(res$analf_resp$m4)
