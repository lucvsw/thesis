# =============================================================================
# 10_heterogeneidade_RAs.R — Heterogeneidade do efeito do metrô por RA exposta
# =============================================================================
#
# Objetivo: verificar se o efeito do metrô sobre a renda difere entre as RAs
# expostas ao sistema.
#
# Estratégia:
#   (A) Regressão única com interações RA × tratamento (toda a amostra de 10 km)
#       Cada interação dummy_RA_X:log(dummy_1000m + 1) é instrumentada por
#       dummy_RA_X:log(dummyp_1000m + 1) — analogia ao estimador de Bartik.
#       O coeficiente de log(dummy_1000m + 1) é o efeito base (RAs não expostas),
#       e cada coeficiente de interação é o DIFERENCIAL do efeito naquela RA
#       relativo à base.
#
#   (B) Regressões separadas por RA exposta (subsample IV)
#       Sem FE de RA (redundante ao subsetar), erros-padrão heterocedásticos.
#       Útil para leitura direta do efeito em cada RA.
#
# Especificação base: eq_iv_4 de 6_regressoes.R (todos os controles + FE de RA)
# Threshold preferencial: 1000 m
# =============================================================================

# Todas as RAs definidas como expostas em 4_novas_variaveis.R
RAs_EXPOSTAS <- c("1", "2", "3", "8", "9", "10", "12", "15", "19", "20")

# RAs identificáveis no modelo de interações (diagnóstico em resultados_heterogeneidade_RAs.md):
#   RA 1  excluída: cor(D,Z) = 1 — D = Z exatamente para todos os setores
#   RA 2  excluída: n = 1
#   RA 8  excluída: sem setores tratados (D = 0 para todos)
#   RA 15 excluída: sem setores tratados (D = 0 para todos)
#   RA 19 excluída: sem setores tratados (D = 0 para todos)
#   RA 20 excluída: quase todos tratados (137/143), F 1.º estágio = 0,56
RAs_INTERACAO <- c("3", "9", "10", "12")

# -----------------------------------------------------------------------------
# (A) Regressão única com interações RA × tratamento
# -----------------------------------------------------------------------------
rodar_heterogeneidade_RAs <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  # Termos endógenos: tratamento base + interações das 4 RAs identificáveis
  endog_terms   <- c(
    "log(dummy_1000m + 1)",
    paste0("dummy_RA_", RAs_INTERACAO, ":log(dummy_1000m + 1)")
  )

  # Instrumentos correspondentes: projeto base + interações RA × projeto
  instrum_terms <- c(
    "log(dummyp_1000m + 1)",
    paste0("dummy_RA_", RAs_INTERACAO, ":log(dummyp_1000m + 1)")
  )

  fml <- as.formula(paste0(
    "var_renda_per_capita_log ~ ",
    "log(renda_per_capita_2000) + log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) + ",
    "prop_ens_sup_completo_2000 + prop_analfabetos_2000 + prop_over_65_2000 + log(pop_2000)",
    " | ra_cira",
    " | ", paste(endog_terms,  collapse = " + "),
    " ~ ", paste(instrum_terms, collapse = " + ")
  ))

  feols(fml, data = dados, se = "cluster", cluster = ~ra_cira)
}

# -----------------------------------------------------------------------------
# (B) Regressões separadas por RA exposta (subsample IV)
# -----------------------------------------------------------------------------
rodar_iv_por_RA <- function(amostra) {

  dados <- amostra %>% filter(dummy_metro_10km == 1)

  resultados <- lapply(RAs_EXPOSTAS, function(ra) {

    dados_ra <- dados %>% filter(ra_cira == ra)

    # Verificações mínimas de variação para rodar o IV
    tem_variacao_d <- length(unique(dados_ra$dummy_1000m)) > 1
    tem_variacao_z <- length(unique(dados_ra$dummyp_1000m)) > 1

    if (!tem_variacao_d || !tem_variacao_z) {
      message("RA ", ra, ": sem variação no tratamento ou instrumento — regressão ignorada.")
      return(NULL)
    }

    tryCatch(
      feols(
        var_renda_per_capita_log ~
          log(renda_per_capita_2000) + log(dist_centro_brasilia_2000) + log(dist_rodovia_2000) +
          prop_ens_sup_completo_2000 + prop_analfabetos_2000 + prop_over_65_2000 + log(pop_2000)
        | log(dummy_1000m + 1) ~ log(dummyp_1000m + 1),
        data = dados_ra,
        se   = "hetero"
      ),
      error = function(e) {
        message("RA ", ra, ": erro na estimação — ", conditionMessage(e))
        NULL
      }
    )
  })

  names(resultados) <- paste0("RA_", RAs_EXPOSTAS)
  resultados
}

# # Modelo A — regressão com interações (objeto feols)                                   
# m_interacoes <- tar_read(resultados_heterog_RAs)
# summary(m_interacoes)
# 
# # Modelo B — lista com IV separado por RA (um objeto feols por RA, ou NULL se não identificável)
# m_por_ra <- tar_read(resultados_iv_por_RA)
# 
# # Ver quais RAs foram estimadas com sucesso
# names(m_por_ra)                          # todas as RAs tentadas
# Filter(Negate(is.null), m_por_ra)        # só as que rodaram
# 
# # Acessar uma RA específica
# summary(m_por_ra$RA_9)   # Ceilândia
# summary(m_por_ra$RA_3)   # Taguatinga
