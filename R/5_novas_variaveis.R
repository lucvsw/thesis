# Unir o censo com os shapefiles do metro para criar dummies de exposição ao metro
novas_variaveis <- function(censo_sf_corrigido, linhas_sf, projeto_metro_sf) {
  # Reprojetar para CRS métrico (UTM zona 23S)
  censo_sf <- st_transform(censo_sf_corrigido, 32723)
  linhas_sf_corr   <- st_transform(linhas_sf, 32723)
  projeto_sf_corr  <- st_transform(projeto_metro_sf, 32723)
  
  # Unir as linhas
  metro_union   <- st_union(linhas_sf_corr)
  projeto_union <- st_union(projeto_sf_corr)
  
  # Limite de distância para definir exposição (em metros)
  limite1 <- 1000
  
  # Checar se a geometria do setor está a até 1 km das linhas
  exposto_metro   <- st_is_within_distance(censo_sf, metro_union, dist = limite1)
  exposto_projeto <- st_is_within_distance(censo_sf, projeto_union, dist = limite1)
  
  # Centroide de cada setor
  centroides <- st_centroid(censo_sf$geom)
  
  # Ponto do centro de Brasília (convertido para o mesmo CRS)
  centro_brasilia <- st_sfc(st_point(c(-47.884, -15.7921)), crs = 4326) %>%
    st_transform(32723)
  
  # Calcular distância do centro de Brasília (em metros)
  dist_centro_brasilia <- st_distance(centroides, centro_brasilia)
  
  # Adicionar variáveis ao censo
  censo_sf <- censo_sf %>%
    mutate(
      dummy_metro_passa   = as.integer(lengths(exposto_metro) > 0),
      dummy_projeto_passa = as.integer(lengths(exposto_projeto) > 0),
      renda_per_capita = renda_total / pop,
      renda_por_domicilios = renda_total / domicilios,
      dist_centro_brasilia = as.numeric(dist_centro_brasilia),
      over_65 = rowSums(across(c(idade_65, idade_70, idade_75, idade_80)), na.rm = TRUE),
      prop_over_65 = over_65 / pop,
      prop_ens_sup = ens_superior / domicilios,
      prop_analfabetos = analfabetos / pop,
      empregados = domicilios - sem_renda
    )
  
  return(censo_sf)
}



###############################################################################

# # Função para construir fórmula IV
# criar_formula_iv <- function(y, endog, instrumento) {
#   as.formula(paste0("log(", y, ") ~ ", endog, " | ", instrumento))
# }
# 
# rodar_iv <- function(data, ano, y, endog, instrumento) {
#   df_filtrado <- subset(data, ano == ano)
#   
#   # Variáveis usadas
#   vars_usadas <- c(y, endog, instrumento)
#   
#   # Filtrar NA, Inf e zeros/negativos na variável dependente (antes do log)
#   df_filtrado <- df_filtrado %>%
#     dplyr::filter(
#       dplyr::if_all(dplyr::all_of(vars_usadas), ~ is.finite(.)) &
#         !!rlang::sym(y) > 0  # remove pop <= 0
#     )
#   
#   # Criar fórmula
#   formula_iv <- criar_formula_iv(y, endog, instrumento)
#   
#   # Rodar IV
#   ivreg(formula_iv, data = df_filtrado)
# }
# 
# 
# # Função que roda para vários anos
# rodar_ivreg <- function(data, anos = c(2000, 2010), 
#                         y = "pop", endog = "dummy_metro", 
#                         instrumento = "dummy_projeto") {
#   modelos <- lapply(anos, function(ano_atual) {
#     cat("Rodando IV para o ano", ano_atual, "\n")
#     rodar_iv(data, ano_atual, y, endog, instrumento)
#   })
#   names(modelos) <- paste0("IV_", y, "_", anos)
#   modelos
# }


# 
# # Combinar os dois plots lado a lado
# grid.arrange(p_metro, p_projeto, ncol = 1)
# 
# ### Scatterplot
# ggplot(censo_sf, aes(x = dist_metro, y = log_diff_pop)) +
#   geom_point(alpha = 0.5) +
#   labs(
#     x = "Distância ao Metrô",
#     y = "Log da Diferença Populacional (2010 - 2000)",
#     title = "Scatterplot: Crescimento Populacional vs. Distância ao Metrô"
#   ) +
#   theme_minimal()
# 
# ###############################################################################
# 
# eq3 <- feols(log_diff_pop  ~ - 1 | dummy_metro ~ dummy_projeto,
#              se="hetero", data=censo_sf)
# summary(eq3)
# summary(eq3, stage=1)
