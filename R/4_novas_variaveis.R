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


# Excluir duplicatas
excluir_duplicatas <- function(censo_sf) {
  # 1. WKT(s) a excluir
  wkts_excluir <- c(
    "MULTIPOLYGON (((159646.5 8257491, 159760 8257368, 160033.9 8257387, 160156.7 8257491, 160114.2 8257779, 159887.6 8257938, 159731.6 8257860, 159618.2 8257680, 159646.5 8257491)))",
    "MULTIPOLYGON (((189379.5 8268313, 189415.3 8267982, 191055.3 8268016, 191019.6 8268385, 189417.3 8268374, 189379.5 8268313)))",
    "MULTIPOLYGON (((193372.7 8271524, 193387 8271194, 193515.8 8271079, 193701.9 8271075, 193835.6 8271166, 193890.5 8271450, 193804.6 8271579, 193637.6 8271649, 193372.7 8271524)))",
    "MULTIPOLYGON (((229916.7 8270944, 229130.5 8271730, 227740.2 8270280, 228951 8269101, 230375.4 8270552, 229916.7 8270944)))",
    "MULTIPOLYGON (((212084.6 8264586, 212172.1 8264319, 212656 8264076, 213195.1 8264052, 213214.6 8264547, 212685.1 8264868, 212320.8 8264872, 212077.9 8264693, 212084.6 8264586)))",
    "MULTIPOLYGON (((201237.6 8246589, 200872.3 8246257, 200540.3 8246122, 200685.8 8245913, 200587 8245411, 201009.2 8245359, 201009.1 8245621, 201399.4 8245673, 201448.9 8246489, 201314.9 8246659, 201237.6 8246589)))",
    "MULTIPOLYGON (((203576.2 8247249, 203445.4 8247404, 203211.8 8247487, 202921.8 8247337, 202860.2 8247008, 203127.2 8246751, 203363.2 8246741, 203589 8246865, 203576.2 8247249)))",
    "MULTIPOLYGON (((226284.9 8233013, 227156.9 8233033, 227156.9 8234028, 227085.8 8234281, 226265.5 8234168, 226194.5 8233091, 226284.9 8233013)))",
    "MULTIPOLYGON (((163723.1 8252898, 163472.4 8252891, 163271.1 8252561, 163440.6 8252285, 163642 8252243, 163748.4 8252305, 163907 8252541, 163870 8252747, 163723.1 8252898)))",
    "MULTIPOLYGON (((161116.4 8242864, 161092.6 8242622, 161164.2 8242553, 161403.1 8242543, 161485 8242619, 161512.3 8242745, 161399.6 8242912, 161226.1 8242954, 161116.4 8242864)))",
    "MULTIPOLYGON (((162354.6 8240017, 162532 8239914, 162665.1 8239938, 162747.1 8240014, 162740.2 8240239, 162488 8240348, 162378.5 8240259, 162354.6 8240017)))",
    "MULTIPOLYGON (((152388.9 8234568, 152365 8234326, 152436.7 8234257, 152675.5 8234246, 152757.5 8234322, 152784.8 8234449, 152672.1 8234616, 152498.5 8234658, 152388.9 8234568)))",
    "MULTIPOLYGON (((154221.5 8234981, 154399 8234879, 154532 8234903, 154613.9 8234978, 154607.1 8235203, 154355 8235313, 154245.4 8235224, 154221.5 8234981)))",
    "MULTIPOLYGON (((155916.2 8235588, 155892.4 8235344, 155964 8235277, 156203 8235266, 156284.8 8235341, 156312.1 8235468, 156199.6 8235635, 156026 8235677, 155916.2 8235588)))",
    "MULTIPOLYGON (((197884.1 8243263, 197816.3 8243085, 197873.7 8242887, 198098 8242725, 198405.8 8242814, 198505 8243127, 198426.7 8243310, 198114 8243413, 197884.1 8243263)))",
    "MULTIPOLYGON (((198045.9 8243984, 197978 8243807, 198035.4 8243608, 198259.8 8243446, 198567.6 8243534, 198666.7 8243848, 198588.4 8244031, 198275.8 8244134, 198045.9 8243984)))",
    "MULTIPOLYGON (((198755.7 8242415, 198904.7 8242279, 199216.6 8242340, 199344.4 8242476, 199252.4 8242767, 199037.5 8242911, 198817.9 8242893, 198679.9 8242586, 198755.7 8242415)))",
    "MULTIPOLYGON (((199070.5 8243430, 199082.5 8243187, 199256 8243063, 199421.3 8243115, 199421.3 8243442, 199131 8243502, 199070.5 8243430)))",
    "MULTIPOLYGON (((200570.1 8243128, 200305.1 8243402, 199926.2 8243038, 199758 8242673, 200094 8242416, 200628 8243022, 200570.1 8243128)))",
    "MULTIPOLYGON (((199883.3 8238884, 199495.7 8238341, 199882.9 8237938, 199981.7 8237937, 200410.7 8238480, 200410.7 8238587, 200008.3 8238892, 199883.3 8238884)))",
    "MULTIPOLYGON (((201296.9 8239102, 201383.3 8239199, 201383.9 8239385, 201242.5 8239521, 200999.3 8239390, 201047.2 8239130, 201296.9 8239102)))",
    "MULTIPOLYGON (((201905.1 8238323, 201519.7 8238042, 201826.9 8237639, 202205.8 8237915, 201905.1 8238323)))",
    "MULTIPOLYGON (((205269.4 8269075, 205232.5 8268999, 205327.2 8268855, 205486.5 8268906, 205447.3 8269100, 205269.4 8269075)))"
    # adicione outros WKT aqui, se precisar
  )

  # 2. Função auxiliar para “normalizar” o WKT (remove espaços extras)
  normalize_wkt <- function(wkt) {
    gsub("\\s+", " ", trimws(wkt))
  }

  wkts_excluir_norm <- normalize_wkt(wkts_excluir)

  # 3. Filtra removendo qualquer feição cujo WKT bate com o da lista
  censo_sf_clean <- censo_sf %>%
    rowwise() %>%
    mutate(
      wkt = normalize_wkt(st_as_text(geom))
    ) %>%
    ungroup() %>%
    filter(!wkt %in% wkts_excluir_norm) %>%
    select(-wkt)

  return(censo_sf_clean)
}

# Variação da população
criar_variacoes <- function(censo_sf_clean) {
  censo_sf_clean <- censo_sf_clean %>%
    group_by(code_tract) %>%
    mutate(
      # valores de 2000 para referência
      pop_2000            = first(pop[ano == 2000], default = NA_real_),
      empregados_2000     = first(empregados[ano == 2000], default = NA_real_),
      renda_per_capita_2000 = first(renda_per_capita[ano == 2000], default = NA_real_),
      
      # diferenças em 2010
      var_pop  = if_else(ano == 2010, pop - pop_2000, NA_real_),
      var_empregados = if_else(ano == 2010, empregados - empregados_2000, NA_real_),
      var_renda_per_capita = if_else(ano == 2010, renda_per_capita - renda_per_capita_2000, NA_real_)
    ) %>%
    ungroup() %>%
    select(-pop_2000, -empregados_2000, -renda_per_capita_2000)
  
  return(censo_sf_clean)
}





