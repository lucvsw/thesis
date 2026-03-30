# Unir o censo com os shapefiles do metro para criar dummies de exposição ao metro, dummies de RAs
novas_variaveis <- function(censo_sf_corrigido, linhas_sf, projeto_metro_sf, estacoes_sf, RAs_sf, rodovias_sf) {
  # 1) Reprojetar todos os objetos para um CRS métrico (UTM zona 23S)
  censo_sf        <- st_transform(censo_sf_corrigido, 32723)
  linhas_sf_corr  <- st_transform(linhas_sf,        32723)
  projeto_sf_corr <- st_transform(projeto_metro_sf, 32723)
  estacoes_corr   <- st_transform(estacoes_sf,      32723)
  RAs_corr        <- st_transform(RAs_sf,           32723)
  rodovias_corr   <- st_transform(rodovias_sf,      32723)
  
  # 2) Unir as linhas do metrô e do projeto para cálculo das dummies antigas
  metro_union    <- st_union(linhas_sf_corr)
  projeto_union  <- st_union(projeto_sf_corr)
  
  # 3) Centróides dos setores (para medir distâncias “ponto a ponto”)
  centroides     <- st_centroid(censo_sf$geom)
  
  # 4) Distância do centro de Brasília
  centro_brasilia <- st_sfc(st_point(c(-47.884, -15.7921)), crs = 4326) %>%
    st_transform(32723)
  dist_centro_brasilia <- st_distance(centroides, centro_brasilia)
  
  # 5a) Distância até a estação mais próxima
  #    - st_distance(centroides, estacoes) retorna uma matriz N x M
  #    - aplicamos min por linha
  dist_estacao_mat <- st_distance(centroides, estacoes_corr)
  dist_estacao     <- apply(dist_estacao_mat, 1, min)
  
  # 5b) Distância até o trecho de linha descartada
  dist_projeto_mat <- st_distance(centroides, projeto_union)
  dist_projeto     <- apply(dist_projeto_mat, 1, min)

  # 5c) Distância até a rodovia mais próxima
  # Filtrar geometrias corrompidas que causam ParseException no GEOS
  rodovias_validas <- rodovias_corr[sapply(seq_len(nrow(rodovias_corr)), function(i) {
    tryCatch({ st_union(rodovias_corr[i, ]); TRUE }, error = function(e) FALSE)
  }), ]
  rodovias_union    <- st_union(rodovias_validas)
  dist_rodovia_mat  <- st_distance(centroides, rodovias_union)
  dist_rodovia      <- apply(dist_rodovia_mat, 1, min)
  
  # 6) Limites para as novas dummies/intensidades
  limites <- c(500, 1000, 1500, 2000, 2500)

  # 7) Montar a tabela com novas variáveis
  censo_sf <- censo_sf %>%
    mutate(
      ## dummies de exposição às linhas de metrô
      dummy_metro_passa    = as.integer(lengths(st_is_within_distance(censo_sf, metro_union,    dist = limites[2])) > 0),
      dummy_projeto_passa_500  = as.integer(lengths(st_is_within_distance(censo_sf, projeto_union, dist = limites[1])) > 0),
      dummy_projeto_passa_1000  = as.integer(lengths(st_is_within_distance(censo_sf, projeto_union, dist = limites[2])) > 0),
      dummy_projeto_passa_1500  = as.integer(lengths(st_is_within_distance(censo_sf, projeto_union, dist = limites[3])) > 0),
      dummy_projeto_passa_2000  = as.integer(lengths(st_is_within_distance(censo_sf, projeto_union, dist = limites[4])) > 0),
      
      ## variáveis básicas
      renda_per_capita     = renda_total / pop,
      renda_por_domicilios = renda_total / domicilios,
      dist_centro_brasilia = as.numeric(dist_centro_brasilia),
      
      ## contagem / proporções demográficas
      over_65               = rowSums(across(c(idade_65, idade_70, idade_75, idade_80)), na.rm = TRUE),
      prop_over_65          = over_65 / pop,
      prop_ens_sup          = ens_superior / domicilios,
      prop_ens_sup_completo = ens_sup_completo / domicilios,
      prop_analfabetos      = analfabetos / pop,
      empregados            = domicilios - sem_renda,
      prop_apartamentos     = apartamentos / domicilios,

      ## proporções demográficas para análise de mecanismos (Hipótese 2)
      prop_mulheres   = ifelse(total_moradores > 0,
                               (total_moradores - moradores_homens) / total_moradores,
                               NA_real_),
      prop_fam_num    = ifelse(domicilios > 0, dom_5mais / domicilios, NA_real_),
      prop_analf_resp = ifelse(resp_heads_total > 0,
                               (resp_heads_total - resp_heads_alfab) / resp_heads_total,
                               NA_real_),

      ## distância contínua até estação mais próxima (em metros)
      dist_estacao          = as.numeric(dist_estacao),

      ## distância contínua até a rodovia mais próxima (em metros)
      dist_rodovia          = as.numeric(dist_rodovia),
      
      ## dummies de exposição:
      dummy_500m            = as.integer(dist_estacao <= limites[1]),
      dummy_1000m           = as.integer(dist_estacao <= limites[2]),
      dummy_1500m           = as.integer(dist_estacao <= limites[3]),
      dummy_2000m           = as.integer(dist_estacao <= limites[4]),
      dummy_2500m           = as.integer(dist_estacao <= limites[5]),
      
      ## intensidades: decaem linearmente de 1 (no ponto) a 0 (no limite)
      int_500m              = pmax(0, 1 - dist_estacao / limites[1]),
      int_1000m             = pmax(0, 1 - dist_estacao / limites[2]),
      int_1500m             = pmax(0, 1 - dist_estacao / limites[3]),
      int_2000m             = pmax(0, 1 - dist_estacao / limites[4]),
      int_2500m             = pmax(0, 1 - dist_estacao / limites[5]),
      
      ## distância contínua até trecho descartado (m)
      dist_projeto         = as.numeric(dist_projeto),
      
      # Dummies para amostragem
      dummy_metro_5km      = as.integer(dist_estacao <= 5000),
      dummy_metro_7_5km    = as.integer(dist_estacao <= 7500),
      dummy_metro_10km     = as.integer(dist_estacao <= 10000),
      dummy_metro_12_5km   = as.integer(dist_estacao <= 12500),
      dummy_metro_15km     = as.integer(dist_estacao <= 15000),
      dummy_metro_20km     = as.integer(dist_estacao <= 20000),
      
      ## dummies de exposição ao projeto descartado
      dummyp_500m          = as.integer(dist_projeto <= limites[1]),
      dummyp_1000m         = as.integer(dist_projeto <= limites[2]),
      dummyp_1500m         = as.integer(dist_projeto <= limites[3]),
      dummyp_2000m         = as.integer(dist_projeto <= limites[4]),
      dummyp_2500m         = as.integer(dist_projeto <= limites[5]),
      
      ## intensidades decrescentes para o projeto descartado
      intp_500m            = pmax(0, 1 - dist_projeto / limites[1]),
      intp_1000m           = pmax(0, 1 - dist_projeto / limites[2]),
      intp_1500m           = pmax(0, 1 - dist_projeto / limites[3]),
      intp_2000m           = pmax(0, 1 - dist_projeto / limites[4]),
      intp_2500m           = pmax(0, 1 - dist_projeto / limites[5])
    )
  
  # 8) Dummies por Região Administrativa
  # 8a)   área do setor
  censo_sf <- censo_sf %>% 
    mutate(area_setor = st_area(geom))
  
  # 8b)   interseção setor × RA  (cada par vira uma linha)
  interseccoes <- st_intersection(
    censo_sf %>% select(code_tract, area_setor),
    RAs_corr   %>% select(ra_cira)
  )
  
  # 8c)   proporção de cada RA dentro do setor
  interseccoes_unicas <- interseccoes %>% 
    mutate(
      area_inter = st_area(geom),
      pct        = as.numeric(area_inter / area_setor)
    ) %>% 
    # (opcional) exigir >50 % de sobreposição
    filter(pct > 0.5) %>%                             
    # garantir 1 RA por setor (a de maior pct)
    group_by(code_tract) %>% 
    slice_max(pct, with_ties = FALSE) %>% 
    ungroup() %>% 
    st_drop_geometry() %>%           # só atributos para o join
    select(code_tract, ra_cira)
  
  # 8d)   anexa RA dominante ao censo (sem duplicar setores)
  censo_sf <- censo_sf %>% 
    left_join(interseccoes_unicas, by = "code_tract") %>% 
    mutate(
      ra_cira = ifelse(is.na(ra_cira), "foraRA", ra_cira)
    ) %>% 
    select(-area_setor)               # coluna auxiliar já não é precisa
  
  # 8e)   matriz de dummies
  ra_mat <- model.matrix(~ ra_cira - 1, data = st_drop_geometry(censo_sf))
  colnames(ra_mat) <- sub("^ra_cira", "dummy_RA_", colnames(ra_mat))
  
  # 8f)   anexa dummies ao objeto espacial
  censo_sf <- bind_cols(censo_sf, as_tibble(ra_mat))
  
  # 8g) Dummy para RAs expostas ao metrô
  RAs_expostas <- c("1", "2", "3", "8", "9", "10", "12", "15", "19", "20")
  
  # 8h) Lista de setores a incluir manualmente (mesmo fora das RAs acima)
  code_tract_expostos_extra <- c(
    "5300108051400041",
    "5300108051400191",
    "5300108051400421",
    "5300108051400411"
  )
  
  # 8i) Criar dummy combinando RA + ajustes manuais
  censo_sf <- censo_sf %>%
    mutate(
      dummy_RA_exposta_metro = as.integer(
        ra_cira %in% RAs_expostas |
          code_tract %in% code_tract_expostos_extra
      )
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

# Criar variações em nível e log
criar_variacoes <- function(censo_sf_clean) {
  censo_sf_clean <- censo_sf_clean %>%
    group_by(code_tract) %>%
    mutate(
      # valores de 2000 como base
      pop_2000              = first(pop[ano == 2000], default = NA_real_),
      empregados_2000       = first(empregados[ano == 2000], default = NA_real_),
      renda_per_capita_2000 = first(renda_per_capita[ano == 2000], default = NA_real_),
      prop_apt_2000         = first(prop_apartamentos[ano == 2000], default = NA_real_),
      apartamentos_2000     = first(apartamentos[ano == 2000], default = NA_real_),
      
      # valores em 2010
      pop_2010              = first(pop[ano == 2010], default = NA_real_),
      empregados_2010       = first(empregados[ano == 2010], default = NA_real_),
      renda_per_capita_2010 = first(renda_per_capita[ano == 2010], default = NA_real_),
      prop_apt_2010         = first(prop_apartamentos[ano == 2010], default = NA_real_),
      apartamentos_2010     = first(apartamentos[ano == 2010], default = NA_real_),
      
      # diferenças em nível
      var_pop_nivel              = if_else(ano == 2010, pop_2010 - pop_2000, NA_real_),
      var_empregados_nivel       = if_else(ano == 2010, empregados_2010 - empregados_2000, NA_real_),
      var_renda_per_capita_nivel = if_else(ano == 2010, renda_per_capita_2010 - renda_per_capita_2000, NA_real_),
      var_prop_apt_nivel         = if_else(ano == 2010, prop_apt_2010 - prop_apt_2000, NA_real_),
      var_apt_nivel              = if_else(ano == 2010, apartamentos_2010 - apartamentos_2000, NA_real_),
      
      # diferenças em log
      var_pop_log = if_else(ano == 2010 & pop_2000 > 0 & pop_2010 > 0,
                            log(pop_2010) - log(pop_2000),
                            NA_real_),
      var_empregados_log = if_else(ano == 2010 & empregados_2000 > 0 & empregados_2010 > 0,
                                   log(empregados_2010) - log(empregados_2000),
                                   NA_real_),
      var_renda_per_capita_log = if_else(ano == 2010 & renda_per_capita_2000 > 0 & renda_per_capita_2010 > 0,
                                         log(renda_per_capita_2010) - log(renda_per_capita_2000),
                                         NA_real_),
      var_prop_apt_log = if_else(ano == 2010 & prop_apt_2000 > 0 & prop_apt_2010 > 0,
                                 log(prop_apt_2010) - log(prop_apt_2000),
                                 NA_real_),
      var_apt_log = if_else(ano == 2010 & apartamentos_2000 > 0 & apartamentos_2010 > 0,
                                 log(apartamentos_2010) - log(apartamentos_2000),
                                 NA_real_)
    ) %>%
    ungroup() %>%
    select(-pop_2000, -pop_2010, -empregados_2000, -empregados_2010, 
           -renda_per_capita_2000, -renda_per_capita_2010,
           -prop_apt_2000, -prop_apt_2010, -apartamentos_2010, apartamentos_2000)
  
  return(censo_sf_clean)
}

# Criando coeficientes de aproveitamento
criar_coeficientes <- function(censo_sf_clean) {
  # Tabela manual de coeficientes por setor censitário (code_tract)
  # Dentro do mesmo setor censitário, existem regiões onde o coeficiente básico varia, irei utilizar o coeficiente mais baixo nesse caso. Como o coeficiente básico define a capacidade de construção sem necessidade de contrapartidas, adotar o menor valor dentro do setor garante uma abordagem conservadora e evita superestimar o potencial construtivo médio, especialmente quando há zonas mais restritivas no interior do mesmo setor.
  # Para o máximo, irei utilizar o coeficiente mais alto dentro do setor. Como o coeficiente máximo representa o potencial construtivo total, inclusive considerando outorgas onerosas ou flexibilizações normativas, adotar o maior valor dentro do setor censitário é a abordagem mais adequada. Isso garante que se capture o limite superior da capacidade de adensamento
  coef_df <- tribble(
    ~code_tract,           ~cfa_b, ~cfa_m,
    # ----- GAMA -----
    # -- GAMA: SUL --
    "5300108050701391",    2.0,    4.0,
    "5300108050701361",    2.0,    4.0,
    "530010805070135",     2.0,    2.4,
    "530010805070134",     2.0,    2.4,
    "530010805070131",     2.0,    2.4,
    "5300108050701301",    2.0,    4.0,
    "5300108050701271",    2.0,    4.0,
    "5300108050701261",    2.0,    2.4,
    "530010805070123",     2.0,    2.4,
    "530010805070122",     2.0,    2.4,
    "5300108050701191",    2.0,    2.4,
    "5300108050701181",    2.0,    4.0,
    "530010805070115",     2.0,    2.4,
    "530010805070112",     2.0,    2.4,
    "5300108050701111",    2.0,    2.4,
    "5300108050701101",    0.25,   5.6,
    "530010805070116",     2.0,    5.6,
    "530010805070117",     2.0,    5.6,
    "530010805070120",     2.0,    5.6,
    "5300108050701211",    2.0,    5.6,
    "530010805070124",     2.0,    5.6,
    "530010805070125",     2.0,    5.6,
    "530010805070128",     2.0,    5.6,
    "530010805070129",     2.0,    5.6,
    "530010805070132",     2.0,    5.6,
    "530010805070133",     2.0,    5.6,
    "5300108050701371",    2.0,    5.6,
    "530010805070138",     2.0,    5.6,
    "530010805070113",     2.0,    2.0,
    "530010805070114",     2.0,    2.4,
    # -- GAMA: LESTE ---
    "530010805070141",     2.0,    4.0,
    "530010805070140",     2.0,    4.0,
    "530010805070055",     2.0,    4.0,
    "530010805070054",     0.25,   4.0,
    "530010805070056",     2.0,    4.0,
    "530010805070053",     2.0,    4.0,
    "530010805070052",     2.0,    4.0,
    "530010805070051",     2.0,    4.0,
    "530010805070050",     2.0,    4.0,
    "530010805070049",     2.0,    4.0,
    "530010805070043",     2.0,    4.0,
    "530010805070044",     2.0,    4.0,
    "5300108050700451",     2.0,    4.0,
    "530010805070046",     2.0,    4.0,
    "530010805070048",     2.0,    4.0,
    "530010805070047",     2.0,    4.0,
    "530010805070037",     2.0,    4.0,
    "530010805070038",     2.0,    4.0,
    "530010805070039",     2.0,    4.0,
    "530010805070040",     2.0,    4.0,
    "530010805070041",     2.0,    4.0,
    "530010805070042",     2.0,    4.0,
    "530010805070032",     2.0,    4.0,
    "530010805070033",     2.0,    4.0,
    "530010805070034",     2.0,    4.0,
    "530010805070035",     2.0,    4.0,
    "530010805070036",     2.0,    4.0,
    "530010805070025",     2.0,    4.0,
    "530010805070026",     2.0,    4.0,
    "530010805070027",     2.0,    4.0,
    "530010805070028",     2.0,    4.0,
    "530010805070029",     2.0,    4.0,
    "530010805070030",     2.0,    4.0,
    "530010805070031",     2.0,    4.0,
    # -- GAMA: CENTRAL --
    "530010805070020",     2.0,    8.0,
    "530010805070021",     2.0,    4.0,
    "530010805070022",     2.0,    8.0,
    "530010805070024",     2.0,    8.0,
    "530010805070023",     2.0,    8.0,
    "530010805070183",     2.0,    6.0,
    "530010805070019",     2.0,    8.0,
    "530010805070018",     2.0,    8.0,
    "530010805070017",     2.0,    8.0,
    "530010805070016",     2.0,    8.0,
    "530010805070015",     2.0,    8.0,
    "5300108050700071",     0.25,    8.0,  
    "530010805070008",     4.2,    8.0,
    "5300108050700041",     2.5,    8.0,
    "530010805070005",     8.0,    8.0,
    "530010805070006",     4.2,    8.0,
    "530010805070001",     4.2,    8.0,
    "530010805070002",     2.0,    8.0,
    "5300108050700031",     7.0,    7.0,
    "530010805070011",     7.0,    7.0,
    "530010805070010",     7.0,    7.0,
    "530010805070009",     2.0,    8.0,
    "530010805070013",     4.2,    8.0,
    "530010805070012",     4.2,    8.0,
    "530010805070014",     4.2,    8.0,
    # -- GAMA: NORTE --
    "5300108050700581",     2.0,    3.0,
    "530010805070057",     2.0,    2.4,
    "5300108050700591",     2.0,    3.0,
    "5300108050700601",     2.0,    3.0,
    "530010805070061",     2.0,    2.4,
    "530010805070062",     0.25,    3.0,
    "530010805070061",     2.0,    4.0,
    "530010805070187",     2.0,    4.0,
    "530010805070066",     2.0,    2.0,
    "530010805070145",     2.0,    4.0,
    "530010805070065",     2.0,    4.0,
    # -- GAMA: IND --
    "5300108050700631",     0.25,    5.6,
    "5300108050700641",     2.0,    4.0,
    # -- GAMA: LESTE --
    "530010805070067",     2.0,    4.0,
    "530010805070068",     2.0,    4.0,
    "530010805070069",     2.0,    4.0,
    "530010805070074",     2.0,    4.0,
    "530010805070073",     2.0,    4.0,
    "530010805070072",     2.0,    4.0,
    "5300108050700711",     2.0,    4.0,
    "530010805070075",     2.0,    4.0,
    "530010805070076",     2.0,    4.0,
    "5300108050700771",     2.0,    4.0,
    "530010805070078",     2.0,    4.0,
    "5300108050700791",     2.0,    2.0,
    "5300108050700801",     2.0,    2.0,
    "5300108050700811",     2.0,    4.0,
    "530010805070082",     2.0,    4.0,
    "530010805070083",     2.0,    4.0,
    "530010805070084",     2.0,    4.0,
    "5300108050700851",     2.0,    4.0,
    "530010805070092",     2.0,    4.0,
    "530010805070093",     2.0,    4.0,
    "530010805070091",     2.0,    4.0,
    "530010805070090",     2.0,    4.0,
    "530010805070089",     2.0,    4.0,
    "5300108050700881",     2.0,    4.0,
    "5300108050700971",     2.0,    4.0,
    "530010805070096",     2.0,    4.0,
    "5300108050700951",     2.0,    4.0,
    "5300108050700941",     2.0,    4.0,
    "5300108050700981",     2.0,    4.0,
    "5300108050700991",     2.0,    4.0,
    "530010805070100",     2.0,    4.0,
    "530010805070101",     2.0,    4.0,
    "530010805070102",     2.0,    4.0,
    "530010805070104",     2.0,    4.0,
    "530010805070103",     2.0,    4.0,
    "530010805070105",     0.25,    4.0,
    "530010805070106",     2.0,    4.0,
    "530010805070107",     2.0,    5.6,
    "530010805070108",     2.0,    4.0,
    "5300108050701091",     2.0,    5.6,
    "530010805070086",     2.0,    4.0,
    "5300108050700871",     2.0,    4.0,
    "5300108050700701",     2.0,    4.0,
    
    # -- Taguatinga --
    # -- TAG: SUL --
    "530010805080001",     2.0,    3.5,
    "530010805080014",     2.0,    3.5,
    "530010805080015",     2.0,    3.5,
    "5300108050800131",     2.0,    4.0,
    "530010805080031",     2.0,    4.0,
    "5300108050800301",     2.0,    4.0,
    "530010805080016",     2.0,    2.0,
    "530010805080017",     2.0,    2.0,
    "530010805080018",     2.0,    2.0,
    "530010805080027",     0.5,    4.0,
    "530010805080028",     2.0,    4.0,
    "530010805080024",     2.0,    4.0,
    "5300108050800231",     2.0,    4.0,
    "5300108050800201",     2.0,    4.0,
    "530010805080029",     2.0,    4.0,
    "530010805080026",     2.0,    4.0,
    "530010805080025",     2.0,    4.0,
    "530010805080022",     2.0,    4.0,
    "5300108050800211",     2.0,    4.0,
    "5300108050800191",     2.0,    4.0,
    "530010805080183",     2.0,    3.5,
    "530010805080184",     2.0,    3.0,
    "530010805080185",     2.0,    3.0,
    "530010805080196",     2.0,    3.0,
    "530010805080197",     2.0,    3.0,
    "530010805080198",     2.0,    3.0,
    "530010805080187",     2.0,    2.0,
    "530010805080188",     2.0,    3.0,
    "5300108050801891",     2.0,    2.0,
    "5300108050801951",     0.5,    3.8,
    "530010805080194",     2.0,    3.0,
    "530010805080199",     2.0,    3.0,
    "530010805080192",     2.0,    2.0,
    "530010805080191",     2.0,    2.0,
    "530010805080190",     2.0,    3.0,
    "530010805080186",     2.0,    3.0,
    "530010805080193",     2.0,    3.0,
    "5300108050802001",     0.5,    3.0,
    "530010805080462",     1.8,    1.8,
    "530010805080463",     1.8,    1.8,
    "5300108050802001",     2.0,    3.0,
    "530010805080225",     2.0,    3.0,
    "5300108050802281",     2.0,    9.0,
    "530010805080500",     2.0,    9.0,
    "530010805080227",     2.0,    2.5,
    "530010805080226",     2.0,    5.0,
    "5300108050802301",     2.0,    9.0,
    "530010805080229",     2.0,    2.5,
    "530010805080232",     2.0,    3.0,
    "530010805080233",     2.0,    9.0,
    "530010805080231",     2.0,    9.0,
    "5300108050802241",     2.0,    9.0,
    "5300108050802221",     2.0,    5.0,
    "5300108050802211",     2.0,    5.0,
    "530010805080214",     2.0,    5.0,
    "530010805080212",     2.0,    5.0,
    "530010805080211",     2.0,    5.0,
    "530010805080206",     2.0,    5.0,
    "530010805080205",     2.0,    3.0,
    "5300108050802031",     2.0,    2.0,
    "530010805080204",     2.0,    2.0,
    "5300108050802021",     2.0,    2.0,
    "530010805080201",     2.0,    5.0,
    "530010805080208",     2.0,    5.0,
    "530010805080207",     2.0,    5.0,
    "530010805080223",     2.0,    5.0,
    "530010805080209",     2.0,    5.0,
    "530010805080210",     2.0,    5.0,
    "530010805080220",     2.0,    5.0,
    "530010805080219",     2.0,    5.0,
    "530010805080217",     2.0,    5.0,
    "530010805080215",     2.0,    5.0,
    "530010805080216",     2.0,    5.0,
    "530010805080213",     2.0,    5.0,
    "530010805080243",     2.0,    5.0,
    "530010805080244",     2.0,    5.0,
    "5300108050802451",     2.0,    5.0,
    "530010805080246",     2.0,    5.0,
    "5300108050802471",     2.0,    5.0,
    "5300108050802501",     2.0,    5.0,
    "530010805080248",     2.0,    5.0,
    "5300108050802541",     2.0,    5.0,
    "5300108050802551",     2.0,    5.0,
    "5300108050802631",     2.0,    5.0,
    "530010805080264",     2.0,    5.0,
    "530010805080277",     2.0,    5.0,
    "530010805080278",     2.0,    5.0,
    "530010805080280",     2.0,    5.0,
    "530010805080657",     2.0,    5.0,
    "530010805080249",     2.0,    5.0,
    "5300108050802521",     2.0,    5.0,
    "530010805080251",     2.0,    5.0,
    "530010805080251",     2.0,    5.0,
    "530010805080258",     2.0,    5.0,
    "530010805080256",     2.0,    5.0,
    "530010805080257",     2.0,    5.0,
    "530010805080261",     2.0,    5.0,
    "530010805080260",     2.0,    5.0,
    "530010805080262",     2.0,    5.0,
    "5300108050802651",     2.0,    5.0,
    "530010805080276",     2.0,    5.0,
    "5300108050802661",     2.0,    5.0,
    "530010805080275",     2.0,    5.0,
    "530010805080274",     2.0,    5.0,
    "530010805080269",     2.0,    5.0,
    "530010805080273",     2.0,    5.0,
    "5300108050802721",     2.0,    5.0,
    "530010805080271",     2.0,    5.0,
    "5300108050802701",     2.0,    5.0,
    "5300108050802681",     0.8,    5.0,
    "530010805080267",     2.0,    5.0,
    "530010805080259",     2.0,    5.0,
    "530010805080279",     2.0,    5.0,
    "5300108050802531",     2.0,    5.0,
    "530010805080218",     2.0,    5.0,
    # -- TAG: SETOR NORTE --
    "530010805080281",     2.0,    2.5,
    "530010805080289",     2.0,    3.0,
    "530010805080290",     2.0,    3.0,
    "530010805080302",     2.0,    3.0,
    "5300108050803031",     2.0,    3.0,
    "530010805080304",     2.0,    5.0,
    "5300108050803011",     2.0,    5.0,
    "530010805080291",     2.0,    2.5,
    "530010805080292",     2.0,    5.0,
    "530010805080288",     2.0,    5.0,
    "530010805080282",     2.0,    5.0,
    "5300108050802831",     2.0,    5.0,
    "530010805080287",     2.0,    5.0,
    "5300108050802931",     2.0,    5.0,
    "5300108050803001",     2.0,    5.0,
    "530010805080298",     2.0,    5.0,
    "530010805080299",     2.0,    5.0,
    "530010805080284",     2.0,    5.0,
    "530010805080285",     2.0,    5.0,
    "530010805080286",     2.0,    5.0,
    "530010805080294",     2.0,    5.0,
    "530010805080295",     2.0,    2.0,
    "530010805080296",     2.0,    2.5,
    "530010805080297",     2.0,    5.0,
    "530010805080319",     2.0,    3.0,
    "530010805080317",     2.0,    2.5,
    "530010805080316",     2.0,    3.0,
    "530010805080314",     2.0,    2.5,
    "530010805080313",     2.0,    2.5,
    "530010805080305",     2.0,    5.0,
    "530010805080318",     2.0,    3.0,
    "530010805080315",     2.0,    3.0,
    "530010805080312",     2.0,    3.0,
    "530010805080311",     0.5,    5.0,
    "530010805080306",     2.0,    5.0,
    "530010805080307",     2.0,    5.0,
    "530010805080310",     2.0,    5.0,
    "530010805080309",     2.0,    5.0,
    "530010805080320",     2.0,    3.5,
    "530010805080321",     2.0,    3.0,
    "530010805080323",     2.0,    3.0,
    "530010805080330",     2.0,    5.0,
    "530010805080331",     2.0,    3.0,
    "530010805080322",     2.0,    2.5,
    "530010805080325",     2.0,    2.5,
    "530010805080326",     2.0,    2.5,
    "5300108050803241",     2.0,    3.0,
    "530010805080328",     2.0,    2.0,
    "530010805080329",     2.0,    2.0,
    "530010805080328",     2.0,    2.0,
    "530010805080336",     2.0,    2.5,
    "530010805080334",     2.0,    2.5,
    "530010805080327",     2.0,    2.0,
    "530010805080335",     2.0,    2.5, 
    "530010805080333",     2.0,    2.5,
    "5300108050803321",     2.0,    5.0,
    "530010805080338",     2.0,    5.0,
    "530010805080339",     2.0,    2.5,
    "530010805080337",     2.0,    3.0,
    "5300108050803081",     0.25,    3.0,
    "5300108050803491",     2.0,    3.0,
    "530010805080340",     2.0,    5.0,
    "5300108050803411",     2.0,    5.0,
    "530010805080342",     2.0,    5.0,
    "530010805080343",     2.0,    5.0,
    "530010805080344",     2.0,    5.0,
    "530010805080348",     2.0,    5.0,
    "530010805080347",     2.0,    5.0,
    "5300108050803461",     2.0,    3.0,
    "5300108050803451",     2.0,    3.0,
    "530010805080431",     2.0,    3.0,
    "530010805080721",     2.0,    3.0,
    "530010805080429",     2.0,    5.0,
    "5300108050804301",     2.0,    2.0,
    "530010805080434",     2.0,    2.0,
    "530010805080440",     2.0,    3.0,
    "530010805080441",     2.0,    3.0,
    "530010805080442",     2.0,    3.0,
    "530010805080432",     2.0,    3.0,
    "530010805080682",     2.0,    3.0,
    "530010805080433",     2.0,    2.0,
    "5300108050804431",     2.0,    2.0,
    "5300108050804461",     2.0,    3.0,
    "530010805080439",     2.0,    3.0,
    "530010805080438",     2.0,    2.0,
    "530010805080437",     2.0,    2.0,
    "530010805080436",     2.0,    2.0,
    "530010805080461",     2.0,    2.0,
    "530010805080459",     2.0,    3.0,
    "530010805080457",     2.0,    3.0,
    "5300108050804601",     2.0,    3.0,
    "530010805080458",     2.0,    3.0,
    "530010805080456",     2.0,    3.0,
    "530010805080455",     2.0,    2.0,
    "5300108050804521",     2.0,    2.0,
    "530010805080453",     2.0,    3.0,
    "5300108050804541",     2.0,    2.0,
    "530010805080451",     2.0,    3.0,
    "530010805080450",     2.0,    3.0,
    "5300108050804491",     2.0,    2.0,
    "530010805080533",     2.0,    2.0,
    "530010805080448",     2.0,    2.0,
    "530010805080447",     2.0,    3.0,
    "530010805080445",     2.0,    2.0,
    "530010805080444",     2.0,    3.0,
    "5300108050804351",     2.0,    3.0,
    "530010805080419",     2.0,    3.0,
    "530010805080420",     2.0,    3.0,
    "530010805080423",     2.0,    3.0,
    "530010805080424",     2.0,    2.0,
    "530010805080426",     2.0,    3.0,
    "530010805080428",     2.0,    3.0,
    "530010805080427",     2.0,    2.5,
    "530010805080405",     2.0,    2.5,
    "530010805080404",     2.0,    2.5,
    "5300108050803961",     2.0,    2.5,
    "5300108050803951",     2.0,    2.5,
    "5300108050803941",     2.0,    2.5,
    "530010805080392",     2.0,    2.5,
    "530010805080391",     2.0,    2.5,
    "530010805080370",     2.0,    2.5,
    "5300108050803691",     2.0,    2.5,
    "530010805080368",     2.0,    2.5,
    "530010805080367",     2.0,    2.5,
    "530010805080366",     2.0,    2.5,
    "530010805080365",     2.0,    2.5,
    "530010805080403",     2.0,    2.5,
    "530010805080393",     2.0,    2.5,
    "5300108050804251",     2.0,    3.0,
    "530010805080406",     2.0,    2.5,
    "530010805080402",     2.0,    2.5,
    "5300108050803971",     2.0,    2.5,
    "530010805080390",     2.0,    2.5,
    "530010805080371",     2.0,    2.5,
    "5300108050803641",     2.0,    4.0,
    "530010805080361",     2.0,    4.0,
    "530010805080362",     2.0,    3.0,
    "530010805080363",     2.0,    3.0,
    "530010805080372",     2.0,    3.0,
    "530010805080388",     2.0,    3.0,
    "530010805080389",     2.0,    2.5,
    "530010805080399",     2.0,    3.0,
    "530010805080400",     2.0,    3.0,
    "530010805080398",     2.0,    2.5,
    "530010805080401",     2.0,    3.0,
    "530010805080408",     2.0,    3.0,
    "530010805080409",     2.0,    3.0,
    "530010805080407",     2.0,    2.5,
    "530010805080416",     2.0,    2.0,
    "530010805080415",     2.0,    2.0,
    "530010805080414",     2.0,    2.0,
    "530010805080379",     2.0,    3.0,
    "530010805080378",     2.0,    2.0,
    "530010805080377",     2.0,    3.0,
    "530010805080356",     2.0,    2.0,
    "530010805080355",     2.0,    2.0,
    "530010805080354",     2.0,    3.0,
    "530010805080358",     2.0,    3.0,
    "5300108050803571",     2.0,    2.0,
    "530010805080376",     2.0,    3.0,
    "530010805080381",     2.0,    3.0,
    "530010805080382",     2.0,    3.0,
    "530010805080380",     2.0,    2.5,
    "530010805080413",     2.0,    2.5,
    "530010805080417",     2.0,    3.0,
    "530010805080418",     2.0,    2.5,
    "530010805080412",     2.0,    2.5,
    "530010805080410",     2.0,    2.5,
    "530010805080421",     2.0,    2.5,
    "530010805080422",     2.0,    3.0,
    "530010805080383",     2.0,    3.0,
    "530010805080384",     2.0,    2.0,
    "530010805080385",     2.0,    3.0,
    "5300108050803751",     2.0,    3.0,
    "530010805080359",     2.0,    3.0,
    "530010805080360",     2.0,    2.0,
    "530010805080374",     2.0,    2.0,
    "530010805080373",     2.0,    2.0,
    "530010805080386",     2.0,    2.0,
    "530010805080387",     2.0,    2.0,
    "530010805080411",     2.0,    3.0,
    
    # -- SANTA MARIA --
    "5300108050701421",     2.0,    4.0,
    "5300108052501151",     1.0,    3.6,
    # -- SANTA MARIA: SANTOS DUMONT --
    "530010805250101",     0.80,    0.84,
    "530010805250103",     0.80,    0.84
  )
  
  
  # Join com base no code_tract
  censo_sf_clean <- censo_sf_clean %>%
    left_join(coef_df, by = "code_tract")
  
  return(censo_sf_clean)
#  Coeficiente Básico (usar o menor):
#   O coeficiente básico define o direito de construir "gratuito", sem pagamento de outorga onerosa. Adotar o menor valor no setor censitário é uma forma conservadora e prudente de refletir a restrição mais severa à ocupação urbana imposta dentro daquele território. Isso evita superestimar o adensamento permitido em áreas com usos predominantemente residenciais de baixa densidade.
#  
#  Coeficiente Máximo (usar o maior):
#   O coeficiente máximo representa o limite absoluto de aproveitamento, que pode ser atingido mediante mecanismos como pagamento de outorga ou operações urbanas. Mesmo que só parte do setor permita usos mais intensivos (ex: comercial ou misto), o maior valor possível dentro do setor representa o potencial máximo de transformação urbana naquela célula espacial.
}

# ggplot(censo_final) +
#   geom_sf(aes(fill = cfa_m), color = "white", size = 0.1) +
#   scale_fill_viridis_c(option = "D", na.value = "grey90") +
#   labs(fill = "Coef. Máximo",
#        title = "Coeficiente de Aproveitamento Máximo (cfa_m)",
#        # subtitle = "RA 13 (Santa Maria) — Setores com valores atribuídos",
#        caption = "Cinza: setores sem valor" ) +
#   theme_minimal()




