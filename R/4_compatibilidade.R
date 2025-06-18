# Função para unir os setores de 2010 que formam um setor em 2000, remover os setores de 2010 que foram unidos, nomear o setor de 2010 de acordo com o setor original de 2000 que é renomeado para evitar duplicação com um possível setor de 2010 que tenha o mesmo nome de 2000, mas que não compõe os setores unidos.
unir_setores_temporais <- function(df, setores_2010, setor_2000, novo_codigo) {
  df %>%
    # Remover setores de 2010 que serão fundidos
    filter(!(ano == 2010 & code_tract %in% setores_2010)) %>%
    
    # Remover o setor de 2000 que será renomeado
    filter(!(ano == 2000 & code_tract == setor_2000)) %>%
    
    # Criar novo setor de 2010 com setores fundidos
    bind_rows(
      df %>%
        filter(ano == 2010 & code_tract %in% setores_2010) %>%
        mutate(geom = st_make_valid(geom)) %>%
        summarise(
          domicilios = sum(domicilios, na.rm = TRUE),
          renda_total = sum(renda_total, na.rm = TRUE),
          pop      = sum(pop, na.rm = TRUE),
          renda_percapita      = sum(renda_percapita, na.rm = TRUE),
          renda_positiva      = sum(renda_positiva, na.rm = TRUE),
          renda_media_positiva      = sum(renda_media_positiva, na.rm = TRUE),
          ens_superior      = sum(ens_superior, na.rm = TRUE),
          sem_renda      = sum(sem_renda, na.rm = TRUE),
          idade_65      = sum(idade_65, na.rm = TRUE),
          idade_70      = sum(idade_70, na.rm = TRUE),
          idade_75      = sum(idade_75, na.rm = TRUE),
          idade_80      = sum(idade_80, na.rm = TRUE),
          analfabetos      = sum(analfabetos, na.rm = TRUE),
          geom = st_union(geom),
          .groups = "drop"
        ) %>%
        mutate(
          code_tract = novo_codigo,
          ano = 2010
        ),
      
      # Renomear o setor de 2000
      df %>%
        filter(ano == 2000 & code_tract == setor_2000) %>%
        mutate(code_tract = novo_codigo)
    )
}

# Função para a criação da malha de 2010 compatibilizada
uniao_setores <- function(censo_sf_para_corrigir) {
# Unir setores de 2010 que formam o setor 530010805070149 de 2000 e substituí-los pelo novo setor
  censo_sf_corrigido <- censo_sf_para_corrigir
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070167", "530010805070224", "530010805070225"),
  setor_2000   = "530010805070149",
  novo_codigo  = "5300108050701491"
)

# Unir setores de 2010 que formam o setor 530010805070148 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070166", "530010805070213"),
  setor_2000   = "530010805070148",
  novo_codigo  = "5300108050701481"
)

# Unir setores de 2010 que formam o setor 530010805070146 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070211", "530010805070164"),
  setor_2000   = "530010805070146",
  novo_codigo  = "5300108050701461"
)

# Unir setores de 2010 que formam o setor 530010805070145 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070163", "530010805070210"),
  setor_2000   = "530010805070145",
  novo_codigo  = "5300108050701451"
)

# Unir setores de 2010 que formam o setor 530010805070147 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070165", "530010805070212", "530010805070172", "530010805070173"),
  setor_2000   = "530010805070147",
  novo_codigo  = "5300108050701471"
)

# Unir setores de 2010 que formam o setor 530010805200076 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200119", "530010805200179", "530010805200182", "530010805200120", "530010805200121", "530010805200122", "530010805200123", "530010805200126", "530010805200124", "530010805200125", "530010805200181"),
  setor_2000   = "530010805200076",
  novo_codigo  = "5300108052000761"
)

# Unir setores de 2010 que formam o setor 530010805180172 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180236", "530010805180235", "530010805180234", "530010805180313", "530010805180312", "530010805180238", "530010805180237", "530010805180314"),
  setor_2000   = "530010805180172",
  novo_codigo  = "5300108051801721"
)

# Unir setores de 2010 que formam o setor 530010805150389 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150511", "530010805150510", "530010805150470", "530010805150469", "530010805150457"),
  setor_2000   = "530010805150389",
  novo_codigo  = "5300108051503891"
)

# Unir setores de 2010 que formam o setor 530010805120059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120074", "530010805120083"),
  setor_2000   = "530010805120059",
  novo_codigo  = "5300108051200591"
)

# Unir setores de 2010 que formam o setor 530010805110166 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110193", "530010805110205"),
  setor_2000   = "530010805110166",
  novo_codigo  = "5300108051101661"
)

# Unir setores de 2010 que formam o setor 530010805110165 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110192", "530010805110293", "530010805110292"),
  setor_2000   = "530010805110165",
  novo_codigo  = "5300108051101651"
)

# Unir setores de 2010 que formam o setor 530010805110160-0160 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110187", "530010805110287"),
  setor_2000   = "530010805110160-0160",
  novo_codigo  = "530010805110160-01601"
)

# Unir setores de 2010 que formam o setor 530010805110162 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110189", "530010805110276"),
  setor_2000   = "530010805110162",
  novo_codigo  = "5300108051101621"
)

# Unir setores de 2010 que formam o setor 530010805110158 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110289", "530010805110185"),
  setor_2000   = "530010805110158",
  novo_codigo  = "5300108051101581"
)

# Unir setores de 2010 que formam o setor 530010805110184 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110184", "530010805110288", "530010805110204"),
  setor_2000   = "530010805110184",
  novo_codigo  = "5300108051101841"
)

# Unir setores de 2010 que formam o setor 530010805100152 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100232", "530010805100231", "530010805100228"),
  setor_2000   = "530010805100152",
  novo_codigo  = "5300108051001521"
)

# Unir setores de 2010 que formam o setor 530010805110185 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110291", "530010805110175"),
  setor_2000   = "530010805110185",
  novo_codigo  = "5300108051101851"
)

# Unir setores de 2010 que formam o setor 530010805110175 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110201", "530010805110283", "530010805110282"),
  setor_2000   = "530010805110175",
  novo_codigo  = "5300108051101751"
)

# Unir setores de 2010 que formam o setor 530010805110170 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110290", "530010805110197", "530010805110198", "530010805110285"),
  setor_2000   = "530010805110170",
  novo_codigo  = "5300108051101701"
)

# Unir setores de 2010 que formam o setor 530010805110172 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110199", "530010805110277", "530010805110279", "530010805110298", "530010805110278"),
  setor_2000   = "530010805110172",
  novo_codigo  = "5300108051101721"
)

# Unir setores de 2010 que formam o setor 530010805120058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120073", "530010805120082", "530010805120100"),
  setor_2000   = "530010805120058",
  novo_codigo  = "5300108051200581"
)

# Unir setores de 2010 que formam o setor 530010805120060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120075", "530010805120084"),
  setor_2000   = "530010805120060",
  novo_codigo  = "5300108051200601"
)

# Unir setores de 2010 que formam o setor 530010805120057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120072", "530010805120081", "530010805120096", "530010805120080"),
  setor_2000   = "530010805120057",
  novo_codigo  = "5300108051200571"
)

# Unir setores de 2010 que formam o setor 530010805120061 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120076", "530010805120085"),
  setor_2000   = "530010805120061",
  novo_codigo  = "5300108051200611"
)

# Unir setores de 2010 que formam o setor 530010805120056 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120098", "530010805120079", "530010805120097", "530010805120078", "530010805120071"),
  setor_2000   = "530010805120056",
  novo_codigo  = "5300108051200561"
)

###### Unir setores de 2010 que formam o setor 530010805110182 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110182", "530010805110203"),
  setor_2000   = "530010805110182",
  novo_codigo  = "5300108051101821"
)

# Unir setores de 2010 que formam o setor 530010805300060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300126", "530010805300089", "530010805300127", "530010805300092", "530010805300128"),
  setor_2000   = "530010805300060",
  novo_codigo  = "5300108053000601"
)

# Unir setores de 2010 que formam o setor 530010805250116 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250197", "530010805250196", "530010805250129"),
  setor_2000   = "530010805250116",
  novo_codigo  = "5300108052501161"
)

# Unir setores de 2010 que formam o setor 530010805250119 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250189", "530010805250191", "530010805250192", "530010805250190", "530010805250126"),
  setor_2000   = "530010805250119",
  novo_codigo  = "5300108052501191"
)

# Unir setores de 2010 que formam o setor 530010805300059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300088", "530010805300143", "530010805300152", "530010805300151", "530010805300091"),
  setor_2000   = "530010805300059",
  novo_codigo  = "5300108053000591"
)

# Unir setores de 2010 que formam o setor 530010805300058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300087"),
  setor_2000   = "530010805300058",
  novo_codigo  = "5300108053000581"
)

# Unir setores de 2010 que formam o setor 530010805300057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300086"),
  setor_2000   = "530010805300057",
  novo_codigo  = "5300108053000571"
)

# Unir setores de 2010 que formam o setor 530010805300054 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300122", "530010805300124", "530010805300085", "530010805300158", "530010805300084", "530010805300107", "530010805300106", "530010805300105", "530010805300104", "530010805300123", "530010805300113", "530010805300148", "530010805300114", "530010805300150", "530010805300115", "530010805300117", "530010805300118", "530010805300156", "530010805300116", "530010805300112", "530010805300110", "530010805300147", "530010805300111", "530010805300109", "530010805300108", "530010805300125", "530010805300149"),
  setor_2000   = "530010805300054",
  novo_codigo  = "5300108053000541"
)

# Unir setores de 2010 que formam o setor 530010805300047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300146", "530010805300145", "530010805300155", "530010805300154", "530010805300078", "530010805300079", "530010805300081", "530010805300119", "530010805300120", "530010805300082", "530010805300080", "530010805300101", "530010805300103", "530010805300102", "530010805300093", "530010805300153", "530010805300129", "530010805300131", "530010805300142", "530010805300083", "530010805300130", "530010805300132", "530010805300133", "530010805300138", "530010805300137", "530010805300141", "530010805300136", "530010805300140", "530010805300139", "530010805300134", "530010805300135", "530010805300121", "530010805300077", "530010805300100", "530010805300099", "530010805300144"),
  setor_2000   = "530010805300047",
  novo_codigo  = "5300108053000471"
)

# Unir setores de 2010 que formam o setor 530010805120048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120069", "530010805120087", "530010805120086", "530010805120095", "530010805120070", "530010805120066", "530010805120065"),
  setor_2000   = "530010805120048",
  novo_codigo  = "5300108051200481"
)

# Unir setores de 2010 que formam o setor 530010805120051 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120062", "530010805120090"),
  setor_2000   = "530010805120051",
  novo_codigo  = "5300108051200511"
)

# Unir setores de 2010 que formam o setor 530010805120052 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120091", "530010805120063"),
  setor_2000   = "530010805120052",
  novo_codigo  = "5300108051200521"
)

# Unir setores de 2010 que formam o setor 530010805120053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120093", "530010805120092", "530010805120064"),
  setor_2000   = "530010805120053",
  novo_codigo  = "5300108051200531"
)

# Unir setores de 2010 que formam o setor 530010805120049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120061", "530010805120089"),
  setor_2000   = "530010805120049",
  novo_codigo  = "5300108051200491"
)

# Unir setores de 2010 que formam o setor 530010805120047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120068", "530010805120101", "530010805120099"),
  setor_2000   = "530010805120047",
  novo_codigo  = "5300108051200471"
)

# Unir setores de 2010 que formam o setor 530010805120046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120067", "530010805120094", "530010805120102"),
  setor_2000   = "530010805120046",
  novo_codigo  = "5300108051200461"
)

# Unir setores de 2010 que formam o setor 530010805100121 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100121", "530010805100282"),
  setor_2000   = "530010805100121",
  novo_codigo  = "5300108051001211"
)

# Unir setores de 2010 que formam o setor 530010805100122 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100122", "530010805100283"),
  setor_2000   = "530010805100122",
  novo_codigo  = "5300108051001221"
)

# Unir setores de 2010 que formam o setor 530010805100120 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100120", "530010805100146"),
  setor_2000   = "530010805100120",
  novo_codigo  = "5300108051001201"
)

# Unir setores de 2010 que formam o setor 530010805100154 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100230", "530010805100325"),
  setor_2000   = "530010805100154",
  novo_codigo  = "5300108051001541"
)

# Unir setores de 2010 que formam o setor 530010805090055 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090084", "530010805090093", "530010805090094", "530010805090083", "530010805090095", "530010805090099", "530010805090098", "530010805090064", "530010805090097", "530010805090096"),
  setor_2000   = "530010805090055",
  novo_codigo  = "5300108050900551"
)

# Unir setores de 2010 que formam o setor 530010805090053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090076", "530010805090077", "530010805090065", "530010805090062", "530010805090075", "530010805090078", "530010805090080", "530010805090079", "530010805090081", "530010805090092", "530010805090067", "530010805090086", "530010805090087", "530010805090088", "530010805090068", "530010805090089", "530010805090069", "530010805090090"),
  setor_2000   = "530010805090053",
  novo_codigo  = "5300108050900531"
)

# Unir setores de 2010 que formam o setor 530010805090061 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090074", "530010805090061"),
  setor_2000   = "530010805090061",
  novo_codigo  = "5300108050900611"
)

# Unir setores de 2010 que formam o setor 530010805090060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090060", "530010805090073"),
  setor_2000   = "530010805090060",
  novo_codigo  = "5300108050900601"
)

# Unir setores de 2010 que formam o setor 530010805150391 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150468", "530010805150608", "530010805150607", "530010805150606", "530010805150467", "530010805150522", "530010805150459"),
  setor_2000   = "530010805150391",
  novo_codigo  = "5300108051503911"
)

# Unir setores de 2010 que formam o setor 530010805150391 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090082", "530010805090063", "530010805090100", "530010805090091", "530010805090052", "530010805090055", "530010805090053", "530010805090054"),
  setor_2000   = "530010805150391",
  novo_codigo  = "5300108051503911"
)

# Unir setores de 2010 que formam o setor 530010805090058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090085", "530010805090066", "530010805090072", "530010805090058", "530010805090101"),
  setor_2000   = "530010805090058",
  novo_codigo  = "5300108050900581"
)

# Unir setores de 2010 que formam o setor 530010805090057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805090071", "530010805090057"),
  setor_2000   = "530010805090057",
  novo_codigo  = "5300108050900571"
)

# Unir setores de 2010 que formam o setor 530010805250115 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250195", "530010805250128", "530010805250193", "530010805250194"),
  setor_2000   = "530010805250115",
  novo_codigo  = "5300108052501151"
)

# Unir setores de 2010 que formam o setor 530010805070143 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070207", "530010805070208", "530010805070217", "530010805070178", "530010805070219", "530010805070206", "530010805070161", "530010805070175", "530010805070215", "530010805070216"),
  setor_2000   = "530010805070143",
  novo_codigo  = "5300108050701431"
)

# Unir setores de 2010 que formam o setor 530010805070144 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070162", "530010805070209", "530010805070221", "530010805070220", "530010805070177", "530010805070176"),
  setor_2000   = "530010805070144",
  novo_codigo  = "5300108050701441"
)

# Unir setores de 2010 que formam o setor 530010805070156 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070174", "530010805070214", "530010805070179", "530010805070218"),
  setor_2000   = "530010805070156",
  novo_codigo  = "5300108050701561"
)

# Unir setores de 2010 que formam o setor 530010805200074 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200176", "530010805200118", "530010805200180", "530010805200178"),
  setor_2000   = "530010805200074",
  novo_codigo  = "5300108052000741"
)

# Unir setores de 2010 que formam o setor 530010805200075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200183", "530010805200075"),
  setor_2000   = "530010805200075",
  novo_codigo  = "5300108052000751"
)

# Unir setores de 2010 que formam o setor 530010805130056 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130124", "530010805130123", "530010805130056"),
  setor_2000   = "530010805130056",
  novo_codigo  = "5300108051300561"
)

# Unir setores de 2010 que formam o setor 530010805130057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130057", "530010805130125", "530010805130126"),
  setor_2000   = "530010805130057",
  novo_codigo  = "5300108051300571"
)

# Unir setores de 2010 que formam o setor 530010805130022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130022", "530010805130099", "530010805130107", "530010805130100", "530010805130101", "530010805130102", "530010805130103", "530010805130104", "530010805130108", "530010805130106", "530010805130105"),
  setor_2000   = "530010805130022",
  novo_codigo  = "5300108051300221"
)

# Unir setores de 2010 que formam o setor 530010805130021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130096", "530010805130095", "530010805130094", "530010805130093", "530010805130092", "530010805130091", "530010805130090", "530010805130089", "530010805130021", "530010805130086", "530010805130088", "530010805130087", "530010805130098", "530010805130097"),
  setor_2000   = "530010805130021",
  novo_codigo  = "5300108051300211"
)

# Unir setores de 2010 que formam o setor 530010805140028 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140028", "530010805140063", "530010805140064"),
  setor_2000   = "530010805140028",
  novo_codigo  = "5300108051400281"
)

# Unir setores de 2010 que formam o setor 530010805140029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140053", "530010805140073", "530010805140065", "530010805140029"),
  setor_2000   = "530010805140029",
  novo_codigo  = "5300108051400291"
)

# Unir setores de 2010 que formam o setor 530010805140030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140030", "530010805140066"),
  setor_2000   = "530010805140030",
  novo_codigo  = "5300108051400301"
)

# Unir setores de 2010 que formam o setor 530010805140034 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140034", "530010805140069"),
  setor_2000   = "530010805140034",
  novo_codigo  = "5300108051400341"
)

# Unir setores de 2010 que formam o setor 530010805140033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140033", "530010805140068"),
  setor_2000   = "530010805140033",
  novo_codigo  = "5300108051400331"
)

# Unir setores de 2010 que formam o setor 530010805140032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140032", "530010805140067"),
  setor_2000   = "530010805140032",
  novo_codigo  = "5300108051400321"
)

# Unir setores de 2010 que formam o setor 530010805230046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230060", "530010805230061", "530010805230046"),
  setor_2000   = "530010805230046",
  novo_codigo  = "5300108052300461"
)

# Unir setores de 2010 que formam o setor 530010805250120 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250199", "530010805250130", "530010805250127", "530010805250200", "530010805250201"),
  setor_2000   = "530010805250120",
  novo_codigo  = "5300108052501201"
)

# Unir setores de 2010 que formam o setor 530010805070151 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070169", "530010805070223", "530010805070222"),
  setor_2000   = "530010805070151",
  novo_codigo  = "5300108050701511"
)

# Unir setores de 2010 que formam o setor 530010805070151 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070202", "530010805070066", "530010805070145"),
  setor_2000   = "530010805070151",
  novo_codigo  = "5300108050701511"
)

# Unir setores de 2010 que formam o setor 530010805250113 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250177", "530010805250178", "530010805250179", "530010805250113", "530010805250172", "530010805250173", "530010805250174", "530010805250175", "530010805250176", "530010805250123", "530010805250180", "530010805250181", "530010805250182", "530010805250183", "530010805250184", "530010805250185", "530010805250186", "530010805250187", "530010805250188"),
  setor_2000   = "530010805250113",
  novo_codigo  = "5300108052501131"
)

# Unir setores de 2010 que formam o setor 530010805100135 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100207", "530010805100338", "530010805100337", "530010805100336", "530010805100335", "530010805100298", "530010805100206", "530010805100332", "530010805100333"),
  setor_2000   = "530010805100135",
  novo_codigo  = "5300108051001351"
)

# Unir setores de 2010 que formam o setor 530010805100139-0139 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100235", "530010805100339", "530010805100340", "530010805100308", "530010805100211"),
  setor_2000   = "530010805100139-0139",
  novo_codigo  = "530010805100139-01391"
)

# Unir setores de 2010 que formam o setor 530010805100138-0138 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100210"),
  setor_2000   = "530010805100138-0138",
  novo_codigo  = "530010805100138-01381"
)

# Unir setores de 2010 que formam o setor 530010805100137-0137 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100300", "530010805100209", "530010805100299"),
  setor_2000   = "530010805100137-0137",
  novo_codigo  = "530010805100137-01371"
)

# Unir setores de 2010 que formam o setor 530010805100136-0136 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100208", "530010805100341", "530010805100334"),
  setor_2000   = "530010805100136-0136",
  novo_codigo  = "530010805100136-01361"
)

# Unir setores de 2010 que formam o setor 530010805230029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230053", "530010805230029", "530010805230054", "530010805230052"),
  setor_2000   = "530010805230029",
  novo_codigo  = "5300108052300291"
)

# Unir setores de 2010 que formam o setor 530010805060286 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060424", "530010805060449", "530010805060286", "530010805060446"),
  setor_2000   = "530010805060286",
  novo_codigo  = "5300108050602861"
)

# Unir setores de 2010 que formam o setor 530010805060288 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060288"),
  setor_2000   = "530010805060288",
  novo_codigo  = "5300108050602881"
)

# Unir setores de 2010 que formam o setor 530010805060287 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060287"),
  setor_2000   = "530010805060287",
  novo_codigo  = "5300108050602871"
)

# Unir setores de 2010 que formam o setor 530010805210022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210070", "530010805210072", "530010805210058", "530010805210069", "530010805210022", "530010805210068", "530010805210071"),
  setor_2000   = "530010805210022",
  novo_codigo  = "5300108052100221"
)

# Unir setores de 2010 que formam o setor 530010805210023 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210059", "530010805210056", "530010805210057", "530010805210055", "530010805210050", "530010805210074", "530010805210073", "530010805210023", "530010805210048", "530010805210047", "530010805210045", "530010805210044", "530010805210043", "530010805210046", "530010805210054", "530010805210053", "530010805210052", "530010805210051", "530010805210049"),
  setor_2000   = "530010805210023",
  novo_codigo  = "5300108052100231"
)

# Unir setores de 2010 que formam o setor 530010805210025 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210025"),
  setor_2000   = "530010805210025",
  novo_codigo  = "5300108052100251"
)

# Unir setores de 2010 que formam o setor 530010805210026 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210026", "530010805210060"),
  setor_2000   = "530010805210026",
  novo_codigo  = "5300108052100261"
)

# Unir setores de 2010 que formam o setor 530010805210027 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210075", "530010805210027", "530010805210061"),
  setor_2000   = "530010805210027",
  novo_codigo  = "5300108052100271"
)

# Unir setores de 2010 que formam o setor 530010805210028 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210028", "530010805210035"),
  setor_2000   = "530010805210028",
  novo_codigo  = "5300108052100281"
)

# Unir setores de 2010 que formam o setor 530010805210029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210029"),
  setor_2000   = "530010805210029",
  novo_codigo  = "5300108052100291"
)

# Unir setores de 2010 que formam o setor 530010805210030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210036", "530010805210030"),
  setor_2000   = "530010805210030",
  novo_codigo  = "5300108052100301"
)

# Unir setores de 2010 que formam o setor 530010805100123 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100327", "530010805100123", "530010805100326", "530010805100159", "530010805100286", "530010805100160", "530010805100161", "530010805100162", "530010805100163", "530010805100164", "530010805100165", "530010805100166", "530010805100158", "530010805100157", "530010805100156", "530010805100155", "530010805100154", "530010805100153", "530010805100152", "530010805100147", "530010805100148", "530010805100150", "530010805100149", "530010805100149", "530010805100169", "530010805100287", "530010805100187", "530010805100192", "530010805100193", "530010805100194", "530010805100294", "530010805100295", "530010805100293", "530010805100190", "530010805100292", "530010805100189", "530010805100291", "530010805100188", "530010805100290", "530010805100167", "530010805100168", "530010805100186", "530010805100185", "530010805100184", "530010805100183", "530010805100191", "530010805100289", "530010805100174", "530010805100173", "530010805100172", "530010805100170", "530010805100171", "530010805100288", "530010805100178", "530010805100176", "530010805100175", "530010805100177", "530010805100182", "530010805100181", "530010805100180", "530010805100179", "530010805100195", "530010805100296", "530010805100328"),
  setor_2000   = "530010805100123",
  novo_codigo  = "5300108051001231"
)

# Unir setores de 2010 que formam o setor 530010805100118 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100118", "530010805100145", "530010805100285"),
  setor_2000   = "530010805100118",
  novo_codigo  = "5300108051001181"
)

# Unir setores de 2010 que formam o setor 530010805100133 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100197", "530010805100196"),
  setor_2000   = "530010805100133",
  novo_codigo  = "5300108051001331"
)

# Unir setores de 2010 que formam o setor 530010805100117 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100117", "530010805100144", "530010805100143", "530010805100142", "530010805100141", "530010805100140", "530010805100284", "530010805100139", "530010805100138", "530010805100137"),
  setor_2000   = "530010805100117",
  novo_codigo  = "5300108051001171"
)

# Unir setores de 2010 que formam o setor 530010805100106 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100272", "530010805100271", "530010805100270", "530010805100269", "530010805100106"),
  setor_2000   = "530010805100106",
  novo_codigo  = "5300108051001061"
)

# Unir setores de 2010 que formam o setor 530010805100105 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100105", "530010805100268"),
  setor_2000   = "530010805100105",
  novo_codigo  = "5300108051001051"
)

# Unir setores de 2010 que formam o setor 530010805100147-0147 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100219", "530010805100310", "530010805100309", "530010805100306", "530010805100227", "530010805100223", "530010805100222", "530010805100226", "530010805100305", "530010805100314", "530010805100221", "530010805100220", "530010805100304"),
  setor_2000   = "530010805100147-0147",
  novo_codigo  = "530010805100147-01471"
)

# Unir setores de 2010 que formam o setor 530010805100145 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100321", "530010805100217", "530010805100322", "530010805100323", "530010805100324"),
  setor_2000   = "530010805100145",
  novo_codigo  = "5300108051001451"
)

# Unir setores de 2010 que formam o setor 530010805100146 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100218", "530010805100303"),
  setor_2000   = "530010805100146",
  novo_codigo  = "5300108051001461"
)

# Unir setores de 2010 que formam o setor 530010805100144 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100302", "530010805100216", "530010805100215", "530010805100301", "530010805100234", "530010805100307"),
  setor_2000   = "530010805100144",
  novo_codigo  = "5300108051001441"
)

# Unir setores de 2010 que formam o setor 530010805100141 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100331", "530010805100213"),
  setor_2000   = "530010805100141",
  novo_codigo  = "5300108051001411"
)

# Unir setores de 2010 que formam o setor 530010805100132 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100203", "530010805100317"),
  setor_2000   = "530010805100132",
  novo_codigo  = "5300108051001321"
)

# Unir setores de 2010 que formam o setor 530010805100129 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100311", "530010805100312", "530010805100200"),
  setor_2000   = "530010805100129",
  novo_codigo  = "5300108051001291"
)

# Unir setores de 2010 que formam o setor 530010805100127 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100318", "530010805100198", "530010805100199"),
  setor_2000   = "530010805100127",
  novo_codigo  = "5300108051001271"
)

# Unir setores de 2010 que formam o setor 530010805100126 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100329", "530010805100205"),
  setor_2000   = "530010805100126",
  novo_codigo  = "5300108051001261"
)

# Unir setores de 2010 que formam o setor 530010805210021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210067", "530010805210021", "530010805210066"),
  setor_2000   = "530010805210021",
  novo_codigo  = "5300108052100211"
)

# Unir setores de 2010 que formam o setor 530010805230041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230041", "530010805230059", "530010805230058"),
  setor_2000   = "530010805230041",
  novo_codigo  = "5300108052300411"
)

# Unir setores de 2010 que formam o setor 530010805230040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230040", "530010805230057"),
  setor_2000   = "530010805230040",
  novo_codigo  = "5300108052300401"
)

# Unir setores de 2010 que formam o setor 530010805230035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230035", "530010805230056"),
  setor_2000   = "530010805230035",
  novo_codigo  = "5300108052300351"
)

# Unir setores de 2010 que formam o setor 530010805230026 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230026", "530010805230051", "530010805230027", "530010805230028"),
  setor_2000   = "530010805230026",
  novo_codigo  = "5300108052300261"
)

# Unir setores de 2010 que formam o setor 530010805230023 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230023", "530010805230050"),
  setor_2000   = "530010805230023",
  novo_codigo  = "5300108052300231"
)

# Unir setores de 2010 que formam o setor 530010805060269 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060445", "530010805060441", "530010805060442", "530010805060443", "530010805060444", "530010805060440", "530010805060439", "530010805060437", "530010805060438", "530010805060436", "530010805060269"),
  setor_2000   = "530010805060269",
  novo_codigo  = "5300108050602691"
)

# Unir setores de 2010 que formam o setor 530010805060267 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060267", "530010805060359"),
  setor_2000   = "530010805060267",
  novo_codigo  = "5300108050602671"
)

# Unir setores de 2010 que formam o setor 530010805060268 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060268", "530010805060423"),
  setor_2000   = "530010805060268",
  novo_codigo  = "5300108050602681"
)

# Unir setores de 2010 que formam o setor 530010805210010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210010", "530010805210032"),
  setor_2000   = "530010805210010",
  novo_codigo  = "5300108052100101"
)

# Unir setores de 2010 que formam o setor 530010805210011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210011", "530010805210038"),
  setor_2000   = "530010805210011",
  novo_codigo  = "5300108052100111"
)

# Unir setores de 2010 que formam o setor 530010805210009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210031", "530010805210009"),
  setor_2000   = "530010805210009",
  novo_codigo  = "5300108052100091"
)

# Unir setores de 2010 que formam o setor 530010805210013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210013", "530010805210039"),
  setor_2000   = "530010805210013",
  novo_codigo  = "5300108052100131"
)

# Unir setores de 2010 que formam o setor 530010805210015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210015", "530010805210040"),
  setor_2000   = "530010805210015",
  novo_codigo  = "5300108052100151"
)

# Unir setores de 2010 que formam o setor 530010805210017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210017", "530010805210033"),
  setor_2000   = "530010805210017",
  novo_codigo  = "5300108052100171"
)

# Unir setores de 2010 que formam o setor 530010805210019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210062", "530010805210063", "530010805210064", "530010805210065", "530010805210019", "530010805210034"),
  setor_2000   = "530010805210019",
  novo_codigo  = "5300108052100191"
)

# Unir setores de 2010 que formam o setor 530010805210002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805210002", "530010805210037"),
  setor_2000   = "530010805210002",
  novo_codigo  = "5300108052100021"
)

# Unir setores de 2010 que formam o setor 530010805060284 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060447", "530010805060284"),
  setor_2000   = "530010805060284",
  novo_codigo  = "5300108050602841"
)

# Unir setores de 2010 que formam o setor 530010805080242 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080656", "530010805080242"),
  setor_2000   = "530010805080242",
  novo_codigo  = "5300108050802421"
)

# Unir setores de 2010 que formam o setor 530010805160115 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160115", "530010805160217", "530010805160218"),
  setor_2000   = "530010805160115",
  novo_codigo  = "5300108051601151"
)

# Unir setores de 2010 que formam o setor 530010805160140 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160208", "530010805160140", "530010805160203", "530010805160204", "530010805160206", "530010805160207", "530010805160205"),
  setor_2000   = "530010805160140",
  novo_codigo  = "5300108051601401"
)

# Unir setores de 2010 que formam o setor 530010805160112 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160112", "530010805160173", "530010805160215", "530010805160174"),
  setor_2000   = "530010805160112",
  novo_codigo  = "5300108051601121"
)

# Unir setores de 2010 que formam o setor 530010805160113 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160113", "530010805160220", "530010805160194", "530010805160193", "530010805160195", "530010805160192"),
  setor_2000   = "530010805160113",
  novo_codigo  = "5300108051601131"
)

# Unir setores de 2010 que formam o setor 530010805080239 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080727", "530010805080515", "530010805080516", "530010805080726", "530010805080239", "530010805080654", "530010805080514", "530010805080702"),
  setor_2000   = "530010805080239",
  novo_codigo  = "5300108050802391"
)

# Unir setores de 2010 que formam o setor 530010805080486 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080707", "530010805080694", "530010805080486", "530010805080708", "530010805080709", "530010805080547"),
  setor_2000   = "530010805080486",
  novo_codigo  = "5300108050804861"
)

# Unir setores de 2010 que formam o setor 530010805080238 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080725", "530010805080724", "530010805080513", "530010805080653", "530010805080238"),
  setor_2000   = "530010805080238",
  novo_codigo  = "5300108050802381"
)

# Unir setores de 2010 que formam o setor 530010805080237 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080511", "530010805080722", "530010805080237", "530010805080512", "530010805080652", "530010805080723"),
  setor_2000   = "530010805080237",
  novo_codigo  = "5300108050802371"
)

# Unir setores de 2010 que formam o setor 530010805080485 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080543", "530010805080544", "530010805080706", "530010805080729", "530010805080485", "530010805080545", "530010805080546"),
  setor_2000   = "530010805080485",
  novo_codigo  = "5300108050804851"
)

# Unir setores de 2010 que formam o setor 530010805080487 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080549", "530010805080712", "530010805080695", "530010805080548", "530010805080711", "530010805080710", "530010805080487"),
  setor_2000   = "530010805080487",
  novo_codigo  = "5300108050804871"
)

# Unir setores de 2010 que formam o setor 530010805080488 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080488", "530010805080550", "530010805080551"),
  setor_2000   = "530010805080488",
  novo_codigo  = "5300108050804881"
)

# Unir setores de 2010 que formam o setor 530010805080489 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080553", "530010805080730", "530010805080552", "530010805080696", "530010805080489"),
  setor_2000   = "530010805080489",
  novo_codigo  = "5300108050804891"
)

# Unir setores de 2010 que formam o setor 530010805080241 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080241", "530010805080655", "530010805080518", "530010805080703"),
  setor_2000   = "530010805080241",
  novo_codigo  = "5300108050802411"
)

# Unir setores de 2010 que formam o setor 530010805080235 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080509", "530010805080508", "530010805080507", "530010805080505", "530010805080506", "530010805080651", "530010805080235"),
  setor_2000   = "530010805080235",
  novo_codigo  = "5300108050802351"
)

# Unir setores de 2010 que formam o setor 530010805080481 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080481", "530010805080542"),
  setor_2000   = "530010805080481",
  novo_codigo  = "5300108050804811"
)

# Unir setores de 2010 que formam o setor 530010805080478 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080478", "530010805080539", "530010805080693"),
  setor_2000   = "530010805080478",
  novo_codigo  = "5300108050804781"
)

# Unir setores de 2010 que formam o setor 530010805080234 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080234", "530010805080504", "530010805080501", "530010805080502"),
  setor_2000   = "530010805080234",
  novo_codigo  = "5300108050802341"
)

# Unir setores de 2010 que formam o setor 530010805080479 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080540", "530010805080479", "530010805080503", "530010805080739"),
  setor_2000   = "530010805080479",
  novo_codigo  = "5300108050804791"
)

# Unir setores de 2010 que formam o setor 530010805080480 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080541", "530010805080480"),
  setor_2000   = "530010805080480",
  novo_codigo  = "5300108050804801"
)

# Unir setores de 2010 que formam o setor 530010805080236 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080718", "530010805080236", "530010805080510", "530010805080701"),
  setor_2000   = "530010805080236",
  novo_codigo  = "5300108050802361"
)

# Unir setores de 2010 que formam o setor 530010805080483 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080492", "530010805080483"),
  setor_2000   = "530010805080483",
  novo_codigo  = "5300108050804831"
)

# Unir setores de 2010 que formam o setor 530010805080484 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080735", "530010805080484", "530010805080738", "530010805080737", "530010805080736"),
  setor_2000   = "530010805080484",
  novo_codigo  = "5300108050804841"
)

# Unir setores de 2010 que formam o setor 530010805140041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140041", "530010805140071"),
  setor_2000   = "530010805140041",
  novo_codigo  = "5300108051400411"
)

# Unir setores de 2010 que formam o setor 530010805080176 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080635", "530010805080176"),
  setor_2000   = "530010805080176",
  novo_codigo  = "5300108050801761"
)

# Unir setores de 2010 que formam o setor 530010805140042 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140042", "530010805140072"),
  setor_2000   = "530010805140042",
  novo_codigo  = "5300108051400421"
)

# Unir setores de 2010 que formam o setor 530010805080033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080567", "530010805080566", "530010805080033"),
  setor_2000   = "530010805080033",
  novo_codigo  = "5300108050800331"
)

# Unir setores de 2010 que formam o setor 530010805080032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080032", "530010805080564", "530010805080565"),
  setor_2000   = "530010805080032",
  novo_codigo  = "5300108050800321"
)

# Unir setores de 2010 que formam o setor 530010805080034 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080034", "530010805080568", "530010805080569"),
  setor_2000   = "530010805080034",
  novo_codigo  = "5300108050800341"
)

# Unir setores de 2010 que formam o setor 530010805080035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080035", "530010805080570"),
  setor_2000   = "530010805080035",
  novo_codigo  = "5300108050800351"
)

# Unir setores de 2010 que formam o setor 530010805080036 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080572", "530010805080571", "530010805080036"),
  setor_2000   = "530010805080036",
  novo_codigo  = "5300108050800361"
)

# Unir setores de 2010 que formam o setor 530010805080037 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080573", "530010805080037"),
  setor_2000   = "530010805080037",
  novo_codigo  = "5300108050800371"
)

# Unir setores de 2010 que formam o setor 530010805080038 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080495", "530010805080699", "530010805080574", "530010805080038"),
  setor_2000   = "530010805080038",
  novo_codigo  = "5300108050800381"
)

# Unir setores de 2010 que formam o setor 530010805080040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080040", "530010805080577", "530010805080576", "530010805080578"),
  setor_2000   = "530010805080040",
  novo_codigo  = "5300108050800401"
)

# Unir setores de 2010 que formam o setor 530010805080041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080041", "530010805080497"),
  setor_2000   = "530010805080041",
  novo_codigo  = "5300108050800411"
)

# Unir setores de 2010 que formam o setor 530010805080042 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080042", "530010805080579"),
  setor_2000   = "530010805080042",
  novo_codigo  = "5300108050800421"
)

# Unir setores de 2010 que formam o setor 530010805140019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140019", "530010805140056", "530010805140057"),
  setor_2000   = "530010805140019",
  novo_codigo  = "5300108051400191"
)

# Unir setores de 2010 que formam o setor 530010805080491 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080734", "530010805080733", "530010805080732", "530010805080731"),
  setor_2000   = "530010805080491",
  novo_codigo  = "5300108050804911"
)

# Unir setores de 2010 que formam o setor 530010805080476 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080491", "530010805080476"),
  setor_2000   = "530010805080476",
  novo_codigo  = "5300108050804761"
)

# Unir setores de 2010 que formam o setor 530010805080490 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080490", "530010805080697"),
  setor_2000   = "530010805080490",
  novo_codigo  = "5300108050804901"
)

# Unir setores de 2010 que formam o setor 530010805080039 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080039", "530010805080575", "530010805080496", "530010805080700"),
  setor_2000   = "530010805080039",
  novo_codigo  = "5300108050800391"
)

# Unir setores de 2010 que formam o setor 530010805080013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080559", "530010805080560", "530010805080558", "530010805080013"),
  setor_2000   = "530010805080013",
  novo_codigo  = "5300108050800131"
)

# Unir setores de 2010 que formam o setor 530010805080030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080563", "530010805080030"),
  setor_2000   = "530010805080030",
  novo_codigo  = "5300108050800301"
)

# Unir setores de 2010 que formam o setor 530010805110145 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110268", "530010805110145"),
  setor_2000   = "530010805110145",
  novo_codigo  = "5300108051101451"
)

# Unir setores de 2010 que formam o setor 530010805110144 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110267", "530010805110144"),
  setor_2000   = "530010805110144",
  novo_codigo  = "5300108051101441"
)

# Unir setores de 2010 que formam o setor 530010805110157 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110273", "530010805110157", "530010805110272"),
  setor_2000   = "530010805110157",
  novo_codigo  = "5300108051101571"
)

# Unir setores de 2010 que formam o setor 530010805110146 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110146", "530010805110269"),
  setor_2000   = "530010805110146",
  novo_codigo  = "5300108051101461"
)

# Unir setores de 2010 que formam o setor 530010805110142 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110266", "530010805110142", "530010805110264", "530010805110265"),
  setor_2000   = "530010805110142",
  novo_codigo  = "5300108051101421"
)

# Unir setores de 2010 que formam o setor 530010805110174 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110200", "530010805110281", "530010805110280"),
  setor_2000   = "530010805110174",
  novo_codigo  = "5300108051101741"
)

# Unir setores de 2010 que formam o setor 530010805110112 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110112", "530010805110245", "530010805110246"),
  setor_2000   = "530010805110112",
  novo_codigo  = "5300108051101121"
)

# Unir setores de 2010 que formam o setor 530010805110127 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110127", "530010805110255"),
  setor_2000   = "530010805110127",
  novo_codigo  = "5300108051101271"
)

# Unir setores de 2010 que formam o setor 530010805110128 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110256", "530010805110128"),
  setor_2000   = "530010805110128",
  novo_codigo  = "5300108051101281"
)

# Unir setores de 2010 que formam o setor 530010805110130 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110130", "530010805110258"),
  setor_2000   = "530010805110130",
  novo_codigo  = "5300108051101301"
)

# Unir setores de 2010 que formam o setor 530010805110131 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110131", "530010805110259"),
  setor_2000   = "530010805110131",
  novo_codigo  = "5300108051101311"
)

# Unir setores de 2010 que formam o setor 530010805110101 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110101", "530010805110242"),
  setor_2000   = "530010805110101",
  novo_codigo  = "5300108051101011"
)

# Unir setores de 2010 que formam o setor 530010805110104 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110104", "530010805110244"),
  setor_2000   = "530010805110104",
  novo_codigo  = "5300108051101041"
)

# Unir setores de 2010 que formam o setor 530010805110100 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110241", "530010805110100"),
  setor_2000   = "530010805110100",
  novo_codigo  = "5300108051101001"
)

# Unir setores de 2010 que formam o setor 530010805110099 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110239", "530010805110240", "530010805110099"),
  setor_2000   = "530010805110099",
  novo_codigo  = "5300108051100991"
)

# Unir setores de 2010 que formam o setor 530010805110084 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110084", "530010805110234"),
  setor_2000   = "530010805110084",
  novo_codigo  = "5300108051100841"
)

# Unir setores de 2010 que formam o setor 530010805110081 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110231", "530010805110081", "530010805110232"),
  setor_2000   = "530010805110081",
  novo_codigo  = "5300108051100811"
)

# Unir setores de 2010 que formam o setor 530010805110001 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110001", "530010805110206", "530010805110207"),
  setor_2000   = "530010805110001",
  novo_codigo  = "5300108051100011"
)

# Unir setores de 2010 que formam o setor 530010805110019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110161", "530010805110019"),
  setor_2000   = "530010805110019",
  novo_codigo  = "5300108051100191"
)

# Unir setores de 2010 que formam o setor 530010805070139 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070157", "530010805070139"),
  setor_2000   = "530010805070139",
  novo_codigo  = "5300108050701391"
)

# Unir setores de 2010 que formam o setor 530010805070137 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070199", "530010805070137"),
  setor_2000   = "530010805070137",
  novo_codigo  = "5300108050701371"
)

# Unir setores de 2010 que formam o setor 530010805070136 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070156", "530010805070136"),
  setor_2000   = "530010805070136",
  novo_codigo  = "5300108050701361"
)

# Unir setores de 2010 que formam o setor 530010805070130 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070155", "530010805070130"),
  setor_2000   = "530010805070130",
  novo_codigo  = "5300108050701301"
)

# Unir setores de 2010 que formam o setor 530010805070127 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070127", "530010805070198"),
  setor_2000   = "530010805070127",
  novo_codigo  = "5300108050701271"
)

# Unir setores de 2010 que formam o setor 530010805070126 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070154", "530010805070126"),
  setor_2000   = "530010805070126",
  novo_codigo  = "5300108050701261"
)

# Unir setores de 2010 que formam o setor 530010805070121 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070153", "530010805070121"),
  setor_2000   = "530010805070121",
  novo_codigo  = "5300108050701211"
)

# Unir setores de 2010 que formam o setor 530010805070119 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070152", "530010805070119"),
  setor_2000   = "530010805070119",
  novo_codigo  = "5300108050701191"
)

# Unir setores de 2010 que formam o setor 530010805070118 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070118", "530010805070151"),
  setor_2000   = "530010805070118",
  novo_codigo  = "5300108050701181"
)

# Unir setores de 2010 que formam o setor 530010805070110 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070110", "530010805070226"),
  setor_2000   = "530010805070110",
  novo_codigo  = "5300108050701101"
)

# Unir setores de 2010 que formam o setor 530010805070109 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070109", "530010805070150", "530010805070204"),
  setor_2000   = "530010805070109",
  novo_codigo  = "5300108050701091"
)

# Unir setores de 2010 que formam o setor 530010805070098 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070098", "530010805070196"),
  setor_2000   = "530010805070098",
  novo_codigo  = "5300108050700981"
)

# Unir setores de 2010 que formam o setor 530010805070097 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070097", "530010805070195"),
  setor_2000   = "530010805070097",
  novo_codigo  = "5300108050700971"
)

# Unir setores de 2010 que formam o setor 530010805070088 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070088", "530010805070193"),
  setor_2000   = "530010805070088",
  novo_codigo  = "5300108050700881"
)

# Unir setores de 2010 que formam o setor 530010805070087 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070087", "530010805070192"),
  setor_2000   = "530010805070087",
  novo_codigo  = "5300108050700871"
)

# Unir setores de 2010 que formam o setor 530010805070087 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070087", "530010805070192"),
  setor_2000   = "530010805070087",
  novo_codigo  = "5300108050700871"
)

# Unir setores de 2010 que formam o setor 530010805070081 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070081", "530010805070191"),
  setor_2000   = "530010805070081",
  novo_codigo  = "5300108050700811"
)

# Unir setores de 2010 que formam o setor 530010805070080 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070080", "530010805070190"),
  setor_2000   = "530010805070080",
  novo_codigo  = "5300108050700801"
)

# Unir setores de 2010 que formam o setor 530010805070079 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070079", "530010805070189"),
  setor_2000   = "530010805070079",
  novo_codigo  = "5300108050700791"
)

# Unir setores de 2010 que formam o setor 530010805070071 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070071", "530010805070146", "530010805070203"),
  setor_2000   = "530010805070071",
  novo_codigo  = "5300108050700711"
)

# Unir setores de 2010 que formam o setor 530010805070070 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070070", "530010805070188"),
  setor_2000   = "530010805070070",
  novo_codigo  = "5300108050700701"
)

# Unir setores de 2010 que formam o setor 530010805070077 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070077", "530010805070147"),
  setor_2000   = "530010805070077",
  novo_codigo  = "5300108050700771"
)

# Unir setores de 2010 que formam o setor 530010805070063 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070063", "530010805070143", "530010805070200", "530010805070186", "530010805070201"),
  setor_2000   = "530010805070063",
  novo_codigo  = "5300108050700631"
)

# Unir setores de 2010 que formam o setor 530010805070064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070064", "530010805070144"),
  setor_2000   = "530010805070064",
  novo_codigo  = "5300108050700641"
)

# Unir setores de 2010 que formam o setor 530010805070058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070058", "530010805070184"),
  setor_2000   = "530010805070058",
  novo_codigo  = "5300108050700581"
)

# Unir setores de 2010 que formam o setor 530010805070059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070185", "530010805070059"),
  setor_2000   = "530010805070059",
  novo_codigo  = "5300108050700591"
)

# Unir setores de 2010 que formam o setor 530010805070060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070158", "530010805070060"),
  setor_2000   = "530010805070060",
  novo_codigo  = "5300108050700601"
)

# Unir setores de 2010 que formam o setor 530010805070045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070142", "530010805070045"),
  setor_2000   = "530010805070045",
  novo_codigo  = "5300108050700451"
)

# Unir setores de 2010 que formam o setor 530010805070004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070004", "530010805070181"),
  setor_2000   = "530010805070004",
  novo_codigo  = "5300108050700041"
)

# Unir setores de 2010 que formam o setor 530010805070007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070007", "530010805070182"),
  setor_2000   = "530010805070007",
  novo_codigo  = "5300108050700071"
)

# Unir setores de 2010 que formam o setor 530010805070003 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070003", "530010805070180"),
  setor_2000   = "530010805070003",
  novo_codigo  = "5300108050700031"
)

# Unir setores de 2010 que formam o setor 530010805070142 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070159", "530010805070160", "530010805070205"),
  setor_2000   = "530010805070142",
  novo_codigo  = "5300108050701421"
)

# Unir setores de 2010 que formam o setor 530010805250005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250005", "530010805250131"),
  setor_2000   = "530010805250005",
  novo_codigo  = "5300108052500051"
)

# Unir setores de 2010 que formam o setor 530010805250016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250016", "530010805250140"),
  setor_2000   = "530010805250016",
  novo_codigo  = "5300108052500161"
)

# Unir setores de 2010 que formam o setor 530010805250012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250012", "530010805250136"),
  setor_2000   = "530010805250012",
  novo_codigo  = "5300108052500121"
)

# Unir setores de 2010 que formam o setor 530010805250013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250013", "530010805250137"),
  setor_2000   = "530010805250013",
  novo_codigo  = "5300108052500131"
)

# Unir setores de 2010 que formam o setor 530010805250014 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250014", "530010805250138"),
  setor_2000   = "530010805250014",
  novo_codigo  = "5300108052500141"
)

# Unir setores de 2010 que formam o setor 530010805250010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250010", "530010805250135"),
  setor_2000   = "530010805250010",
  novo_codigo  = "5300108052500101"
)

# Unir setores de 2010 que formam o setor 530010805250010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250010", "530010805250135"),
  setor_2000   = "530010805250010",
  novo_codigo  = "5300108052500101"
)

# Unir setores de 2010 que formam o setor 530010805250007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250007", "530010805250132"),
  setor_2000   = "530010805250007",
  novo_codigo  = "5300108052500071"
)

# Unir setores de 2010 que formam o setor 530010805250008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250008", "530010805250133"),
  setor_2000   = "530010805250008",
  novo_codigo  = "5300108052500081"
)

# Unir setores de 2010 que formam o setor 530010805250021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250021", "530010805250143"),
  setor_2000   = "530010805250021",
  novo_codigo  = "5300108052500211"
)

# Unir setores de 2010 que formam o setor 530010805250026 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250026", "530010805250146"),
  setor_2000   = "530010805250026",
  novo_codigo  = "5300108052500261"
)

# Unir setores de 2010 que formam o setor 530010805250025 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250145", "530010805250025"),
  setor_2000   = "530010805250025",
  novo_codigo  = "5300108052500251"
)

# Unir setores de 2010 que formam o setor 530010805250035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250035", "530010805250147"),
  setor_2000   = "530010805250035",
  novo_codigo  = "5300108052500351"
)

# Unir setores de 2010 que formam o setor 530010805250054 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250054", "530010805250156", "530010805250157", "530010805250158", "530010805250159", "530010805250160"),
  setor_2000   = "530010805250054",
  novo_codigo  = "5300108052500541"
)

# Unir setores de 2010 que formam o setor 530010805250052 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250052", "530010805250114"),
  setor_2000   = "530010805250052",
  novo_codigo  = "5300108052500521"
)

# Unir setores de 2010 que formam o setor 530010805250057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250057", "530010805250161", "530010805250162"),
  setor_2000   = "530010805250057",
  novo_codigo  = "5300108052500571"
)

# Unir setores de 2010 que formam o setor 530010805250058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250058", "530010805250163"),
  setor_2000   = "530010805250058",
  novo_codigo  = "5300108052500581"
)

# Unir setores de 2010 que formam o setor 530010805250065 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250065", "530010805250115"),
  setor_2000   = "530010805250065",
  novo_codigo  = "5300108052500651"
)

# Unir setores de 2010 que formam o setor 530010805250064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250064", "530010805250164"),
  setor_2000   = "530010805250064",
  novo_codigo  = "5300108052500641"
)

# Unir setores de 2010 que formam o setor 530010805250073 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250117", "530010805250073"),
  setor_2000   = "530010805250073",
  novo_codigo  = "5300108052500731"
)

# Unir setores de 2010 que formam o setor 530010805250072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250072", "530010805250116"),
  setor_2000   = "530010805250072",
  novo_codigo  = "5300108052500721"
)

# Unir setores de 2010 que formam o setor 530010805250085 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250085", "530010805250119"),
  setor_2000   = "530010805250085",
  novo_codigo  = "5300108052500851"
)

# Unir setores de 2010 que formam o setor 530010805250091 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250091", "530010805250120"),
  setor_2000   = "530010805250091",
  novo_codigo  = "5300108052500911"
)

# Unir setores de 2010 que formam o setor 530010805250092 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250092", "530010805250198"),
  setor_2000   = "530010805250092",
  novo_codigo  = "5300108052500921"
)

# Unir setores de 2010 que formam o setor 530010805250094 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250094", "530010805250121"),
  setor_2000   = "530010805250094",
  novo_codigo  = "5300108052500941"
)

# Unir setores de 2010 que formam o setor 530010805250095 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250095", "530010805250122"),
  setor_2000   = "530010805250095",
  novo_codigo  = "5300108052500951"
)

# Unir setores de 2010 que formam o setor 530010805250084 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250084", "530010805250118"),
  setor_2000   = "530010805250084",
  novo_codigo  = "5300108052500841"
)

# Unir setores de 2010 que formam o setor 530010805250082 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250082", "530010805250169"),
  setor_2000   = "530010805250082",
  novo_codigo  = "5300108052500821"
)

# Unir setores de 2010 que formam o setor 530010805250097 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250097", "530010805250170"),
  setor_2000   = "530010805250097",
  novo_codigo  = "5300108052500971"
)

# Unir setores de 2010 que formam o setor 530010805250098 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250098", "530010805250171"),
  setor_2000   = "530010805250098",
  novo_codigo  = "5300108052500981"
)

# Unir setores de 2010 que formam o setor 530010805250079 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250168", "530010805250079"),
  setor_2000   = "530010805250079",
  novo_codigo  = "5300108052500791"
)

# Unir setores de 2010 que formam o setor 530010805250075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250166", "530010805250075"),
  setor_2000   = "530010805250075",
  novo_codigo  = "5300108052500751"
)

# Unir setores de 2010 que formam o setor 530010805250076 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250167", "530010805250076"),
  setor_2000   = "530010805250076",
  novo_codigo  = "5300108052500761"
)

# Unir setores de 2010 que formam o setor 530010805250069 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250069", "530010805250165"),
  setor_2000   = "530010805250069",
  novo_codigo  = "5300108052500691"
)

# Unir setores de 2010 que formam o setor 530010805250049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250049", "530010805250155"),
  setor_2000   = "530010805250049",
  novo_codigo  = "5300108052500491"
)

# Unir setores de 2010 que formam o setor 530010805250048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250048", "530010805250154"),
  setor_2000   = "530010805250048",
  novo_codigo  = "5300108052500481"
)

# Unir setores de 2010 que formam o setor 530010805250047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250047", "530010805250153"),
  setor_2000   = "530010805250047",
  novo_codigo  = "5300108052500471"
)

# Unir setores de 2010 que formam o setor 530010805250043 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250043", "530010805250149"),
  setor_2000   = "530010805250043",
  novo_codigo  = "5300108052500431"
)

# Unir setores de 2010 que formam o setor 530010805250046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250046", "530010805250152"),
  setor_2000   = "530010805250046",
  novo_codigo  = "5300108052500461"
)

# Unir setores de 2010 que formam o setor 530010805250045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250045", "530010805250151"),
  setor_2000   = "530010805250045",
  novo_codigo  = "5300108052500451"
)

# Unir setores de 2010 que formam o setor 530010805250044 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250044", "530010805250150"),
  setor_2000   = "530010805250044",
  novo_codigo  = "5300108052500441"
)

# Unir setores de 2010 que formam o setor 530010805250040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250040", "530010805250148"),
  setor_2000   = "530010805250040",
  novo_codigo  = "5300108052500401"
)

# Unir setores de 2010 que formam o setor 530010805250022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250022", "530010805250144"),
  setor_2000   = "530010805250022",
  novo_codigo  = "5300108052500221"
)

# Unir setores de 2010 que formam o setor 530010805250020 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250020", "530010805250142"),
  setor_2000   = "530010805250020",
  novo_codigo  = "5300108052500201"
)

# Unir setores de 2010 que formam o setor 530010805250019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250019", "530010805250141"),
  setor_2000   = "530010805250019",
  novo_codigo  = "5300108052500191"
)

# Unir setores de 2010 que formam o setor 530010805250015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250015", "530010805250139"),
  setor_2000   = "530010805250015",
  novo_codigo  = "5300108052500151"
)

# Unir setores de 2010 que formam o setor 530010805250009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805250009", "530010805250134"),
  setor_2000   = "530010805250009",
  novo_codigo  = "5300108052500091"
)

# Unir setores de 2010 que formam o setor 530010805300040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300070", "530010805300040"),
  setor_2000   = "530010805300040",
  novo_codigo  = "5300108053000401"
)

# Unir setores de 2010 que formam o setor 530010805300042 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300071", "530010805300072", "530010805300042"),
  setor_2000   = "530010805300042",
  novo_codigo  = "5300108053000421"
)

# Unir setores de 2010 que formam o setor 530010805300043 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300074", "530010805300073", "530010805300043"),
  setor_2000   = "530010805300043",
  novo_codigo  = "5300108053000431"
)

# Unir setores de 2010 que formam o setor 530010805300044 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300044", "530010805300097"),
  setor_2000   = "530010805300044",
  novo_codigo  = "5300108053000441"
)

# Unir setores de 2010 que formam o setor 530010805300045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300075", "530010805300045"),
  setor_2000   = "530010805300045",
  novo_codigo  = "5300108053000451"
)

# Unir setores de 2010 que formam o setor 530010805300036 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300036", "530010805300068", "530010805300098"),
  setor_2000   = "530010805300036",
  novo_codigo  = "5300108053000361"
)

# Unir setores de 2010 que formam o setor 530010805300035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300035", "530010805300067"),
  setor_2000   = "530010805300035",
  novo_codigo  = "5300108053000351"
)

# Unir setores de 2010 que formam o setor 530010805300034 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300034", "530010805300066"),
  setor_2000   = "530010805300034",
  novo_codigo  = "5300108053000341"
)

# Unir setores de 2010 que formam o setor 530010805300033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300065", "530010805300033"),
  setor_2000   = "530010805300033",
  novo_codigo  = "5300108053000331"
)

# Unir setores de 2010 que formam o setor 530010805300032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300064", "530010805300032"),
  setor_2000   = "530010805300032",
  novo_codigo  = "5300108053000321"
)

# Unir setores de 2010 que formam o setor 530010805300031 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300031", "530010805300096", "530010805300063"),
  setor_2000   = "530010805300031",
  novo_codigo  = "5300108053000311"
)

# Unir setores de 2010 que formam o setor 530010805300030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300030", "530010805300062"),
  setor_2000   = "530010805300030",
  novo_codigo  = "5300108053000301"
)

# Unir setores de 2010 que formam o setor 530010805300029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300029", "530010805300061"),
  setor_2000   = "530010805300029",
  novo_codigo  = "5300108053000291"
)

# Unir setores de 2010 que formam o setor 530010805300023 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300023", "530010805300059"),
  setor_2000   = "530010805300023",
  novo_codigo  = "5300108053000231"
)

# Unir setores de 2010 que formam o setor 530010805300026 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300060", "530010805300026"),
  setor_2000   = "530010805300026",
  novo_codigo  = "5300108053000261"
)

# Unir setores de 2010 que formam o setor 530010805300010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300010", "530010805300054"),
  setor_2000   = "530010805300010",
  novo_codigo  = "5300108053000101"
)

# Unir setores de 2010 que formam o setor 530010805300008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300094", "530010805300008"),
  setor_2000   = "530010805300008",
  novo_codigo  = "5300108053000081"
)

# Unir setores de 2010 que formam o setor 530010805300007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300007", "530010805300053"),
  setor_2000   = "530010805300007",
  novo_codigo  = "5300108053000071"
)

# Unir setores de 2010 que formam o setor 530010805300004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300004", "530010805300052"),
  setor_2000   = "530010805300004",
  novo_codigo  = "5300108053000041"
)

# Unir setores de 2010 que formam o setor 530010805300013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300055", "530010805300013"),
  setor_2000   = "530010805300013",
  novo_codigo  = "5300108053000131"
)

# Unir setores de 2010 que formam o setor 530010805300015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300056", "530010805300015"),
  setor_2000   = "530010805300015",
  novo_codigo  = "5300108053000151"
)

# Unir setores de 2010 que formam o setor 530010805300016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300016", "530010805300095"),
  setor_2000   = "530010805300016",
  novo_codigo  = "5300108053000161"
)

# Unir setores de 2010 que formam o setor 530010805300022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300022", "530010805300058"),
  setor_2000   = "530010805300022",
  novo_codigo  = "5300108053000221"
)

# Unir setores de 2010 que formam o setor 530010805300019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300019", "530010805300057"),
  setor_2000   = "530010805300019",
  novo_codigo  = "5300108053000191"
)

# Unir setores de 2010 que formam o setor 530010805300003 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300051", "530010805300050", "530010805300049", "530010805300003"),
  setor_2000   = "530010805300003",
  novo_codigo  = "5300108053000031"
)

# Unir setores de 2010 que formam o setor 530010805300002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300048", "530010805300002"),
  setor_2000   = "530010805300002",
  novo_codigo  = "5300108053000021"
)

# Unir setores de 2010 que formam o setor 530010805230013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230013", "530010805230049"),
  setor_2000   = "530010805230013",
  novo_codigo  = "5300108052300131"
)

# Unir setores de 2010 que formam o setor 530010805230012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230012", "530010805230048"),
  setor_2000   = "530010805230012",
  novo_codigo  = "5300108052300121"
)

# Unir setores de 2010 que formam o setor 530010805230010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805230010", "530010805230047"),
  setor_2000   = "530010805230010",
  novo_codigo  = "5300108052300101"
)

# Unir setores de 2010 que formam o setor 530010805060282 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060282", "530010805060376"),
  setor_2000   = "530010805060282",
  novo_codigo  = "5300108050602821"
)

# Unir setores de 2010 que formam o setor 530010805060265 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060358", "530010805060265"),
  setor_2000   = "530010805060265",
  novo_codigo  = "5300108050602651"
)

# Unir setores de 2010 que formam o setor 530010805100113 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100279", "530010805100278", "530010805100275", "530010805100113"),
  setor_2000   = "530010805100113",
  novo_codigo  = "5300108051001131"
)

# Unir setores de 2010 que formam o setor 530010805100111 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100111", "530010805100136"),
  setor_2000   = "530010805100111",
  novo_codigo  = "5300108051001111"
)

# Unir setores de 2010 que formam o setor 530010805100116 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100116", "530010805100281"),
  setor_2000   = "530010805100116",
  novo_codigo  = "5300108051001161"
)

# Unir setores de 2010 que formam o setor 530010805100115 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100280", "530010805100115"),
  setor_2000   = "530010805100115",
  novo_codigo  = "5300108051001151"
)

# Unir setores de 2010 que formam o setor 530010805100108 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100274", "530010805100273", "530010805100108"),
  setor_2000   = "530010805100108",
  novo_codigo  = "5300108051001081"
)

# Unir setores de 2010 que formam o setor 530010805100104 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100104", "530010805100266", "530010805100267"),
  setor_2000   = "530010805100104",
  novo_codigo  = "5300108051001041"
)

# Unir setores de 2010 que formam o setor 530010805100104 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100264", "530010805100265", "530010805100263", "530010805100103"),
  setor_2000   = "530010805100103",
  novo_codigo  = "5300108051001031"
)

# Unir setores de 2010 que formam o setor 530010805100102 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100102", "530010805100260", "530010805100261", "530010805100262"),
  setor_2000   = "530010805100102",
  novo_codigo  = "5300108051001021"
)

# Unir setores de 2010 que formam o setor 530010805100082 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100134", "530010805100082"),
  setor_2000   = "530010805100082",
  novo_codigo  = "5300108051000821"
)

# Unir setores de 2010 que formam o setor 530010805100083 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100083", "530010805100251", "530010805100252", "530010805100253"),
  setor_2000   = "530010805100083",
  novo_codigo  = "5300108051000831"
)

# Unir setores de 2010 que formam o setor 530010805100092 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100092", "530010805100255"),
  setor_2000   = "530010805100092",
  novo_codigo  = "5300108051000921"
)

# Unir setores de 2010 que formam o setor 530010805100093 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100093", "530010805100256"),
  setor_2000   = "530010805100093",
  novo_codigo  = "5300108051000931"
)

# Unir setores de 2010 que formam o setor 530010805100096 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100096", "530010805100257"),
  setor_2000   = "530010805100096",
  novo_codigo  = "5300108051000961"
)

# Unir setores de 2010 que formam o setor 530010805100098 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100135", "530010805100098"),
  setor_2000   = "530010805100098",
  novo_codigo  = "5300108051000981"
)

# Unir setores de 2010 que formam o setor 530010805100100 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100100", "530010805100259", "530010805100258"),
  setor_2000   = "530010805100100",
  novo_codigo  = "5300108051001001"
)

# Unir setores de 2010 que formam o setor 530010805100079 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100079", "530010805100250"),
  setor_2000   = "530010805100079",
  novo_codigo  = "5300108051000791"
)

# Unir setores de 2010 que formam o setor 530010805100072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100072", "530010805100249"),
  setor_2000   = "530010805100072",
  novo_codigo  = "5300108051000721"
)

# Unir setores de 2010 que formam o setor 530010805100068 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100068", "530010805100248"),
  setor_2000   = "530010805100068",
  novo_codigo  = "5300108051000681"
)

# Unir setores de 2010 que formam o setor 530010805100069 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100069", "530010805100132", "530010805100133"),
  setor_2000   = "530010805100069",
  novo_codigo  = "5300108051000691"
)

# Unir setores de 2010 que formam o setor 530010805100067 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100067", "530010805100319", "530010805100320"),
  setor_2000   = "530010805100067",
  novo_codigo  = "5300108051000671"
)

# Unir setores de 2010 que formam o setor 530010805100066 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100066", "530010805100131", "530010805100247"),
  setor_2000   = "530010805100066",
  novo_codigo  = "5300108051000661"
)

# Unir setores de 2010 que formam o setor 530010805100065 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100065", "530010805100246", "530010805100130"),
  setor_2000   = "530010805100065",
  novo_codigo  = "5300108051000651"
)

# Unir setores de 2010 que formam o setor 530010805100049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100049", "530010805100126", "530010805100313", "530010805100225", "530010805100224", "530010805100127"),
  setor_2000   = "530010805100049",
  novo_codigo  = "5300108051000491"
)

# Unir setores de 2010 que formam o setor 530010805100048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100241", "530010805100048"),
  setor_2000   = "530010805100048",
  novo_codigo  = "5300108051000481"
)

# Unir setores de 2010 que formam o setor 530010805100051 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100242", "530010805100051"),
  setor_2000   = "530010805100051",
  novo_codigo  = "5300108051000511"
)

# Unir setores de 2010 que formam o setor 530010805100053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100053", "530010805100243"),
  setor_2000   = "530010805100053",
  novo_codigo  = "5300108051000531"
)

# Unir setores de 2010 que formam o setor 530010805100054 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100244", "530010805100054"),
  setor_2000   = "530010805100054",
  novo_codigo  = "5300108051000541"
)

# Unir setores de 2010 que formam o setor 530010805100055 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100055", "530010805100315", "530010805100316"),
  setor_2000   = "530010805100055",
  novo_codigo  = "5300108051000551"
)

# Unir setores de 2010 que formam o setor 530010805100060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100060", "530010805100245"),
  setor_2000   = "530010805100060",
  novo_codigo  = "5300108051000601"
)

# Unir setores de 2010 que formam o setor 530010805100059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100059", "530010805100129", "530010805100128"),
  setor_2000   = "530010805100059",
  novo_codigo  = "5300108051000591"
)

# Unir setores de 2010 que formam o setor 530010805100015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100237", "530010805100015"),
  setor_2000   = "530010805100015",
  novo_codigo  = "5300108051000151"
)

# Unir setores de 2010 que formam o setor 530010805100006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100236", "530010805100006"),
  setor_2000   = "530010805100006",
  novo_codigo  = "5300108051000061"
)

# Unir setores de 2010 que formam o setor 530010805100016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100238", "530010805100016"),
  setor_2000   = "530010805100016",
  novo_codigo  = "5300108051000161"
)

# Unir setores de 2010 que formam o setor 530010805100027 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100240", "530010805100027"),
  setor_2000   = "530010805100027",
  novo_codigo  = "5300108051000271"
)

# Unir setores de 2010 que formam o setor 530010805100017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100017", "530010805100125"),
  setor_2000   = "530010805100017",
  novo_codigo  = "5300108051000171"
)

# Unir setores de 2010 que formam o setor 530010805100017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100017", "530010805100125"),
  setor_2000   = "530010805100017",
  novo_codigo  = "5300108051000171"
)

# Unir setores de 2010 que formam o setor 530010805110096 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110096", "530010805110158"),
  setor_2000   = "530010805110096",
  novo_codigo  = "5300108051100961"
)

# Unir setores de 2010 que formam o setor 530010805110095 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110095", "530010805110238"),
  setor_2000   = "530010805110095",
  novo_codigo  = "5300108051100951"
)

# Unir setores de 2010 que formam o setor 530010805110092 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110092", "530010805110286"),
  setor_2000   = "530010805110092",
  novo_codigo  = "5300108051100921"
)

# Unir setores de 2010 que formam o setor 530010805110091 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110091", "530010805110173", "530010805110237"),
  setor_2000   = "530010805110091",
  novo_codigo  = "5300108051100911"
)

# Unir setores de 2010 que formam o setor 530010805110089 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110089", "530010805110171"),
  setor_2000   = "530010805110089",
  novo_codigo  = "5300108051100891"
)

# Unir setores de 2010 que formam o setor 530010805110090 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110090", "530010805110172"),
  setor_2000   = "530010805110090",
  novo_codigo  = "5300108051100901"
)

# Unir setores de 2010 que formam o setor 530010805110088 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110088", "530010805110170"),
  setor_2000   = "530010805110088",
  novo_codigo  = "5300108051100881"
)

# Unir setores de 2010 que formam o setor 530010805110087 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110087", "530010805110236"),
  setor_2000   = "530010805110087",
  novo_codigo  = "5300108051100871"
)

# Unir setores de 2010 que formam o setor 530010805110086 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110086", "530010805110235"),
  setor_2000   = "530010805110086",
  novo_codigo  = "5300108051100861"
)

# Unir setores de 2010 que formam o setor 530010805110085 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110085", "530010805110169"),
  setor_2000   = "530010805110085",
  novo_codigo  = "5300108051100851"
)

# Unir setores de 2010 que formam o setor 530010805110082 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110082", "530010805110168"),
  setor_2000   = "530010805110082",
  novo_codigo  = "5300108051100821"
)

# Unir setores de 2010 que formam o setor 530010805110083 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110083", "530010805110233"),
  setor_2000   = "530010805110083",
  novo_codigo  = "5300108051100831"
)

# Unir setores de 2010 que formam o setor 530010805110080 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110080", "530010805110230"),
  setor_2000   = "530010805110080",
  novo_codigo  = "5300108051100801"
)

# Unir setores de 2010 que formam o setor 530010805110076 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110076", "530010805110229"),
  setor_2000   = "530010805110076",
  novo_codigo  = "5300108051100761"
)

# Unir setores de 2010 que formam o setor 530010805110074 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110074", "530010805110227"),
  setor_2000   = "530010805110074",
  novo_codigo  = "5300108051100741"
)

# Unir setores de 2010 que formam o setor 530010805110075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110075", "530010805110228"),
  setor_2000   = "530010805110075",
  novo_codigo  = "5300108051100751"
)

# Unir setores de 2010 que formam o setor 530010805110073 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110073", "530010805110167", "530010805110226"),
  setor_2000   = "530010805110073",
  novo_codigo  = "5300108051100731"
)

# Unir setores de 2010 que formam o setor 530010805110072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110072", "530010805110166"),
  setor_2000   = "530010805110072",
  novo_codigo  = "5300108051100721"
)

# Unir setores de 2010 que formam o setor 530010805110070 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110070", "530010805110225"),
  setor_2000   = "530010805110070",
  novo_codigo  = "5300108051100701"
)

# Unir setores de 2010 que formam o setor 530010805110067 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110165", "530010805110275", "530010805110067"),
  setor_2000   = "530010805110067",
  novo_codigo  = "5300108051100671"
)

# Unir setores de 2010 que formam o setor 530010805110012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110012", "530010805110214"),
  setor_2000   = "530010805110012",
  novo_codigo  = "5300108051100121"
)

# Unir setores de 2010 que formam o setor 530010805110009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110009", "530010805110212"),
  setor_2000   = "530010805110009",
  novo_codigo  = "5300108051100091"
)

# Unir setores de 2010 que formam o setor 530010805110013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110013", "530010805110215"),
  setor_2000   = "530010805110013",
  novo_codigo  = "5300108051100131"
)

# Unir setores de 2010 que formam o setor 530010805110011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110011", "530010805110213"),
  setor_2000   = "530010805110011",
  novo_codigo  = "5300108051100111"
)

# Unir setores de 2010 que formam o setor 530010805110008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110008", "530010805110211"),
  setor_2000   = "530010805110008",
  novo_codigo  = "5300108051100081"
)

# Unir setores de 2010 que formam o setor 530010805110007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110007", "530010805110210"),
  setor_2000   = "530010805110007",
  novo_codigo  = "5300108051100071"
)

# Unir setores de 2010 que formam o setor 530010805110005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110005", "530010805110209"),
  setor_2000   = "530010805110005",
  novo_codigo  = "5300108051100051"
)

# Unir setores de 2010 que formam o setor 530010805110002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110002", "530010805110208"),
  setor_2000   = "530010805110002",
  novo_codigo  = "5300108051100021"
)

# Unir setores de 2010 que formam o setor 530010805110035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110035", "530010805110162"),
  setor_2000   = "530010805110035",
  novo_codigo  = "5300108051100351"
)

# Unir setores de 2010 que formam o setor 530010805110036 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110036", "530010805110217"),
  setor_2000   = "530010805110036",
  novo_codigo  = "5300108051100361"
)

# Unir setores de 2010 que formam o setor 530010805110041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110041", "530010805110218", "530010805110219"),
  setor_2000   = "530010805110041",
  novo_codigo  = "5300108051100411"
)

# Unir setores de 2010 que formam o setor 530010805110042 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110042", "530010805110220"),
  setor_2000   = "530010805110042",
  novo_codigo  = "5300108051100421"
)

# Unir setores de 2010 que formam o setor 530010805110043 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110043", "530010805110163"),
  setor_2000   = "530010805110043",
  novo_codigo  = "5300108051100431"
)

# Unir setores de 2010 que formam o setor 530010805110048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110048", "530010805110164"),
  setor_2000   = "530010805110048",
  novo_codigo  = "5300108051100481"
)

# Unir setores de 2010 que formam o setor 530010805110063 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110063", "530010805110284"),
  setor_2000   = "530010805110063",
  novo_codigo  = "5300108051100631"
)

# Unir setores de 2010 que formam o setor 530010805110067 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110067", "530010805110275", "530010805110165"),
  setor_2000   = "530010805110067",
  novo_codigo  = "5300108051100671"
)

# Unir setores de 2010 que formam o setor 530010805110070 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110070", "530010805110225"),
  setor_2000   = "530010805110070",
  novo_codigo  = "5300108051100701"
)

# Unir setores de 2010 que formam o setor 530010805110116 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110116", "530010805110159"),
  setor_2000   = "530010805110116",
  novo_codigo  = "5300108051101161"
)

# Unir setores de 2010 que formam o setor 530010805110117 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110117", "530010805110160", "530010805110247", "530010805110274"),
  setor_2000   = "530010805110117",
  novo_codigo  = "5300108051101171"
)

# Unir setores de 2010 que formam o setor 530010805110123 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110123", "530010805110251"),
  setor_2000   = "530010805110123",
  novo_codigo  = "5300108051101231"
)

# Unir setores de 2010 que formam o setor 530010805110124 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110124", "530010805110252"),
  setor_2000   = "530010805110124",
  novo_codigo  = "5300108051101241"
)

# Unir setores de 2010 que formam o setor 530010805110119 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110119", "530010805110248"),
  setor_2000   = "530010805110119",
  novo_codigo  = "5300108051101191"
)

# Unir setores de 2010 que formam o setor 530010805110140 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110140", "530010805110263"),
  setor_2000   = "530010805110140",
  novo_codigo  = "5300108051101401"
)

# Unir setores de 2010 que formam o setor 530010805110137 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110137", "530010805110262"),
  setor_2000   = "530010805110137",
  novo_codigo  = "5300108051101371"
)

# Unir setores de 2010 que formam o setor 530010805110136 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110136", "530010805110261"),
  setor_2000   = "530010805110136",
  novo_codigo  = "5300108051101361"
)

# Unir setores de 2010 que formam o setor 530010805110133 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110133", "530010805110260"),
  setor_2000   = "530010805110133",
  novo_codigo  = "5300108051101331"
)

# Unir setores de 2010 que formam o setor 530010805110061 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110061", "530010805110224"),
  setor_2000   = "530010805110061",
  novo_codigo  = "5300108051100611"
)

# Unir setores de 2010 que formam o setor 530010805110057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110057", "530010805110222"),
  setor_2000   = "530010805110057",
  novo_codigo  = "5300108051100571"
)

# Unir setores de 2010 que formam o setor 530010805110055 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110055", "530010805110221"),
  setor_2000   = "530010805110055",
  novo_codigo  = "5300108051100551"
)

# Unir setores de 2010 que formam o setor 530010805110148 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110148", "530010805110270"),
  setor_2000   = "530010805110148",
  novo_codigo  = "5300108051101481"
)

# Unir setores de 2010 que formam o setor 530010805110149 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110149", "530010805110271"),
  setor_2000   = "530010805110149",
  novo_codigo  = "5300108051101491"
)

# Unir setores de 2010 que formam o setor 530010805120001 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120001", "530010805120046"),
  setor_2000   = "530010805120001",
  novo_codigo  = "5300108051200011"
)

# Unir setores de 2010 que formam o setor 530010805120004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120004", "530010805120047"),
  setor_2000   = "530010805120004",
  novo_codigo  = "5300108051200041"
)

# Unir setores de 2010 que formam o setor 530010805120006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120049", "530010805120050", "530010805120006"),
  setor_2000   = "530010805120006",
  novo_codigo  = "5300108051200061"
)

# Unir setores de 2010 que formam o setor 530010805120008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120008", "530010805120051"),
  setor_2000   = "530010805120008",
  novo_codigo  = "5300108051200081"
)

# Unir setores de 2010 que formam o setor 530010805120010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120010", "530010805120052"),
  setor_2000   = "530010805120010",
  novo_codigo  = "5300108051200101"
)

# Unir setores de 2010 que formam o setor 530010805120012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120012", "530010805120054"),
  setor_2000   = "530010805120012",
  novo_codigo  = "5300108051200121"
)

# Unir setores de 2010 que formam o setor 530010805120011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120053", "530010805120011"),
  setor_2000   = "530010805120011",
  novo_codigo  = "5300108051200111"
)

# Unir setores de 2010 que formam o setor 530010805120018 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120018", "530010805120055"),
  setor_2000   = "530010805120018",
  novo_codigo  = "5300108051200181"
)

# Unir setores de 2010 que formam o setor 530010805120024 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120024", "530010805120058"),
  setor_2000   = "530010805120024",
  novo_codigo  = "5300108051200241"
)

# Unir setores de 2010 que formam o setor 530010805120023 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120023", "530010805120057"),
  setor_2000   = "530010805120023",
  novo_codigo  = "5300108051200231"
)

# Unir setores de 2010 que formam o setor 530010805120022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120022", "530010805120056"),
  setor_2000   = "530010805120022",
  novo_codigo  = "5300108051200221"
)

# Unir setores de 2010 que formam o setor 530010805120037 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120037", "530010805120060"),
  setor_2000   = "530010805120037",
  novo_codigo  = "5300108051200371"
)

# Unir setores de 2010 que formam o setor 530010805120031 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120031", "530010805120059"),
  setor_2000   = "530010805120031",
  novo_codigo  = "5300108051200311"
)

# Unir setores de 2010 que formam o setor 530010805170063 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170063", "530010805170141", "530010805170142", "530010805170099", "530010805170100", "530010805170143"),
  setor_2000   = "530010805170063",
  novo_codigo  = "5300108051700631"
)

# Unir setores de 2010 que formam o setor 530010805170078 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170118", "530010805170078", "530010805170119", "530010805170120"),
  setor_2000   = "530010805170078",
  novo_codigo  = "5300108051700781"
)

# Unir setores de 2010 que formam o setor 530010805170011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170011", "530010805170082"),
  setor_2000   = "530010805170011",
  novo_codigo  = "5300108051700111"
)

# Unir setores de 2010 que formam o setor 530010805170077 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170117", "530010805170077", "530010805170135"),
  setor_2000   = "530010805170077",
  novo_codigo  = "5300108051700771"
)

# Unir setores de 2010 que formam o setor 530010805170076 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170076", "530010805170129"),
  setor_2000   = "530010805170076",
  novo_codigo  = "5300108051700761"
)

# Unir setores de 2010 que formam o setor 530010805170062 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170062", "530010805170096", "530010805170097", "530010805170098"),
  setor_2000   = "530010805170062",
  novo_codigo  = "5300108051700621"
)

# Unir setores de 2010 que formam o setor 530010805170061 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170061", "530010805170126"),
  setor_2000   = "530010805170061",
  novo_codigo  = "5300108051700611"
)

# Unir setores de 2010 que formam o setor 530010805170060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170060", "530010805170095"),
  setor_2000   = "530010805170060",
  novo_codigo  = "5300108051700601"
)

# Unir setores de 2010 que formam o setor 530010805170059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170059", "530010805170125"),
  setor_2000   = "530010805170059",
  novo_codigo  = "5300108051700591"
)

# Unir setores de 2010 que formam o setor 530010805170058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170058", "530010805170094"),
  setor_2000   = "530010805170058",
  novo_codigo  = "5300108051700581"
)

# Unir setores de 2010 que formam o setor 530010805170057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170057", "530010805170124"),
  setor_2000   = "530010805170057",
  novo_codigo  = "5300108051700571"
)

# Unir setores de 2010 que formam o setor 530010805170064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170064", "530010805170101"),
  setor_2000   = "530010805170064",
  novo_codigo  = "5300108051700641"
)

# Unir setores de 2010 que formam o setor 530010805170065 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170109", "530010805170110", "530010805170105", "530010805170106", "530010805170108", "530010805170102", "530010805170107", "530010805170134", "530010805170065", "530010805170127", "530010805170133", "530010805170132", "530010805170103", "530010805170104"),
  setor_2000   = "530010805170065",
  novo_codigo  = "5300108051700651"
)

# Unir setores de 2010 que formam o setor 530010805170075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170075", "530010805170128"),
  setor_2000   = "530010805170075",
  novo_codigo  = "5300108051700751"
)

# Unir setores de 2010 que formam o setor 530010805170072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170114", "530010805170115", "530010805170116", "530010805170116", "530010805170072"),
  setor_2000   = "530010805170072",
  novo_codigo  = "5300108051700721"
)

# Unir setores de 2010 que formam o setor 530010805170068 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170111", "530010805170068"),
  setor_2000   = "530010805170068",
  novo_codigo  = "5300108051700681"
)

# Unir setores de 2010 que formam o setor 530010805170069 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170112", "530010805170069"),
  setor_2000   = "530010805170069",
  novo_codigo  = "5300108051700691"
)

# Unir setores de 2010 que formam o setor 530010805170070 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170113", "530010805170070"),
  setor_2000   = "530010805170070",
  novo_codigo  = "5300108051700701"
)

# Unir setores de 2010 que formam o setor 530010805170053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170053", "530010805170093"),
  setor_2000   = "530010805170053",
  novo_codigo  = "5300108051700531"
)

# Unir setores de 2010 que formam o setor 530010805170052 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170052", "530010805170136", "530010805170137", "530010805170138", "530010805170139", "530010805170140"),
  setor_2000   = "530010805170052",
  novo_codigo  = "5300108051700521"
)

# Unir setores de 2010 que formam o setor 530010805170050 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170050", "530010805170092"),
  setor_2000   = "530010805170050",
  novo_codigo  = "5300108051700501"
)

# Unir setores de 2010 que formam o setor 530010805170049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170049", "530010805170091"),
  setor_2000   = "530010805170049",
  novo_codigo  = "5300108051700491"
)

# Unir setores de 2010 que formam o setor 530010805170048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170048", "530010805170090"),
  setor_2000   = "530010805170048",
  novo_codigo  = "5300108051700481"
)

# Unir setores de 2010 que formam o setor 530010805170046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170046", "530010805170123"),
  setor_2000   = "530010805170046",
  novo_codigo  = "5300108051700461"
)

# Unir setores de 2010 que formam o setor 530010805170045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170045", "530010805170089"),
  setor_2000   = "530010805170045",
  novo_codigo  = "5300108051700451"
)

# Unir setores de 2010 que formam o setor 530010805170028 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170122", "530010805170028"),
  setor_2000   = "530010805170028",
  novo_codigo  = "5300108051700281"
)

# Unir setores de 2010 que formam o setor 530010805170029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170029", "530010805170087"),
  setor_2000   = "530010805170029",
  novo_codigo  = "5300108051700291"
)

# Unir setores de 2010 que formam o setor 530010805170030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170030", "530010805170088"),
  setor_2000   = "530010805170030",
  novo_codigo  = "5300108051700301"
)

# Unir setores de 2010 que formam o setor 530010805170020 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170121", "530010805170020"),
  setor_2000   = "530010805170020",
  novo_codigo  = "5300108051700201"
)

# Unir setores de 2010 que formam o setor 530010805170022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170022", "530010805170086"),
  setor_2000   = "530010805170022",
  novo_codigo  = "5300108051700221"
)

# Unir setores de 2010 que formam o setor 530010805170017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170017", "530010805170085"),
  setor_2000   = "530010805170017",
  novo_codigo  = "5300108051700171"
)

# Unir setores de 2010 que formam o setor 530010805170012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170012", "530010805170083"),
  setor_2000   = "530010805170012",
  novo_codigo  = "5300108051700121"
)

# Unir setores de 2010 que formam o setor 530010805170013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170013", "530010805170084"),
  setor_2000   = "530010805170013",
  novo_codigo  = "5300108051700131"
)

# Unir setores de 2010 que formam o setor 530010805170079 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170079", "530010805170144", "530010805170147", "530010805170149", "530010805170151", "530010805170150", "530010805170145", "530010805170146"),
  setor_2000   = "530010805170079",
  novo_codigo  = "5300108051700791"
)

# Unir setores de 2010 que formam o setor 530010805170080 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170080", "530010805170130"),
  setor_2000   = "530010805170080",
  novo_codigo  = "5300108051700801"
)

# Unir setores de 2010 que formam o setor 530010805170081 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805170081", "530010805170131"),
  setor_2000   = "530010805170081",
  novo_codigo  = "5300108051700811"
)

# Unir setores de 2010 que formam o setor 530010805060235 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060235", "530010805060375", "530010805060435"),
  setor_2000   = "530010805060235",
  novo_codigo  = "5300108050602351"
)

# Unir setores de 2010 que formam o setor 530010805060234 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060234", "530010805060413"),
  setor_2000   = "530010805060234",
  novo_codigo  = "5300108050602341"
)

# Unir setores de 2010 que formam o setor 530010805060171 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060369", "530010805060171"),
  setor_2000   = "530010805060171",
  novo_codigo  = "5300108050601711"
)

# Unir setores de 2010 que formam o setor 530010805060170 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060368", "530010805060170"),
  setor_2000   = "530010805060170",
  novo_codigo  = "5300108050601701"
)

# Unir setores de 2010 que formam o setor 530010805060175 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060175", "530010805060408"),
  setor_2000   = "530010805060175",
  novo_codigo  = "5300108050601751"
)

# Unir setores de 2010 que formam o setor 530010805060179 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060179", "530010805060409"),
  setor_2000   = "530010805060179",
  novo_codigo  = "5300108050601791"
)

# Unir setores de 2010 que formam o setor 530010805060180 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060180", "530010805060410"),
  setor_2000   = "530010805060180",
  novo_codigo  = "5300108050601801"
)

# Unir setores de 2010 que formam o setor 530010805060184 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060371", "530010805060184"),
  setor_2000   = "530010805060184",
  novo_codigo  = "5300108050601841"
)

# Unir setores de 2010 que formam o setor 530010805060185 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060372", "530010805060185"),
  setor_2000   = "530010805060185",
  novo_codigo  = "5300108050601851"
)

# Unir setores de 2010 que formam o setor 530010805060224 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060224", "530010805060411"),
  setor_2000   = "530010805060224",
  novo_codigo  = "5300108050602241"
)

# Unir setores de 2010 que formam o setor 530010805060216 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060373", "530010805060216"),
  setor_2000   = "530010805060216",
  novo_codigo  = "5300108050602161"
)

# Unir setores de 2010 que formam o setor 530010805060169 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060367", "530010805060169"),
  setor_2000   = "530010805060169",
  novo_codigo  = "5300108050601691"
)

# Unir setores de 2010 que formam o setor 530010805060159 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060366", "530010805060159"),
  setor_2000   = "530010805060159",
  novo_codigo  = "5300108050601591"
)

# Unir setores de 2010 que formam o setor 530010805060156 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060156", "530010805060407"),
  setor_2000   = "530010805060156",
  novo_codigo  = "5300108050601561"
)

# Unir setores de 2010 que formam o setor 530010805060151 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060151", "530010805060404"),
  setor_2000   = "530010805060151",
  novo_codigo  = "5300108050601511"
)

# Unir setores de 2010 que formam o setor 530010805060152 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060152", "530010805060405"),
  setor_2000   = "530010805060152",
  novo_codigo  = "5300108050601521"
)

# Unir setores de 2010 que formam o setor 530010805060153 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060153", "530010805060406"),
  setor_2000   = "530010805060153",
  novo_codigo  = "5300108050601531"
)

# Unir setores de 2010 que formam o setor 530010805060149 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060149", "530010805060403"),
  setor_2000   = "530010805060149",
  novo_codigo  = "5300108050601491"
)

# Unir setores de 2010 que formam o setor 530010805060145 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060145", "530010805060364"),
  setor_2000   = "530010805060145",
  novo_codigo  = "5300108050601451"
)

# Unir setores de 2010 que formam o setor 530010805060144 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060363", "530010805060144"),
  setor_2000   = "530010805060144",
  novo_codigo  = "5300108050601441"
)

# Unir setores de 2010 que formam o setor 530010805060141 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060141", "530010805060361"),
  setor_2000   = "530010805060141",
  novo_codigo  = "5300108050601411"
)

# Unir setores de 2010 que formam o setor 530010805060126 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060126", "530010805060400"),
  setor_2000   = "530010805060126",
  novo_codigo  = "5300108050601261"
)

# Unir setores de 2010 que formam o setor 530010805060122 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060122", "530010805060399"),
  setor_2000   = "530010805060122",
  novo_codigo  = "5300108050601221"
)

# Unir setores de 2010 que formam o setor 530010805060116 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060116", "530010805060360"),
  setor_2000   = "530010805060116",
  novo_codigo  = "5300108050601161"
)

# Unir setores de 2010 que formam o setor 530010805060115 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060115", "530010805060398"),
  setor_2000   = "530010805060115",
  novo_codigo  = "5300108050601151"
)

# Unir setores de 2010 que formam o setor 530010805060114 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060114", "530010805060397"),
  setor_2000   = "530010805060114",
  novo_codigo  = "5300108050601141"
)

# Unir setores de 2010 que formam o setor 530010805060128 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060128", "530010805060401"),
  setor_2000   = "530010805060128",
  novo_codigo  = "5300108050601281"
)

# Unir setores de 2010 que formam o setor 530010805060128 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060128", "530010805060401"),
  setor_2000   = "530010805060128",
  novo_codigo  = "5300108050601281"
)

# Unir setores de 2010 que formam o setor 530010805060128 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060362", "530010805060142"),
  setor_2000   = "530010805060128",
  novo_codigo  = "5300108050601281"
)

# Unir setores de 2010 que formam o setor 530010805060146 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060146", "530010805060365"),
  setor_2000   = "530010805060146",
  novo_codigo  = "5300108050601461"
)

# Unir setores de 2010 que formam o setor 530010805060183 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060370", "530010805060183"),
  setor_2000   = "530010805060183",
  novo_codigo  = "5300108050601831"
)

# Unir setores de 2010 que formam o setor 530010805060183 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060370", "530010805060183"),
  setor_2000   = "530010805060183",
  novo_codigo  = "5300108050601831"
)

# Unir setores de 2010 que formam o setor 530010805060147 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060147", "530010805060402"),
  setor_2000   = "530010805060147",
  novo_codigo  = "5300108050601471"
)

# Unir setores de 2010 que formam o setor 530010805060005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060005", "530010805060377", "530010805060378"),
  setor_2000   = "530010805060005",
  novo_codigo  = "5300108050600051"
)

# Unir setores de 2010 que formam o setor 530010805060238 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060416", "530010805060414", "530010805060415", "530010805060238"),
  setor_2000   = "530010805060238",
  novo_codigo  = "5300108050602381"
)

# Unir setores de 2010 que formam o setor 530010805060113 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060113", "530010805060335"),
  setor_2000   = "530010805060113",
  novo_codigo  = "5300108050601131"
)

# Unir setores de 2010 que formam o setor 530010805060111 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060111", "530010805060396"),
  setor_2000   = "530010805060111",
  novo_codigo  = "5300108050601111"
)

# Unir setores de 2010 que formam o setor 530010805060006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060006", "530010805060379"),
  setor_2000   = "530010805060006",
  novo_codigo  = "5300108050600061"
)

# Unir setores de 2010 que formam o setor 530010805060112 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060112", "530010805060334"),
  setor_2000   = "530010805060112",
  novo_codigo  = "5300108050601121"
)

# Unir setores de 2010 que formam o setor 530010805060109 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060333", "530010805060109"),
  setor_2000   = "530010805060109",
  novo_codigo  = "5300108050601091"
)

# Unir setores de 2010 que formam o setor 530010805060009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060009", "530010805060294", "530010805060295"),
  setor_2000   = "530010805060009",
  novo_codigo  = "5300108050600091"
)

# Unir setores de 2010 que formam o setor 530010805060103 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060103", "530010805060395"),
  setor_2000   = "530010805060103",
  novo_codigo  = "5300108050601031"
)

# Unir setores de 2010 que formam o setor 530010805060011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060011", "530010805060380"),
  setor_2000   = "530010805060011",
  novo_codigo  = "5300108050600111"
)

# Unir setores de 2010 que formam o setor 530010805060239 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060239", "530010805060336", "530010805060417"),
  setor_2000   = "530010805060239",
  novo_codigo  = "5300108050602391"
)

# Unir setores de 2010 que formam o setor 530010805060099 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060099", "530010805060332"),
  setor_2000   = "530010805060099",
  novo_codigo  = "5300108050600991"
)

# Unir setores de 2010 que formam o setor 530010805060016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060016", "530010805060296", "530010805060425", "530010805060297"),
  setor_2000   = "530010805060016",
  novo_codigo  = "5300108050600161"
)

# Unir setores de 2010 que formam o setor 530010805060021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060021", "530010805060381"),
  setor_2000   = "530010805060021",
  novo_codigo  = "5300108050600211"
)

# Unir setores de 2010 que formam o setor 530010805060022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060022", "530010805060382"),
  setor_2000   = "530010805060022",
  novo_codigo  = "5300108050600221"
)

# Unir setores de 2010 que formam o setor 530010805060243 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060243", "530010805060338"),
  setor_2000   = "530010805060243",
  novo_codigo  = "5300108050602431"
)

# Unir setores de 2010 que formam o setor 530010805060245 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060245", "530010805060339"),
  setor_2000   = "530010805060245",
  novo_codigo  = "5300108050602451"
)

# Unir setores de 2010 que formam o setor 530010805060032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060032", "530010805060300"),
  setor_2000   = "530010805060032",
  novo_codigo  = "5300108050600321"
)

# Unir setores de 2010 que formam o setor 530010805060034 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060034", "530010805060304"),
  setor_2000   = "530010805060034",
  novo_codigo  = "5300108050600341"
)

# Unir setores de 2010 que formam o setor 530010805060033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060033", "530010805060302", "530010805060303", "530010805060301"),
  setor_2000   = "530010805060033",
  novo_codigo  = "5300108050600331"
)

# Unir setores de 2010 que formam o setor 530010805060089 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060089", "530010805060330"),
  setor_2000   = "530010805060089",
  novo_codigo  = "5300108050600891"
)

# Unir setores de 2010 que formam o setor 530010805060087 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060087", "530010805060392"),
  setor_2000   = "530010805060087",
  novo_codigo  = "5300108050600871"
)

# Unir setores de 2010 que formam o setor 530010805060084 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060084", "530010805060329"),
  setor_2000   = "530010805060084",
  novo_codigo  = "5300108050600841"
)

# Unir setores de 2010 que formam o setor 530010805060078 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060078", "530010805060325", "530010805060326"),
  setor_2000   = "530010805060078",
  novo_codigo  = "5300108050600781"
)

# Unir setores de 2010 que formam o setor 530010805060038 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060038", "530010805060383"),
  setor_2000   = "530010805060038",
  novo_codigo  = "5300108050600381"
)

# Unir setores de 2010 que formam o setor 530010805060037 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060305", "530010805060306", "530010805060307", "530010805060037"),
  setor_2000   = "530010805060037",
  novo_codigo  = "5300108050600371"
)

# Unir setores de 2010 que formam o setor 530010805060250 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060250", "530010805060340"),
  setor_2000   = "530010805060250",
  novo_codigo  = "5300108050602501"
)

# Unir setores de 2010 que formam o setor 530010805060251 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060251", "530010805060418"),
  setor_2000   = "530010805060251",
  novo_codigo  = "5300108050602511"
)

# Unir setores de 2010 que formam o setor 530010805060079 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060079", "530010805060327", "530010805060328"),
  setor_2000   = "530010805060079",
  novo_codigo  = "5300108050600791"
)

# Unir setores de 2010 que formam o setor 530010805060075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060320", "530010805060321", "530010805060428", "530010805060390", "530010805060075", "530010805060319", "530010805060324", "530010805060427", "530010805060322", "530010805060323", "530010805060429"),
  setor_2000   = "530010805060075",
  novo_codigo  = "5300108050600751"
)

# Unir setores de 2010 que formam o setor 530010805060074 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060316", "530010805060389", "530010805060074", "530010805060317", "530010805060318", "530010805060426"),
  setor_2000   = "530010805060074",
  novo_codigo  = "5300108050600741"
)

# Unir setores de 2010 que formam o setor 530010805060072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060072", "530010805060315"),
  setor_2000   = "530010805060074",
  novo_codigo  = "5300108050600741"
)

# Unir setores de 2010 que formam o setor 530010805060064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060064", "530010805060314"),
  setor_2000   = "530010805060064",
  novo_codigo  = "5300108050600641"
)

# Unir setores de 2010 que formam o setor 530010805060062 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060062", "530010805060388"),
  setor_2000   = "530010805060062",
  novo_codigo  = "5300108050600621"
)

# Unir setores de 2010 que formam o setor 530010805060059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060059", "530010805060312"),
  setor_2000   = "530010805060059",
  novo_codigo  = "5300108050600591"
)

# Unir setores de 2010 que formam o setor 530010805060058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060058", "530010805060387"),
  setor_2000   = "530010805060058",
  novo_codigo  = "5300108050600581"
)

# Unir setores de 2010 que formam o setor 530010805060261 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060261", "530010805060357"),
  setor_2000   = "530010805060261",
  novo_codigo  = "5300108050602611"
)

# Unir setores de 2010 que formam o setor 530010805060057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060057", "530010805060311"),
  setor_2000   = "530010805060057",
  novo_codigo  = "5300108050600571"
)

# Unir setores de 2010 que formam o setor 530010805060257 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060257", "530010805060352"),
  setor_2000   = "530010805060257",
  novo_codigo  = "5300108050602571"
)

# Unir setores de 2010 que formam o setor 530010805060053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060053", "530010805060386"),
  setor_2000   = "530010805060053",
  novo_codigo  = "5300108050600531"
)

# Unir setores de 2010 que formam o setor 530010805060052 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060052", "530010805060385"),
  setor_2000   = "530010805060052",
  novo_codigo  = "5300108050600521"
)

# Unir setores de 2010 que formam o setor 530010805060253 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060253", "530010805060346"),
  setor_2000   = "530010805060253",
  novo_codigo  = "5300108050602531"
)

# Unir setores de 2010 que formam o setor 530010805060253 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060253", "530010805060346"),
  setor_2000   = "530010805060253",
  novo_codigo  = "5300108050602531"
)

# Unir setores de 2010 que formam o setor 530010805060255 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060350", "530010805060351", "530010805060255", "530010805060348", "530010805060347", "530010805060349"),
  setor_2000   = "530010805060255",
  novo_codigo  = "5300108050602551"
)

# Unir setores de 2010 que formam o setor 530010805060260 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060260", "530010805060356"),
  setor_2000   = "530010805060260",
  novo_codigo  = "5300108050602601"
)

# Unir setores de 2010 que formam o setor 530010805060263 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060263", "530010805060421"),
  setor_2000   = "530010805060263",
  novo_codigo  = "5300108050602631"
)

# Unir setores de 2010 que formam o setor 530010805060252 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060345", "530010805060434", "530010805060430", "530010805060431", "530010805060432", "530010805060433", "530010805060342", "530010805060344", "530010805060341", "530010805060343", "530010805060252"),
  setor_2000   = "530010805060252",
  novo_codigo  = "5300108050602521"
)

# Unir setores de 2010 que formam o setor 530010805180003 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180003", "530010805180172"),
  setor_2000   = "530010805180003",
  novo_codigo  = "5300108051800031"
)

# Unir setores de 2010 que formam o setor 530010805180002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180002", "530010805180239"),
  setor_2000   = "530010805180002",
  novo_codigo  = "5300108051800021"
)

# Unir setores de 2010 que formam o setor 530010805180006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180006", "530010805180173"),
  setor_2000   = "530010805180006",
  novo_codigo  = "5300108051800061"
)

# Unir setores de 2010 que formam o setor 530010805180007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180007", "530010805180174"),
  setor_2000   = "530010805180007",
  novo_codigo  = "5300108051800071"
)

# Unir setores de 2010 que formam o setor 530010805180009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180009", "530010805180176"),
  setor_2000   = "530010805180009",
  novo_codigo  = "5300108051800091"
)

# Unir setores de 2010 que formam o setor 530010805180008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180008", "530010805180175"),
  setor_2000   = "530010805180008",
  novo_codigo  = "5300108051800081"
)

# Unir setores de 2010 que formam o setor 530010805180010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180010", "530010805180240", "530010805180252"),
  setor_2000   = "530010805180010",
  novo_codigo  = "5300108051800101"
)

# Unir setores de 2010 que formam o setor 530010805180015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180015", "530010805180243"),
  setor_2000   = "530010805180015",
  novo_codigo  = "5300108051800151"
)

# Unir setores de 2010 que formam o setor 530010805180022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180022", "530010805180244"),
  setor_2000   = "530010805180022",
  novo_codigo  = "5300108051800221"
)

# Unir setores de 2010 que formam o setor 530010805180027 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180027", "530010805180246"),
  setor_2000   = "530010805180027",
  novo_codigo  = "5300108051800271"
)

# Unir setores de 2010 que formam o setor 530010805180028 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180028", "530010805180247"),
  setor_2000   = "530010805180028",
  novo_codigo  = "5300108051800281"
)

# Unir setores de 2010 que formam o setor 530010805180033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180033", "530010805180182"),
  setor_2000   = "530010805180033",
  novo_codigo  = "5300108051800331"
)

# Unir setores de 2010 que formam o setor 530010805180033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180033", "530010805180182"),
  setor_2000   = "530010805180033",
  novo_codigo  = "5300108051800331"
)

# Unir setores de 2010 que formam o setor 530010805180171 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180292", "530010805180293", "530010805180291", "530010805180282", "530010805180286", "530010805180288", "530010805180285", "530010805180284", "530010805180283", "530010805180287", "530010805180289", "530010805180290", "530010805180281", "530010805180171"),
  setor_2000   = "530010805180171",
  novo_codigo  = "5300108051801711"
)

# Unir setores de 2010 que formam o setor 530010805180050 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180050", "530010805180255"),
  setor_2000   = "530010805180050",
  novo_codigo  = "5300108051800501"
)

# Unir setores de 2010 que formam o setor 530010805180051 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180186", "530010805180294", "530010805180051"),
  setor_2000   = "530010805180051",
  novo_codigo  = "5300108051800511"
)

# Unir setores de 2010 que formam o setor 530010805180057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180057", "530010805180258"),
  setor_2000   = "530010805180057",
  novo_codigo  = "5300108051800571"
)

# Unir setores de 2010 que formam o setor 530010805180067 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180067", "530010805180260"),
  setor_2000   = "530010805180067",
  novo_codigo  = "5300108051800671"
)

# Unir setores de 2010 que formam o setor 530010805180001 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180233", "530010805180232", "530010805180231", "530010805180311", "530010805180230", "530010805180229", "530010805180228", "530010805180310", "530010805180227", "530010805180226", "530010805180309", "530010805180225", "530010805180224", "530010805180223", "530010805180222", "530010805180308", "530010805180221", "530010805180220", "530010805180219", "530010805180217", "530010805180304", "530010805180216", "530010805180303", "530010805180218", "530010805180305", "530010805180306", "530010805180307", "530010805180215", "530010805180302", "530010805180214", "530010805180212", "530010805180300", "530010805180301", "530010805180211", "530010805180213", "530010805180210", "530010805180001"),
  setor_2000   = "530010805180001",
  novo_codigo  = "5300108051800011"
)

# Unir setores de 2010 que formam o setor 530010805180013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180013", "530010805180242"),
  setor_2000   = "530010805180013",
  novo_codigo  = "5300108051800131"
)

# Unir setores de 2010 que formam o setor 530010805180021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180021", "530010805180178"),
  setor_2000   = "530010805180021",
  novo_codigo  = "5300108051800211"
)

# Unir setores de 2010 que formam o setor 530010805180025 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180025", "530010805180245"),
  setor_2000   = "530010805180025",
  novo_codigo  = "5300108051800251"
)

# Unir setores de 2010 que formam o setor 530010805180029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180029", "530010805180179"),
  setor_2000   = "530010805180029",
  novo_codigo  = "5300108051800291"
)

# Unir setores de 2010 que formam o setor 530010805180032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180032", "530010805180248"),
  setor_2000   = "530010805180032",
  novo_codigo  = "5300108051800321"
)

# Unir setores de 2010 que formam o setor 530010805180036 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180036", "530010805180183"),
  setor_2000   = "530010805180036",
  novo_codigo  = "5300108051800361"
)

# Unir setores de 2010 que formam o setor 530010805180041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180041", "530010805180251"),
  setor_2000   = "530010805180041",
  novo_codigo  = "5300108051800411"
)

# Unir setores de 2010 que formam o setor 530010805180040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180040", "530010805180250"),
  setor_2000   = "530010805180040",
  novo_codigo  = "5300108051800401"
)

# Unir setores de 2010 que formam o setor 530010805180045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180045", "530010805180184"),
  setor_2000   = "530010805180045",
  novo_codigo  = "5300108051800451"
)

# Unir setores de 2010 que formam o setor 530010805180046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180046", "530010805180185"),
  setor_2000   = "530010805180046",
  novo_codigo  = "5300108051800461"
)

# Unir setores de 2010 que formam o setor 530010805180049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180049", "530010805180254"),
  setor_2000   = "530010805180049",
  novo_codigo  = "5300108051800491"
)

# Unir setores de 2010 que formam o setor 530010805180047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180047", "530010805180253"),
  setor_2000   = "530010805180047",
  novo_codigo  = "5300108051800471"
)

# Unir setores de 2010 que formam o setor 530010805180054 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180054", "530010805180187"),
  setor_2000   = "530010805180054",
  novo_codigo  = "5300108051800541"
)

# Unir setores de 2010 que formam o setor 530010805180056 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180056", "530010805180257"),
  setor_2000   = "530010805180056",
  novo_codigo  = "5300108051800561"
)

# Unir setores de 2010 que formam o setor 530010805180055 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180055", "530010805180256"),
  setor_2000   = "530010805180055",
  novo_codigo  = "5300108051800551"
)

# Unir setores de 2010 que formam o setor 530010805180059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180059", "530010805180188"),
  setor_2000   = "530010805180059",
  novo_codigo  = "5300108051800591"
)

# Unir setores de 2010 que formam o setor 530010805180062 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180062", "530010805180189"),
  setor_2000   = "530010805180062",
  novo_codigo  = "5300108051800621"
)

# Unir setores de 2010 que formam o setor 530010805180063 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180063", "530010805180190", "530010805180191", "530010805180295"),
  setor_2000   = "530010805180063",
  novo_codigo  = "5300108051800631"
)

# Unir setores de 2010 que formam o setor 530010805180064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180064", "530010805180192"),
  setor_2000   = "530010805180064",
  novo_codigo  = "5300108051800641"
)

# Unir setores de 2010 que formam o setor 530010805180065 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180065", "530010805180193", "530010805180259"),
  setor_2000   = "530010805180065",
  novo_codigo  = "5300108051800651"
)

# Unir setores de 2010 que formam o setor 530010805180072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180072", "530010805180195"),
  setor_2000   = "530010805180072",
  novo_codigo  = "5300108051800721"
)

# Unir setores de 2010 que formam o setor 530010805180071 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180071", "530010805180194", "530010805180261"),
  setor_2000   = "530010805180071",
  novo_codigo  = "5300108051800711"
)

# Unir setores de 2010 que formam o setor 530010805180074 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180074", "530010805180262"),
  setor_2000   = "530010805180074",
  novo_codigo  = "5300108051800741"
)

# Unir setores de 2010 que formam o setor 530010805180075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180075", "530010805180263", "530010805180264"),
  setor_2000   = "530010805180075",
  novo_codigo  = "5300108051800751"
)

# Unir setores de 2010 que formam o setor 530010805180012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180012", "530010805180241"),
  setor_2000   = "530010805180012",
  novo_codigo  = "5300108051800121"
)

# Unir setores de 2010 que formam o setor 530010805180030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180030", "530010805180180"),
  setor_2000   = "530010805180030",
  novo_codigo  = "5300108051800301"
)

# Unir setores de 2010 que formam o setor 530010805180031 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180031", "530010805180181"),
  setor_2000   = "530010805180031",
  novo_codigo  = "5300108051800311"
)

# Unir setores de 2010 que formam o setor 530010805180035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180035", "530010805180249"),
  setor_2000   = "530010805180035",
  novo_codigo  = "5300108051800351"
)

# Unir setores de 2010 que formam o setor 530010805180164 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180164", "530010805180209"),
  setor_2000   = "530010805180164",
  novo_codigo  = "5300108051801641"
)

# Unir setores de 2010 que formam o setor 530010805180160 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180160", "530010805180208"),
  setor_2000   = "530010805180160",
  novo_codigo  = "5300108051801601"
)

# Unir setores de 2010 que formam o setor 530010805180159 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180159", "530010805180207"),
  setor_2000   = "530010805180159",
  novo_codigo  = "5300108051801591"
)

# Unir setores de 2010 que formam o setor 530010805180157 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180157", "530010805180280"),
  setor_2000   = "530010805180157",
  novo_codigo  = "5300108051801571"
)

# Unir setores de 2010 que formam o setor 530010805180156 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180156", "530010805180206", "530010805180299"),
  setor_2000   = "530010805180156",
  novo_codigo  = "5300108051801561"
)

# Unir setores de 2010 que formam o setor 530010805180155 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180155", "530010805180279"),
  setor_2000   = "530010805180155",
  novo_codigo  = "5300108051801551"
)

# Unir setores de 2010 que formam o setor 530010805180146 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180146", "530010805180278"),
  setor_2000   = "530010805180146",
  novo_codigo  = "5300108051801461"
)

# Unir setores de 2010 que formam o setor 530010805180144 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180144", "530010805180277"),
  setor_2000   = "530010805180144",
  novo_codigo  = "5300108051801441"
)

# Unir setores de 2010 que formam o setor 530010805180145 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180145", "530010805180205", "530010805180298"),
  setor_2000   = "530010805180145",
  novo_codigo  = "5300108051801451"
)

# Unir setores de 2010 que formam o setor 530010805180138 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180138", "530010805180204"),
  setor_2000   = "530010805180138",
  novo_codigo  = "5300108051801381"
)

# Unir setores de 2010 que formam o setor 530010805180137 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180137", "530010805180276"),
  setor_2000   = "530010805180137",
  novo_codigo  = "5300108051801371"
)

# Unir setores de 2010 que formam o setor 530010805180132 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180132", "530010805180203"),
  setor_2000   = "530010805180132",
  novo_codigo  = "5300108051801321"
)

# Unir setores de 2010 que formam o setor 530010805180129 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180129", "530010805180202", "530010805180297"),
  setor_2000   = "530010805180129",
  novo_codigo  = "5300108051801291"
)

# Unir setores de 2010 que formam o setor 530010805180122 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180122", "530010805180274"),
  setor_2000   = "530010805180122",
  novo_codigo  = "5300108051801221"
)

# Unir setores de 2010 que formam o setor 530010805180123 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180123", "530010805180275"),
  setor_2000   = "530010805180123",
  novo_codigo  = "5300108051801231"
)

# Unir setores de 2010 que formam o setor 530010805180113 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180113", "530010805180273"),
  setor_2000   = "530010805180113",
  novo_codigo  = "5300108051801131"
)

# Unir setores de 2010 que formam o setor 530010805180114 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180114", "530010805180201"),
  setor_2000   = "530010805180114",
  novo_codigo  = "5300108051801141"
)

# Unir setores de 2010 que formam o setor 530010805180108 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180108", "530010805180272"),
  setor_2000   = "530010805180108",
  novo_codigo  = "5300108051801081"
)

# Unir setores de 2010 que formam o setor 530010805180104 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180104", "530010805180200"),
  setor_2000   = "530010805180104",
  novo_codigo  = "5300108051801041"
)

# Unir setores de 2010 que formam o setor 530010805180100 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180100", "530010805180271"),
  setor_2000   = "530010805180100",
  novo_codigo  = "5300108051801001"
)

# Unir setores de 2010 que formam o setor 530010805180099 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180099", "530010805180270"),
  setor_2000   = "530010805180099",
  novo_codigo  = "5300108051800991"
)

# Unir setores de 2010 que formam o setor 530010805180096 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180096", "530010805180199", "530010805180296"),
  setor_2000   = "530010805180096",
  novo_codigo  = "5300108051800961"
)

# Unir setores de 2010 que formam o setor 530010805180097 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180097", "530010805180269"),
  setor_2000   = "530010805180097",
  novo_codigo  = "5300108051800971"
)

# Unir setores de 2010 que formam o setor 530010805180090 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180090", "530010805180268"),
  setor_2000   = "530010805180090",
  novo_codigo  = "5300108051800901"
)

# Unir setores de 2010 que formam o setor 530010805180089 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180089", "530010805180198"),
  setor_2000   = "530010805180089",
  novo_codigo  = "5300108051800891"
)

# Unir setores de 2010 que formam o setor 530010805180088 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180088", "530010805180267"),
  setor_2000   = "530010805180088",
  novo_codigo  = "5300108051800881"
)

# Unir setores de 2010 que formam o setor 530010805180082 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180082", "530010805180197", "530010805180266"),
  setor_2000   = "530010805180082",
  novo_codigo  = "5300108051800821"
)

# Unir setores de 2010 que formam o setor 530010805180081 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180081", "530010805180196"),
  setor_2000   = "530010805180081",
  novo_codigo  = "5300108051800811"
)

# Unir setores de 2010 que formam o setor 530010805180077 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805180077", "530010805180265"),
  setor_2000   = "530010805180077",
  novo_codigo  = "5300108051800771"
)

# Unir setores de 2010 que formam o setor 530010805130001 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130001", "530010805130058", "530010805130073"),
  setor_2000   = "530010805130001",
  novo_codigo  = "5300108051300011"
)

# Unir setores de 2010 que formam o setor 530010805130010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130063", "530010805130080", "530010805130010", "530010805130078"),
  setor_2000   = "530010805130010",
  novo_codigo  = "5300108051300101"
)

# Unir setores de 2010 que formam o setor 530010805130009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130009", "530010805130062", "530010805130077"),
  setor_2000   = "530010805130009",
  novo_codigo  = "5300108051300091"
)

# Unir setores de 2010 que formam o setor 530010805130008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130008", "530010805130061", "530010805130127"),
  setor_2000   = "530010805130008",
  novo_codigo  = "5300108051300081"
)

# Unir setores de 2010 que formam o setor 530010805130011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130064", "530010805130065", "530010805130128", "530010805130011"),
  setor_2000   = "530010805130011",
  novo_codigo  = "5300108051300111"
)

# Unir setores de 2010 que formam o setor 530010805130016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130016", "530010805130083", "530010805130084"),
  setor_2000   = "530010805130016",
  novo_codigo  = "5300108051300161"
)

# Unir setores de 2010 que formam o setor 530010805130017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130017", "530010805130085", "530010805130068", "530010805130129", "530010805130069", "530010805130130"),
  setor_2000   = "530010805130017",
  novo_codigo  = "5300108051300171"
)

# Unir setores de 2010 que formam o setor 530010805130018 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130018", "530010805130071", "530010805130070"),
  setor_2000   = "530010805130018",
  novo_codigo  = "5300108051300181"
)

# Unir setores de 2010 que formam o setor 530010805130020 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130020", "530010805130072"),
  setor_2000   = "530010805130020",
  novo_codigo  = "5300108051300201"
)

# Unir setores de 2010 que formam o setor 530010805130002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130002", "530010805130074", "530010805130075"),
  setor_2000   = "530010805130002",
  novo_codigo  = "5300108051300021"
)

# Unir setores de 2010 que formam o setor 530010805130004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130004", "530010805130076"),
  setor_2000   = "530010805130004",
  novo_codigo  = "5300108051300041"
)

# Unir setores de 2010 que formam o setor 530010805130007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130007", "530010805130060"),
  setor_2000   = "530010805130007",
  novo_codigo  = "5300108051300071"
)

# Unir setores de 2010 que formam o setor 530010805130006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130006", "530010805130059"),
  setor_2000   = "530010805130006",
  novo_codigo  = "5300108051300061"
)

# Unir setores de 2010 que formam o setor 530010805130012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130012", "530010805130066"),
  setor_2000   = "530010805130012",
  novo_codigo  = "5300108051300121"
)

# Unir setores de 2010 que formam o setor 530010805130013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130013", "530010805130081"),
  setor_2000   = "530010805130013",
  novo_codigo  = "5300108051300131"
)

# Unir setores de 2010 que formam o setor 530010805130014 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130014", "530010805130082"),
  setor_2000   = "530010805130014",
  novo_codigo  = "5300108051300141"
)

# Unir setores de 2010 que formam o setor 530010805130015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130015", "530010805130067"),
  setor_2000   = "530010805130015",
  novo_codigo  = "5300108051300151"
)

# Unir setores de 2010 que formam o setor 530010805140027 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140027", "530010805140062"),
  setor_2000   = "530010805140027",
  novo_codigo  = "5300108051400271"
)

# Unir setores de 2010 que formam o setor 530010805140026 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140026", "530010805140061"),
  setor_2000   = "530010805140026",
  novo_codigo  = "5300108051400261"
)

# Unir setores de 2010 que formam o setor 530010805140025 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140025", "530010805140052"),
  setor_2000   = "530010805140025",
  novo_codigo  = "5300108051400251"
)

# Unir setores de 2010 que formam o setor 530010805140024 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140024", "530010805140051", "530010805140059", "530010805140060"),
  setor_2000   = "530010805140024",
  novo_codigo  = "5300108051400241"
)

# Unir setores de 2010 que formam o setor 530010805140010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140010", "530010805140055"),
  setor_2000   = "530010805140010",
  novo_codigo  = "5300108051400101"
)

# Unir setores de 2010 que formam o setor 530010805140009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140009", "530010805140045", "530010805140046"),
  setor_2000   = "530010805140009",
  novo_codigo  = "5300108051400091"
)

# Unir setores de 2010 que formam o setor 530010805140005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140005", "530010805140043"),
  setor_2000   = "530010805140005",
  novo_codigo  = "5300108051400051"
)

# Unir setores de 2010 que formam o setor 530010805140006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140006", "530010805140044"),
  setor_2000   = "530010805140006",
  novo_codigo  = "5300108051400061"
)

# Unir setores de 2010 que formam o setor 530010805140014 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140014", "530010805140048"),
  setor_2000   = "530010805140014",
  novo_codigo  = "5300108051400141"
)

# Unir setores de 2010 que formam o setor 530010805140017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140017", "530010805140049"),
  setor_2000   = "530010805140017",
  novo_codigo  = "5300108051400171"
)

# Unir setores de 2010 que formam o setor 530010805140012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140012", "530010805140047"),
  setor_2000   = "530010805140012",
  novo_codigo  = "5300108051400121"
)

# Unir setores de 2010 que formam o setor 530010805140022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140022", "530010805140050", "530010805140058"),
  setor_2000   = "530010805140022",
  novo_codigo  = "5300108051400221"
)

# Unir setores de 2010 que formam o setor 530010805190004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805190004", "530010805190021"),
  setor_2000   = "530010805190004",
  novo_codigo  = "5300108051900041"
)

# Unir setores de 2010 que formam o setor 530010805190015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805190015", "530010805190022"),
  setor_2000   = "530010805190015",
  novo_codigo  = "5300108051900151"
)

# Unir setores de 2010 que formam o setor 530010805190001 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805190001", "530010805190020"),
  setor_2000   = "530010805190001",
  novo_codigo  = "5300108051900011"
)

# Unir setores de 2010 que formam o setor 530010805140004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805140004", "530010805140054"),
  setor_2000   = "530010805140004",
  novo_codigo  = "5300108051400041"
)

# Unir setores de 2010 que formam o setor 530010805160108 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160108", "530010805160191"),
  setor_2000   = "530010805160108",
  novo_codigo  = "5300108051601081"
)

# Unir setores de 2010 que formam o setor 530010805160085 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160213", "530010805160166", "530010805160212", "530010805160165", "530010805160164", "530010805160210", "530010805160188", "530010805160211", "530010805160185", "530010805160186", "530010805160187", "530010805160085"),
  setor_2000   = "530010805160085",
  novo_codigo  = "5300108051600851"
)

# Unir setores de 2010 que formam o setor 530010805160114 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160114", "530010805160196"),
  setor_2000   = "530010805160114",
  novo_codigo  = "5300108051601141"
)

# Unir setores de 2010 que formam o setor 530010805160105 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160105", "530010805160170"),
  setor_2000   = "530010805160105",
  novo_codigo  = "5300108051601051"
)

# Unir setores de 2010 que formam o setor 530010805160107 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160107", "530010805160172"),
  setor_2000   = "530010805160107",
  novo_codigo  = "5300108051601071"
)

# Unir setores de 2010 que formam o setor 530010805160106 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160106", "530010805160171", "530010805160190"),
  setor_2000   = "530010805160106",
  novo_codigo  = "5300108051601061"
)

# Unir setores de 2010 que formam o setor 530010805160104 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160104", "530010805160169", "530010805160189"),
  setor_2000   = "530010805160104",
  novo_codigo  = "5300108051601041"
)

# Unir setores de 2010 que formam o setor 530010805160101 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160101", "530010805160167"),
  setor_2000   = "530010805160101",
  novo_codigo  = "5300108051601011"
)

# Unir setores de 2010 que formam o setor 530010805160103 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160103", "530010805160216", "530010805160168"),
  setor_2000   = "530010805160103",
  novo_codigo  = "5300108051601031"
)

# Unir setores de 2010 que formam o setor 530010805160082 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160082", "530010805160163"),
  setor_2000   = "530010805160082",
  novo_codigo  = "5300108051600821"
)

# Unir setores de 2010 que formam o setor 530010805160080 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160080", "530010805160162"),
  setor_2000   = "530010805160080",
  novo_codigo  = "5300108051600801"
)

# Unir setores de 2010 que formam o setor 530010805160077 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160077", "530010805160160"),
  setor_2000   = "530010805160077",
  novo_codigo  = "5300108051600771"
)

# Unir setores de 2010 que formam o setor 530010805160076 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160076", "530010805160157", "530010805160209", "530010805160158", "530010805160159"),
  setor_2000   = "530010805160076",
  novo_codigo  = "5300108051600761"
)

# Unir setores de 2010 que formam o setor 530010805160064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160064", "530010805160155"),
  setor_2000   = "530010805160064",
  novo_codigo  = "5300108051600641"
)

# Unir setores de 2010 que formam o setor 530010805160070 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160070", "530010805160184"),
  setor_2000   = "530010805160070",
  novo_codigo  = "5300108051600701"
)

# Unir setores de 2010 que formam o setor 530010805160060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160060", "530010805160154"),
  setor_2000   = "530010805160060",
  novo_codigo  = "5300108051600601"
)

# Unir setores de 2010 que formam o setor 530010805160053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160153", "530010805160181", "530010805160053"),
  setor_2000   = "530010805160053",
  novo_codigo  = "5300108051600531"
)

# Unir setores de 2010 que formam o setor 530010805160061 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160061", "530010805160183"),
  setor_2000   = "530010805160061",
  novo_codigo  = "5300108051600611"
)

# Unir setores de 2010 que formam o setor 530010805160075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160075", "530010805160156"),
  setor_2000   = "530010805160075",
  novo_codigo  = "5300108051600751"
)

# Unir setores de 2010 que formam o setor 530010805160051 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160214", "530010805160180", "530010805160051"),
  setor_2000   = "530010805160051",
  novo_codigo  = "5300108051600511"
)

# Unir setores de 2010 que formam o setor 530010805160109 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160182", "530010805160219", "530010805160109", "530010805160221"),
  setor_2000   = "530010805160109",
  novo_codigo  = "5300108051601091"
)

# Unir setores de 2010 que formam o setor 530010805160129 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160129", "530010805160200"),
  setor_2000   = "530010805160129",
  novo_codigo  = "5300108051601291"
)

# Unir setores de 2010 que formam o setor 530010805160032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160032", "530010805160177"),
  setor_2000   = "530010805160032",
  novo_codigo  = "5300108051600321"
)

# Unir setores de 2010 que formam o setor 530010805160034 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160034", "530010805160178"),
  setor_2000   = "530010805160034",
  novo_codigo  = "5300108051600341"
)

# Unir setores de 2010 que formam o setor 530010805160035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160035", "530010805160145"),
  setor_2000   = "530010805160035",
  novo_codigo  = "5300108051600351"
)

# Unir setores de 2010 que formam o setor 530010805160037 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160037", "530010805160146"),
  setor_2000   = "530010805160037",
  novo_codigo  = "5300108051600371"
)

# Unir setores de 2010 que formam o setor 530010805160039 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160039", "530010805160148"),
  setor_2000   = "530010805160039",
  novo_codigo  = "5300108051600391"
)

# Unir setores de 2010 que formam o setor 530010805160040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160040", "530010805160149"),
  setor_2000   = "530010805160040",
  novo_codigo  = "5300108051600401"
)

# Unir setores de 2010 que formam o setor 530010805160038 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160038", "530010805160147"),
  setor_2000   = "530010805160038",
  novo_codigo  = "5300108051600381"
)

# Unir setores de 2010 que formam o setor 530010805160018 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160018", "530010805160144"),
  setor_2000   = "530010805160018",
  novo_codigo  = "5300108051600181"
)

# Unir setores de 2010 que formam o setor 530010805160005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160005", "530010805160142"),
  setor_2000   = "530010805160005",
  novo_codigo  = "5300108051600051"
)

# Unir setores de 2010 que formam o setor 530010805160006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160006", "530010805160143"),
  setor_2000   = "530010805160006",
  novo_codigo  = "5300108051600061"
)

# Unir setores de 2010 que formam o setor 530010805160007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160007", "530010805160175"),
  setor_2000   = "530010805160007",
  novo_codigo  = "5300108051600071"
)

# Unir setores de 2010 que formam o setor 530010805160022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160022", "530010805160176"),
  setor_2000   = "530010805160022",
  novo_codigo  = "5300108051600221"
)

# Unir setores de 2010 que formam o setor 530010805160046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160046", "530010805160179"),
  setor_2000   = "530010805160046",
  novo_codigo  = "5300108051600461"
)

# Unir setores de 2010 que formam o setor 530010805160045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160045", "530010805160152"),
  setor_2000   = "530010805160045",
  novo_codigo  = "5300108051600451"
)

# Unir setores de 2010 que formam o setor 530010805160044 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160044", "530010805160151"),
  setor_2000   = "530010805160044",
  novo_codigo  = "5300108051600441"
)

# Unir setores de 2010 que formam o setor 530010805160043 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160043", "530010805160150"),
  setor_2000   = "530010805160043",
  novo_codigo  = "5300108051600431"
)

# Unir setores de 2010 que formam o setor 530010805160119 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160119", "530010805160198"),
  setor_2000   = "530010805160119",
  novo_codigo  = "5300108051601191"
)

# Unir setores de 2010 que formam o setor 530010805160118 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160118", "530010805160197"),
  setor_2000   = "530010805160118",
  novo_codigo  = "5300108051601181"
)

# Unir setores de 2010 que formam o setor 530010805160130 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160201", "530010805160202", "530010805160130"),
  setor_2000   = "530010805160130",
  novo_codigo  = "5300108051601301"
)

# Unir setores de 2010 que formam o setor 530010805160121 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805160121", "530010805160199"),
  setor_2000   = "530010805160121",
  novo_codigo  = "5300108051601211"
)

# Unir setores de 2010 que formam o setor 530010805150386 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150455", "530010805150554", "530010805150471", "530010805150473", "530010805150472", "530010805150476", "530010805150477", "530010805150474", "530010805150475", "530010805150555", "530010805150483", "530010805150481", "530010805150482", "530010805150479", "530010805150480", "530010805150478", "530010805150580", "530010805150556", "530010805150557", "530010805150558", "530010805150559", "530010805150560", "530010805150561", "530010805150562", "530010805150563", "530010805150564", "530010805150565", "530010805150566", "530010805150567", "530010805150568", "530010805150570", "530010805150571", "530010805150572", "530010805150573", "530010805150575", "530010805150574", "530010805150576", "530010805150577", "530010805150578", "530010805150579", "530010805150581", "530010805150582", "530010805150576", "530010805150569", "530010805150583", "530010805150585", "530010805150586", "530010805150584", "530010805150587", "530010805150588", "530010805150590", "530010805150591", "530010805150592", "530010805150593", "530010805150594", "530010805150595", "530010805150599", "530010805150600", "530010805150596", "530010805150597", "530010805150598", "530010805150589", "530010805150536", "530010805150602", "530010805150603", "530010805150604", "530010805150601", "530010805150605", "530010805150550"),
  setor_2000   = "530010805150386",
  novo_codigo  = "5300108051503861"
)

# Unir setores de 2010 que formam o setor 530010805150387 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150529", "530010805150387", "530010805150528", "530010805150533", "530010805150534", "530010805150526", "530010805150531", "530010805150525", "530010805150527", "530010805150532", "530010805150523", "530010805150524", "530010805150530", "530010805150535"),
  setor_2000   = "530010805150387",
  novo_codigo  = "5300108051503871"
)

# Unir setores de 2010 que formam o setor 530010805130053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130053", "530010805130122"),
  setor_2000   = "530010805130053",
  novo_codigo  = "5300108051300531"
)

# Unir setores de 2010 que formam o setor 530010805130052 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130120", "530010805130121", "530010805130052", "530010805130119"),
  setor_2000   = "530010805130052",
  novo_codigo  = "5300108051300521"
)

# Unir setores de 2010 que formam o setor 530010805130047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130047", "530010805130115", "530010805130116"),
  setor_2000   = "530010805130047",
  novo_codigo  = "5300108051300471"
)

# Unir setores de 2010 que formam o setor 530010805130039 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130039", "530010805130111", "530010805130112"),
  setor_2000   = "530010805130039",
  novo_codigo  = "5300108051300391"
)

# Unir setores de 2010 que formam o setor 530010805130033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130033", "530010805130109"),
  setor_2000   = "530010805130033",
  novo_codigo  = "5300108051300331"
)

# Unir setores de 2010 que formam o setor 530010805130036 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130036", "530010805130110"),
  setor_2000   = "530010805130036",
  novo_codigo  = "5300108051300361"
)

# Unir setores de 2010 que formam o setor 530010805130044 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130044", "530010805130114"),
  setor_2000   = "530010805130044",
  novo_codigo  = "5300108051300441"
)

# Unir setores de 2010 que formam o setor 530010805130041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130041", "530010805130113"),
  setor_2000   = "530010805130041",
  novo_codigo  = "5300108051300411"
)

# Unir setores de 2010 que formam o setor 530010805130049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130049", "530010805130118"),
  setor_2000   = "530010805130049",
  novo_codigo  = "5300108051300491"
)

# Unir setores de 2010 que formam o setor 530010805130048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805130048", "530010805130117"),
  setor_2000   = "530010805130048",
  novo_codigo  = "5300108051300481"
)

# Unir setores de 2010 que formam o setor 530010805200019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200019", "530010805200136"),
  setor_2000   = "530010805200019",
  novo_codigo  = "5300108052000191"
)

# Unir setores de 2010 que formam o setor 530010805200020 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200020", "530010805200137"),
  setor_2000   = "530010805200020",
  novo_codigo  = "5300108052000201"
)

# Unir setores de 2010 que formam o setor 530010805200021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200021", "530010805200138"),
  setor_2000   = "530010805200021",
  novo_codigo  = "5300108052000211"
)

# Unir setores de 2010 que formam o setor 530010805200091 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200091", "530010805200171"),
  setor_2000   = "530010805200091",
  novo_codigo  = "5300108052000911"
)

# Unir setores de 2010 que formam o setor 530010805200090 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200090", "530010805200170"),
  setor_2000   = "530010805200090",
  novo_codigo  = "5300108052000901"
)

# Unir setores de 2010 que formam o setor 530010805200088 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200088", "530010805200169"),
  setor_2000   = "530010805200088",
  novo_codigo  = "5300108052000881"
)

# Unir setores de 2010 que formam o setor 530010805200087 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200087", "530010805200168"),
  setor_2000   = "530010805200087",
  novo_codigo  = "5300108052000871"
)

# Unir setores de 2010 que formam o setor 530010805200084 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200084", "530010805200167"),
  setor_2000   = "530010805200084",
  novo_codigo  = "5300108052000841"
)

# Unir setores de 2010 que formam o setor 530010805200022 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200022", "530010805200102"),
  setor_2000   = "530010805200022",
  novo_codigo  = "5300108052000221"
)

# Unir setores de 2010 que formam o setor 530010805200024 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200024", "530010805200104"),
  setor_2000   = "530010805200024",
  novo_codigo  = "5300108052000241"
)

# Unir setores de 2010 que formam o setor 530010805200023 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200023", "530010805200103"),
  setor_2000   = "530010805200023",
  novo_codigo  = "5300108052000231"
)

# Unir setores de 2010 que formam o setor 530010805200016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200016", "530010805200079"),
  setor_2000   = "530010805200016",
  novo_codigo  = "5300108052000161"
)

# Unir setores de 2010 que formam o setor 530010805200015 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200015", "530010805200135"),
  setor_2000   = "530010805200015",
  novo_codigo  = "5300108052000151"
)

# Unir setores de 2010 que formam o setor 530010805200025 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200025", "530010805200105"),
  setor_2000   = "530010805200025",
  novo_codigo  = "5300108052000251"
)

# Unir setores de 2010 que formam o setor 530010805200026 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200026", "530010805200106"),
  setor_2000   = "530010805200026",
  novo_codigo  = "5300108052000261"
)

# Unir setores de 2010 que formam o setor 530010805200014 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200014", "530010805200134"),
  setor_2000   = "530010805200014",
  novo_codigo  = "5300108052000141"
)

# Unir setores de 2010 que formam o setor 530010805200013 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200013", "530010805200133"),
  setor_2000   = "530010805200013",
  novo_codigo  = "5300108052000131"
)

# Unir setores de 2010 que formam o setor 530010805200027 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200027", "530010805200107"),
  setor_2000   = "530010805200027",
  novo_codigo  = "5300108052000271"
)

# Unir setores de 2010 que formam o setor 530010805200012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200012", "530010805200132"),
  setor_2000   = "530010805200012",
  novo_codigo  = "5300108052000121"
)

# Unir setores de 2010 que formam o setor 530010805200028 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200028", "530010805200139"),
  setor_2000   = "530010805200028",
  novo_codigo  = "5300108052000281"
)

# Unir setores de 2010 que formam o setor 530010805200011 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200011", "530010805200078"),
  setor_2000   = "530010805200011",
  novo_codigo  = "5300108052000111"
)

# Unir setores de 2010 que formam o setor 530010805200029 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200029", "530010805200108"),
  setor_2000   = "530010805200029",
  novo_codigo  = "5300108052000291"
)

# Unir setores de 2010 que formam o setor 530010805200010 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200010", "530010805200131"),
  setor_2000   = "530010805200010",
  novo_codigo  = "5300108052000101"
)

# Unir setores de 2010 que formam o setor 530010805200030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200030", "530010805200140"),
  setor_2000   = "530010805200030",
  novo_codigo  = "5300108052000301"
)

# Unir setores de 2010 que formam o setor 530010805200009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200009", "530010805200077"),
  setor_2000   = "530010805200009",
  novo_codigo  = "5300108052000091"
)

# Unir setores de 2010 que formam o setor 530010805200031 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200031", "530010805200109"),
  setor_2000   = "530010805200031",
  novo_codigo  = "5300108052000311"
)

# Unir setores de 2010 que formam o setor 530010805200008 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200008", "530010805200130"),
  setor_2000   = "530010805200008",
  novo_codigo  = "5300108052000081"
)

# Unir setores de 2010 que formam o setor 530010805200032 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200032", "530010805200141"),
  setor_2000   = "530010805200032",
  novo_codigo  = "5300108052000321"
)

# Unir setores de 2010 que formam o setor 530010805200033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200033", "530010805200110"),
  setor_2000   = "530010805200033",
  novo_codigo  = "5300108052000331"
)

# Unir setores de 2010 que formam o setor 530010805200034 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200142", "530010805200034", "530010805200143", "530010805200144"),
  setor_2000   = "530010805200034",
  novo_codigo  = "5300108052000341"
)

# Unir setores de 2010 que formam o setor 530010805200033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200033", "530010805200110"),
  setor_2000   = "530010805200033",
  novo_codigo  = "5300108052000331"
)

# Unir setores de 2010 que formam o setor 530010805200007 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200007", "530010805200129"),
  setor_2000   = "530010805200007",
  novo_codigo  = "5300108052000071"
)

# Unir setores de 2010 que formam o setor 530010805200006 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200006", "530010805200128"),
  setor_2000   = "530010805200006",
  novo_codigo  = "5300108052000061"
)

# Unir setores de 2010 que formam o setor 530010805200004 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200004", "530010805200127"),
  setor_2000   = "530010805200004",
  novo_codigo  = "5300108052000041"
)

# Unir setores de 2010 que formam o setor 530010805200003 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200003", "530010805200177"),
  setor_2000   = "530010805200003",
  novo_codigo  = "5300108052000031"
)

# Unir setores de 2010 que formam o setor 530010805200002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200002", "530010805200076"),
  setor_2000   = "530010805200002",
  novo_codigo  = "5300108052000021"
)

# Unir setores de 2010 que formam o setor 530010805200001 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200001", "530010805200074"),
  setor_2000   = "530010805200001",
  novo_codigo  = "5300108052000011"
)

# Unir setores de 2010 que formam o setor 530010805200035 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200145", "530010805200035"),
  setor_2000   = "530010805200035",
  novo_codigo  = "5300108052000351"
)

# Unir setores de 2010 que formam o setor 530010805200037 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200147", "530010805200037"),
  setor_2000   = "530010805200037",
  novo_codigo  = "5300108052000371"
)

# Unir setores de 2010 que formam o setor 530010805200038 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200038", "530010805200111"),
  setor_2000   = "530010805200038",
  novo_codigo  = "5300108052000381"
)

# Unir setores de 2010 que formam o setor 530010805200039 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200039", "530010805200112"),
  setor_2000   = "530010805200039",
  novo_codigo  = "5300108052000391"
)

# Unir setores de 2010 que formam o setor 530010805200040 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200148", "530010805200040"),
  setor_2000   = "530010805200040",
  novo_codigo  = "5300108052000401"
)

# Unir setores de 2010 que formam o setor 530010805200041 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200149", "530010805200041", "530010805200150"),
  setor_2000   = "530010805200041",
  novo_codigo  = "5300108052000411"
)

# Unir setores de 2010 que formam o setor 530010805200042 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200151", "530010805200042"),
  setor_2000   = "530010805200042",
  novo_codigo  = "5300108052000421"
)

# Unir setores de 2010 que formam o setor 530010805200050 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200050", "530010805200158"),
  setor_2000   = "530010805200050",
  novo_codigo  = "5300108052000501"
)

# Unir setores de 2010 que formam o setor 530010805200049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200049", "530010805200157"),
  setor_2000   = "530010805200049",
  novo_codigo  = "5300108052000491"
)

# Unir setores de 2010 que formam o setor 530010805200045 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200152", "530010805200045"),
  setor_2000   = "530010805200045",
  novo_codigo  = "5300108052000451"
)

# Unir setores de 2010 que formam o setor 530010805200072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200072", "530010805200166"),
  setor_2000   = "530010805200072",
  novo_codigo  = "5300108052000721"
)

# Unir setores de 2010 que formam o setor 530010805200048 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200155", "530010805200048", "530010805200156"),
  setor_2000   = "530010805200048",
  novo_codigo  = "5300108052000481"
)

# Unir setores de 2010 que formam o setor 530010805200047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200154", "530010805200047"),
  setor_2000   = "530010805200047",
  novo_codigo  = "5300108052000471"
)

# Unir setores de 2010 que formam o setor 530010805200059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200059", "530010805200163"),
  setor_2000   = "530010805200059",
  novo_codigo  = "5300108052000591"
)

# Unir setores de 2010 que formam o setor 530010805200057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200057", "530010805200161"),
  setor_2000   = "530010805200057",
  novo_codigo  = "5300108052000571"
)

# Unir setores de 2010 que formam o setor 530010805200056 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200056", "530010805200160"),
  setor_2000   = "530010805200056",
  novo_codigo  = "5300108052000561"
)

# Unir setores de 2010 que formam o setor 530010805200058 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200058", "530010805200162"),
  setor_2000   = "530010805200058",
  novo_codigo  = "5300108052000581"
)

# Unir setores de 2010 que formam o setor 530010805200100 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200100", "530010805200116"),
  setor_2000   = "530010805200100",
  novo_codigo  = "5300108052001001"
)

# Unir setores de 2010 que formam o setor 530010805200098 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200098", "530010805200175"),
  setor_2000   = "530010805200098",
  novo_codigo  = "5300108052000981"
)

# Unir setores de 2010 que formam o setor 530010805200092 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200092", "530010805200172"),
  setor_2000   = "530010805200092",
  novo_codigo  = "5300108052000921"
)

# Unir setores de 2010 que formam o setor 530010805200101 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200101", "530010805200117"),
  setor_2000   = "530010805200101",
  novo_codigo  = "5300108052001011"
)

# Unir setores de 2010 que formam o setor 530010805200097 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200097", "530010805200115"),
  setor_2000   = "530010805200097",
  novo_codigo  = "5300108052000971"
)

# Unir setores de 2010 que formam o setor 530010805200093 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200093", "530010805200173"),
  setor_2000   = "530010805200093",
  novo_codigo  = "5300108052000931"
)

# Unir setores de 2010 que formam o setor 530010805200096 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200096", "530010805200114"),
  setor_2000   = "530010805200096",
  novo_codigo  = "5300108052000961"
)

# Unir setores de 2010 que formam o setor 530010805200094 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200094", "530010805200113"),
  setor_2000   = "530010805200094",
  novo_codigo  = "5300108052000941"
)

# Unir setores de 2010 que formam o setor 530010805200095 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200095", "530010805200174"),
  setor_2000   = "530010805200095",
  novo_codigo  = "5300108052000951"
)

# Unir setores de 2010 que formam o setor 530010805200060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200060", "530010805200164"),
  setor_2000   = "530010805200060",
  novo_codigo  = "5300108052000601"
)

# Unir setores de 2010 que formam o setor 530010805200054 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200054", "530010805200159"),
  setor_2000   = "530010805200054",
  novo_codigo  = "5300108052000541"
)

# Unir setores de 2010 que formam o setor 530010805200066 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200066", "530010805200165"),
  setor_2000   = "530010805200066",
  novo_codigo  = "5300108052000661"
)

# Unir setores de 2010 que formam o setor 530010805080180 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080180", "530010805080636"),
  setor_2000   = "530010805080180",
  novo_codigo  = "5300108050801801"
)

# Unir setores de 2010 que formam o setor 530010805080173 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080173", "530010805080633"),
  setor_2000   = "530010805080173",
  novo_codigo  = "5300108050801731"
)

# Unir setores de 2010 que formam o setor 530010805080174 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080174", "530010805080634"),
  setor_2000   = "530010805080174",
  novo_codigo  = "5300108050801741"
)

# Unir setores de 2010 que formam o setor 530010805080181 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080181", "530010805080637", "530010805080640", "530010805080638", "530010805080639", "530010805080641"),
  setor_2000   = "530010805080181",
  novo_codigo  = "5300108050801811"
)

# Unir setores de 2010 que formam o setor 530010805080171 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080171", "530010805080631", "530010805080632"),
  setor_2000   = "530010805080171",
  novo_codigo  = "5300108050801711"
)

# Unir setores de 2010 que formam o setor 530010805080182 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080182", "530010805080642"),
  setor_2000   = "530010805080182",
  novo_codigo  = "5300108050801821"
)

# Unir setores de 2010 que formam o setor 530010805080166 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080166", "530010805080630"),
  setor_2000   = "530010805080166",
  novo_codigo  = "5300108050801661"
)

# Unir setores de 2010 que formam o setor 530010805080162 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080162", "530010805080629"),
  setor_2000   = "530010805080162",
  novo_codigo  = "5300108050801621"
)

# Unir setores de 2010 que formam o setor 530010805080156 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080156", "530010805080628"),
  setor_2000   = "530010805080156",
  novo_codigo  = "5300108050801561"
)

# Unir setores de 2010 que formam o setor 530010805080143 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080143", "530010805080621"),
  setor_2000   = "530010805080143",
  novo_codigo  = "5300108050801431"
)

# Unir setores de 2010 que formam o setor 530010805080144 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080144", "530010805080622"),
  setor_2000   = "530010805080144",
  novo_codigo  = "5300108050801441"
)

# Unir setores de 2010 que formam o setor 530010805080145 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080145", "530010805080623"),
  setor_2000   = "530010805080145",
  novo_codigo  = "5300108050801451"
)

# Unir setores de 2010 que formam o setor 530010805080149 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080149", "530010805080626"),
  setor_2000   = "530010805080149",
  novo_codigo  = "5300108050801491"
)

# Unir setores de 2010 que formam o setor 530010805080148 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080148", "530010805080625"),
  setor_2000   = "530010805080148",
  novo_codigo  = "5300108050801481"
)

# Unir setores de 2010 que formam o setor 530010805080128 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080128", "530010805080618", "530010805080617"),
  setor_2000   = "530010805080128",
  novo_codigo  = "5300108050801281"
)

# Unir setores de 2010 que formam o setor 530010805080106 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080106", "530010805080607", "530010805080608"),
  setor_2000   = "530010805080106",
  novo_codigo  = "5300108050801061"
)

# Unir setores de 2010 que formam o setor 530010805080051 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080051", "530010805080583", "530010805080584", "530010805080051"),
  setor_2000   = "530010805080051",
  novo_codigo  = "5300108050800511"
)

# Unir setores de 2010 que formam o setor 530010805080050 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080050", "530010805080582"),
  setor_2000   = "530010805080050",
  novo_codigo  = "5300108050800501"
)

# Unir setores de 2010 que formam o setor 530010805080142 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080142", "530010805080620"),
  setor_2000   = "530010805080142",
  novo_codigo  = "5300108050801421"
)

# Unir setores de 2010 que formam o setor 530010805080141 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080141", "530010805080619"),
  setor_2000   = "530010805080141",
  novo_codigo  = "5300108050801411"
)

# Unir setores de 2010 que formam o setor 530010805080121 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080121", "530010805080613"),
  setor_2000   = "530010805080121",
  novo_codigo  = "5300108050801211"
)

# Unir setores de 2010 que formam o setor 530010805080146 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080146", "530010805080624"),
  setor_2000   = "530010805080146",
  novo_codigo  = "5300108050801461"
)

# Unir setores de 2010 que formam o setor 530010805080057 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080057", "530010805080589"),
  setor_2000   = "530010805080057",
  novo_codigo  = "5300108050800571"
)

# Unir setores de 2010 que formam o setor 530010805080062 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080062", "530010805080591"),
  setor_2000   = "530010805080062",
  novo_codigo  = "5300108050800621"
)

# Unir setores de 2010 que formam o setor 530010805080060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080060", "530010805080590"),
  setor_2000   = "530010805080060",
  novo_codigo  = "5300108050800601"
)

# Unir setores de 2010 que formam o setor 530010805080085 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080085", "530010805080597"),
  setor_2000   = "530010805080085",
  novo_codigo  = "5300108050800851"
)

# Unir setores de 2010 que formam o setor 530010805080097 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080097", "530010805080603"),
  setor_2000   = "530010805080097",
  novo_codigo  = "5300108050800971"
)

# Unir setores de 2010 que formam o setor 530010805080098 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080604", "530010805080605", "530010805080098"),
  setor_2000   = "530010805080098",
  novo_codigo  = "5300108050800981"
)

# Unir setores de 2010 que formam o setor 530010805080115 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080115", "530010805080611"),
  setor_2000   = "530010805080115",
  novo_codigo  = "5300108050801151"
)

# Unir setores de 2010 que formam o setor 530010805080072 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080072", "530010805080595"),
  setor_2000   = "530010805080072",
  novo_codigo  = "5300108050800721"
)

# Unir setores de 2010 que formam o setor 530010805080089 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080089", "530010805080598", "530010805080599"),
  setor_2000   = "530010805080089",
  novo_codigo  = "5300108050800891"
)

# Unir setores de 2010 que formam o setor 530010805080092 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080092", "530010805080601"),
  setor_2000   = "530010805080092",
  novo_codigo  = "5300108050800921"
)

# Unir setores de 2010 que formam o setor 530010805080095 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080095", "530010805080602"),
  setor_2000   = "530010805080095",
  novo_codigo  = "5300108050800951"
)

# Unir setores de 2010 que formam o setor 530010805080123 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080123", "530010805080612"),
  setor_2000   = "530010805080123",
  novo_codigo  = "5300108050801231"
)

# Unir setores de 2010 que formam o setor 530010805080124 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080124", "530010805080614"),
  setor_2000   = "530010805080124",
  novo_codigo  = "5300108050801241"
)

# Unir setores de 2010 que formam o setor 530010805080125 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080125", "530010805080615", "530010805080616"),
  setor_2000   = "530010805080125",
  novo_codigo  = "5300108050801251"
)

# Unir setores de 2010 que formam o setor 530010805080107 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080107", "530010805080609", "530010805080610"),
  setor_2000   = "530010805080107",
  novo_codigo  = "5300108050801071"
)

# Unir setores de 2010 que formam o setor 530010805080091 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080091", "530010805080600"),
  setor_2000   = "530010805080091",
  novo_codigo  = "5300108050800911"
)

# Unir setores de 2010 que formam o setor 530010805080075 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080075", "530010805080716", "530010805080717"),
  setor_2000   = "530010805080075",
  novo_codigo  = "5300108050800751"
)

# Unir setores de 2010 que formam o setor 530010805080063 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080063", "530010805080592"),
  setor_2000   = "530010805080063",
  novo_codigo  = "5300108050800631"
)

# Unir setores de 2010 que formam o setor 530010805080055 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080055", "530010805080588"),
  setor_2000   = "530010805080055",
  novo_codigo  = "5300108050800551"
)

# Unir setores de 2010 que formam o setor 530010805080076 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080076", "530010805080596"),
  setor_2000   = "530010805080076",
  novo_codigo  = "5300108050800761"
)

# Unir setores de 2010 que formam o setor 530010805080066 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080066", "530010805080594"),
  setor_2000   = "530010805080066",
  novo_codigo  = "5300108050800661"
)

# Unir setores de 2010 que formam o setor 530010805080064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080064", "530010805080593"),
  setor_2000   = "530010805080064",
  novo_codigo  = "5300108050800641"
)

# Unir setores de 2010 que formam o setor 530010805080053 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080053", "530010805080587"),
  setor_2000   = "530010805080053",
  novo_codigo  = "5300108050800531"
)

# Unir setores de 2010 que formam o setor 530010805080052 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080052", "530010805080586"),
  setor_2000   = "530010805080052",
  novo_codigo  = "5300108050800521"
)

# Unir setores de 2010 que formam o setor 530010805080049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080049", "530010805080581"),
  setor_2000   = "530010805080049",
  novo_codigo  = "5300108050800491"
)

# Unir setores de 2010 que formam o setor 530010805080047 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080047", "530010805080580"),
  setor_2000   = "530010805080047",
  novo_codigo  = "5300108050800471"
)

# Unir setores de 2010 que formam o setor 530010805080002 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080002", "530010805080713", "530010805080714", "530010805080715"),
  setor_2000   = "530010805080002",
  novo_codigo  = "5300108050800021"
)

# Unir setores de 2010 que formam o setor 530010805080005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080005", "530010805080556"),
  setor_2000   = "530010805080005",
  novo_codigo  = "5300108050800051"
)

# Unir setores de 2010 que formam o setor 530010805080019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080019", "530010805080493"),
  setor_2000   = "530010805080019",
  novo_codigo  = "5300108050800191"
)

# Unir setores de 2010 que formam o setor 530010805080023 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080023", "530010805080494"),
  setor_2000   = "530010805080023",
  novo_codigo  = "5300108050800231"
)

# Unir setores de 2010 que formam o setor 530010805080020 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080020", "530010805080740"),
  setor_2000   = "530010805080020",
  novo_codigo  = "5300108050800201"
)

# Unir setores de 2010 que formam o setor 530010805080021 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080021", "530010805080562"),
  setor_2000   = "530010805080021",
  novo_codigo  = "5300108050800211"
)

# Unir setores de 2010 que formam o setor 530010805080189 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080189", "530010805080643"),
  setor_2000   = "530010805080189",
  novo_codigo  = "5300108050801891"
)

# Unir setores de 2010 que formam o setor 530010805080195 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080195", "530010805080498"),
  setor_2000   = "530010805080195",
  novo_codigo  = "5300108050801951"
)

# Unir setores de 2010 que formam o setor 530010805080464 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080464", "530010805080690", "530010805080465", "530010805080534", "530010805080535", "530010805080555", "530010805080536", "530010805080537", "530010805080538", "530010805080561", "530010805080698"),
  setor_2000   = "530010805080464",
  novo_codigo  = "5300108050804641"
)

# Unir setores de 2010 que formam o setor 530010805080200 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080200", "530010805080644"),
  setor_2000   = "530010805080200",
  novo_codigo  = "5300108050802001"
)

# Unir setores de 2010 que formam o setor 530010805080228 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080228", "530010805080650"),
  setor_2000   = "530010805080228",
  novo_codigo  = "5300108050802281"
)

# Unir setores de 2010 que formam o setor 530010805080224 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080224", "530010805080649"),
  setor_2000   = "530010805080224",
  novo_codigo  = "5300108050802241"
)

# Unir setores de 2010 que formam o setor 530010805080222 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080222", "530010805080648"),
  setor_2000   = "530010805080222",
  novo_codigo  = "5300108050802221"
)

# Unir setores de 2010 que formam o setor 530010805080221 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080221", "530010805080647"),
  setor_2000   = "530010805080221",
  novo_codigo  = "5300108050802211"
)

# Unir setores de 2010 que formam o setor 530010805080202 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080202", "530010805080645"),
  setor_2000   = "530010805080202",
  novo_codigo  = "5300108050802021"
)

# Unir setores de 2010 que formam o setor 530010805080203 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080203", "530010805080646"),
  setor_2000   = "530010805080203",
  novo_codigo  = "5300108050802031"
)

# Unir setores de 2010 que formam o setor 530010805080230 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080230", "530010805080499"),
  setor_2000   = "530010805080230",
  novo_codigo  = "5300108050802301"
)

# Unir setores de 2010 que formam o setor 530010805080245 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080245", "530010805080519"),
  setor_2000   = "530010805080245",
  novo_codigo  = "5300108050802451"
)

# Unir setores de 2010 que formam o setor 530010805080247 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080247", "530010805080520"),
  setor_2000   = "530010805080247",
  novo_codigo  = "5300108050802471"
)

# Unir setores de 2010 que formam o setor 530010805080254 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080254", "530010805080660"),
  setor_2000   = "530010805080254",
  novo_codigo  = "5300108050802541"
)

# Unir setores de 2010 que formam o setor 530010805080255 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080255", "530010805080661"),
  setor_2000   = "530010805080255",
  novo_codigo  = "5300108050802551"
)

# Unir setores de 2010 que formam o setor 530010805080263 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080263", "530010805080662"),
  setor_2000   = "530010805080263",
  novo_codigo  = "5300108050802631"
)

# Unir setores de 2010 que formam o setor 530010805080301 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080301", "530010805080670"),
  setor_2000   = "530010805080301",
  novo_codigo  = "5300108050803011"
)

# Unir setores de 2010 que formam o setor 530010805080303 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080303", "530010805080671"),
  setor_2000   = "530010805080303",
  novo_codigo  = "5300108050803031"
)

# Unir setores de 2010 que formam o setor 530010805080324 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080324", "530010805080672"),
  setor_2000   = "530010805080324",
  novo_codigo  = "5300108050803241"
)

# Unir setores de 2010 que formam o setor 530010805080250 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080250", "530010805080658", "530010805080521"),
  setor_2000   = "530010805080250",
  novo_codigo  = "5300108050802501"
)

# Unir setores de 2010 que formam o setor 530010805080252 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080252", "530010805080522"),
  setor_2000   = "530010805080252",
  novo_codigo  = "5300108050802521"
)

# Unir setores de 2010 que formam o setor 530010805080253 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080659", "530010805080253"),
  setor_2000   = "530010805080253",
  novo_codigo  = "5300108050802531"
)

# Unir setores de 2010 que formam o setor 530010805080265 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080265", "530010805080663"),
  setor_2000   = "530010805080265",
  novo_codigo  = "5300108050802651"
)

# Unir setores de 2010 que formam o setor 530010805080266 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080266", "530010805080664"),
  setor_2000   = "530010805080266",
  novo_codigo  = "5300108050802661"
)

# Unir setores de 2010 que formam o setor 530010805080268 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080268", "530010805080523"),
  setor_2000   = "530010805080268",
  novo_codigo  = "5300108050802681"
)

# Unir setores de 2010 que formam o setor 530010805080270 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080270", "530010805080665"),
  setor_2000   = "530010805080270",
  novo_codigo  = "5300108050802701"
)

# Unir setores de 2010 que formam o setor 530010805080272 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080272", "530010805080666"),
  setor_2000   = "530010805080272",
  novo_codigo  = "5300108050802721"
)

# Unir setores de 2010 que formam o setor 530010805080283 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080667", "530010805080283"),
  setor_2000   = "530010805080283",
  novo_codigo  = "5300108050802831"
)

# Unir setores de 2010 que formam o setor 530010805080293 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080293", "530010805080668"),
  setor_2000   = "530010805080293",
  novo_codigo  = "5300108050802931"
)

# Unir setores de 2010 que formam o setor 530010805080300 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080300", "530010805080669"),
  setor_2000   = "530010805080300",
  novo_codigo  = "5300108050803001"
)

# Unir setores de 2010 que formam o setor 530010805080308 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080524", "530010805080308"),
  setor_2000   = "530010805080308",
  novo_codigo  = "5300108050803081"
)

# Unir setores de 2010 que formam o setor 530010805080332 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080332", "530010805080525"),
  setor_2000   = "530010805080332",
  novo_codigo  = "5300108050803321"
)

# Unir setores de 2010 que formam o setor 530010805080349 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080349", "530010805080674", "530010805080529"),
  setor_2000   = "530010805080349",
  novo_codigo  = "5300108050803491"
)

# Unir setores de 2010 que formam o setor 530010805080341 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080341", "530010805080673"),
  setor_2000   = "530010805080341",
  novo_codigo  = "5300108050803411"
)

# Unir setores de 2010 que formam o setor 530010805080346 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080346", "530010805080528"),
  setor_2000   = "530010805080346",
  novo_codigo  = "5300108050803461"
)

# Unir setores de 2010 que formam o setor 530010805080345 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080345", "530010805080526", "530010805080527"),
  setor_2000   = "530010805080345",
  novo_codigo  = "5300108050803451"
)

# Unir setores de 2010 que formam o setor 530010805080357 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080357", "530010805080675"),
  setor_2000   = "530010805080357",
  novo_codigo  = "5300108050803571"
)

# Unir setores de 2010 que formam o setor 530010805080364 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080530", "530010805080531", "530010805080364", "530010805080704", "530010805080705"),
  setor_2000   = "530010805080364",
  novo_codigo  = "5300108050803641"
)

# Unir setores de 2010 que formam o setor 530010805080375 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080375", "530010805080532"),
  setor_2000   = "530010805080375",
  novo_codigo  = "5300108050803751"
)

# Unir setores de 2010 que formam o setor 530010805080397 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080397", "530010805080679"),
  setor_2000   = "530010805080397",
  novo_codigo  = "5300108050803971"
)

# Unir setores de 2010 que formam o setor 530010805080425 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080425", "530010805080680"),
  setor_2000   = "530010805080425",
  novo_codigo  = "5300108050804251"
)

# Unir setores de 2010 que formam o setor 530010805080369 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080369", "530010805080676"),
  setor_2000   = "530010805080369",
  novo_codigo  = "5300108050803691"
)

# Unir setores de 2010 que formam o setor 530010805080394 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080394", "530010805080677"),
  setor_2000   = "530010805080394",
  novo_codigo  = "5300108050803941"
)

# Unir setores de 2010 que formam o setor 530010805080395 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080395", "530010805080678"),
  setor_2000   = "530010805080395",
  novo_codigo  = "5300108050803951"
)

# Unir setores de 2010 que formam o setor 530010805080396 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080396", "530010805080720"),
  setor_2000   = "530010805080396",
  novo_codigo  = "5300108050803961"
)

# Unir setores de 2010 que formam o setor 530010805150163 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150163", "530010805150426"),
  setor_2000   = "530010805150163",
  novo_codigo  = "5300108051501631"
)

# Unir setores de 2010 que formam o setor 530010805150162 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150424", "530010805150425", "530010805150162"),
  setor_2000   = "530010805150162",
  novo_codigo  = "5300108051501621"
)

# Unir setores de 2010 que formam o setor 530010805150104 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150104", "530010805150412"),
  setor_2000   = "530010805150104",
  novo_codigo  = "5300108051501041"
)

# Unir setores de 2010 que formam o setor 530010805150019 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150019", "530010805150401"),
  setor_2000   = "530010805150019",
  novo_codigo  = "5300108051500191"
)

# Unir setores de 2010 que formam o setor 530010805150016 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150016", "530010805150485"),
  setor_2000   = "530010805150016",
  novo_codigo  = "5300108051500161"
)

# Unir setores de 2010 que formam o setor 530010805150012 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150012", "530010805150484"),
  setor_2000   = "530010805150012",
  novo_codigo  = "5300108051500121"
)

# Unir setores de 2010 que formam o setor 530010805150009 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150009", "530010805150400"),
  setor_2000   = "530010805150009",
  novo_codigo  = "5300108051500091"
)

# Unir setores de 2010 que formam o setor 530010805150033 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150402", "530010805150033"),
  setor_2000   = "530010805150033",
  novo_codigo  = "5300108051500331"
)

# Unir setores de 2010 que formam o setor 530010805150064 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150064", "530010805150487"),
  setor_2000   = "530010805150064",
  novo_codigo  = "5300108051500641"
)

# Unir setores de 2010 que formam o setor 530010805150061 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150061", "530010805150486"),
  setor_2000   = "530010805150061",
  novo_codigo  = "5300108051500611"
)

# Unir setores de 2010 que formam o setor 530010805150060 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150060", "530010805150406"),
  setor_2000   = "530010805150060",
  novo_codigo  = "5300108051500601"
)

# Unir setores de 2010 que formam o setor 530010805150084 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150084", "530010805150407"),
  setor_2000   = "530010805150084",
  novo_codigo  = "5300108051500841"
)

# Unir setores de 2010 que formam o setor 530010805150085 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150085", "530010805150408"),
  setor_2000   = "530010805150085",
  novo_codigo  = "5300108051500851"
)

# Unir setores de 2010 que formam o setor 530010805150086 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150086", "530010805150409"),
  setor_2000   = "530010805150086",
  novo_codigo  = "5300108051500861"
)

# Unir setores de 2010 que formam o setor 530010805150090 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150090", "530010805150488"),
  setor_2000   = "530010805150090",
  novo_codigo  = "5300108051500901"
)

# Unir setores de 2010 que formam o setor 530010805150122 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150122", "530010805150493"),
  setor_2000   = "530010805150122",
  novo_codigo  = "5300108051501221"
)

# Unir setores de 2010 que formam o setor 530010805150114 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150114", "530010805150492"),
  setor_2000   = "530010805150114",
  novo_codigo  = "5300108051501141"
)

# Unir setores de 2010 que formam o setor 530010805150113 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150113", "530010805150414"),
  setor_2000   = "530010805150113",
  novo_codigo  = "5300108051501131"
)

# Unir setores de 2010 que formam o setor 530010805150112 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150112", "530010805150413"),
  setor_2000   = "530010805150112",
  novo_codigo  = "5300108051501121"
)

# Unir setores de 2010 que formam o setor 530010805150111 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150111", "530010805150491"),
  setor_2000   = "530010805150111",
  novo_codigo  = "5300108051501111"
)

# Unir setores de 2010 que formam o setor 530010805150128 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150128", "530010805150386"),
  setor_2000   = "530010805150128",
  novo_codigo  = "5300108051501281"
)

# Unir setores de 2010 que formam o setor 530010805150130 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150552", "530010805150553", "530010805150549", "530010805150551", "530010805150546", "530010805150547", "530010805150548", "530010805150130"),
  setor_2000   = "530010805150130",
  novo_codigo  = "5300108051501301"
)

# Unir setores de 2010 que formam o setor 530010805150131 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150518", "530010805150519", "530010805150537", "530010805150514", "530010805150516", "530010805150517", "530010805150131", "530010805150515"),
  setor_2000   = "530010805150131",
  novo_codigo  = "5300108051501311"
)

# Unir setores de 2010 que formam o setor 530010805150132 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150132", "530010805150415"),
  setor_2000   = "530010805150132",
  novo_codigo  = "5300108051501321"
)

# Unir setores de 2010 que formam o setor 530010805150151 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150151", "530010805150420"),
  setor_2000   = "530010805150151",
  novo_codigo  = "5300108051501511"
)

# Unir setores de 2010 que formam o setor 530010805150150 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150150", "530010805150419"),
  setor_2000   = "530010805150150",
  novo_codigo  = "5300108051501501"
)

# Unir setores de 2010 que formam o setor 530010805150149 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150149", "530010805150418"),
  setor_2000   = "530010805150149",
  novo_codigo  = "5300108051501491"
)

# Unir setores de 2010 que formam o setor 530010805150157 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150157", "530010805150421"),
  setor_2000   = "530010805150157",
  novo_codigo  = "5300108051501571"
)

# Unir setores de 2010 que formam o setor 530010805150160 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150160", "530010805150422"),
  setor_2000   = "530010805150160",
  novo_codigo  = "5300108051501601"
)

# Unir setores de 2010 que formam o setor 530010805150161 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150161", "530010805150423"),
  setor_2000   = "530010805150161",
  novo_codigo  = "5300108051501611"
)

# Unir setores de 2010 que formam o setor 530010805080240 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080517", "530010805080719", "530010805080728", "530010805080240"),
  setor_2000   = "530010805080240",
  novo_codigo  = "5300108050802401"
)

# Unir setores de 2010 que formam o setor 530010805080430 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080430", "530010805080681"),
  setor_2000   = "530010805080430",
  novo_codigo  = "5300108050804301"
)

# Unir setores de 2010 que formam o setor 530010805080435 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080435", "530010805080683"),
  setor_2000   = "530010805080435",
  novo_codigo  = "5300108050804351"
)

# Unir setores de 2010 que formam o setor 530010805080443 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080443", "530010805080684"),
  setor_2000   = "530010805080443",
  novo_codigo  = "5300108050804431"
)

# Unir setores de 2010 que formam o setor 530010805080446 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080446", "530010805080685"),
  setor_2000   = "530010805080446",
  novo_codigo  = "5300108050804461"
)

# Unir setores de 2010 que formam o setor 530010805080449 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080449", "530010805080686"),
  setor_2000   = "530010805080449",
  novo_codigo  = "5300108050804491"
)

# Unir setores de 2010 que formam o setor 530010805080452 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080452", "530010805080687"),
  setor_2000   = "530010805080452",
  novo_codigo  = "5300108050804521"
)

# Unir setores de 2010 que formam o setor 530010805080454 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080454", "530010805080688"),
  setor_2000   = "530010805080454",
  novo_codigo  = "5300108050804541"
)

# Unir setores de 2010 que formam o setor 530010805080460 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805080460", "530010805080689"),
  setor_2000   = "530010805080460",
  novo_codigo  = "5300108050804601"
)

# Unir setores de 2010 que formam o setor 530010805150164 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150164", "530010805150427"),
  setor_2000   = "530010805150164",
  novo_codigo  = "5300108051501641"
)

# Unir setores de 2010 que formam o setor 530010805150165 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150165", "530010805150428"),
  setor_2000   = "530010805150165",
  novo_codigo  = "5300108051501651"
)

# Unir setores de 2010 que formam o setor 530010805150166 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150429", "530010805150430", "530010805150521", "530010805150166"),
  setor_2000   = "530010805150166",
  novo_codigo  = "5300108051501661"
)

# Unir setores de 2010 que formam o setor 530010805150222 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150222", "530010805150439"),
  setor_2000   = "530010805150222",
  novo_codigo  = "5300108051502221"
)

# Unir setores de 2010 que formam o setor 530010805150313 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150313", "530010805150512"),
  setor_2000   = "530010805150313",
  novo_codigo  = "5300108051503131"
)

# Unir setores de 2010 que formam o setor 530010805150314 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150314", "530010805150392", "530010805150501"),
  setor_2000   = "530010805150314",
  novo_codigo  = "5300108051503141"
)

# Unir setores de 2010 que formam o setor 530010805150312 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150312", "530010805150391"),
  setor_2000   = "530010805150312",
  novo_codigo  = "5300108051503121"
)

# Unir setores de 2010 que formam o setor 530010805150317 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150317", "530010805150502"),
  setor_2000   = "530010805150317",
  novo_codigo  = "5300108051503171"
)

# Unir setores de 2010 que formam o setor 530010805150320 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150320", "530010805150393"),
  setor_2000   = "530010805150320",
  novo_codigo  = "5300108051503201"
)

# Unir setores de 2010 que formam o setor 530010805150377 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150377", "530010805150508"),
  setor_2000   = "530010805150377",
  novo_codigo  = "5300108051503771"
)

# Unir setores de 2010 que formam o setor 530010805150380 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150380", "530010805150509"),
  setor_2000   = "530010805150380",
  novo_codigo  = "5300108051503801"
)

# Unir setores de 2010 que formam o setor 530010805150361 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150361", "530010805150506"),
  setor_2000   = "530010805150361",
  novo_codigo  = "5300108051503611"
)

# Unir setores de 2010 que formam o setor 530010805150362 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150362", "530010805150507"),
  setor_2000   = "530010805150362",
  novo_codigo  = "5300108051503621"
)

# Unir setores de 2010 que formam o setor 530010805150337 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150337", "530010805150503"),
  setor_2000   = "530010805150337",
  novo_codigo  = "5300108051503371"
)

# Unir setores de 2010 que formam o setor 530010805150300 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150300", "530010805150390"),
  setor_2000   = "530010805150300",
  novo_codigo  = "5300108051503001"
)

# Unir setores de 2010 que formam o setor 530010805150297 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150297", "530010805150389"),
  setor_2000   = "530010805150297",
  novo_codigo  = "5300108051502971"
)

# Unir setores de 2010 que formam o setor 530010805150352 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150352", "530010805150504"),
  setor_2000   = "530010805150352",
  novo_codigo  = "5300108051503521"
)

# Unir setores de 2010 que formam o setor 530010805150353 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150353", "530010805150505"),
  setor_2000   = "530010805150353",
  novo_codigo  = "5300108051503531"
)

# Unir setores de 2010 que formam o setor 530010805150348 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150348", "530010805150396"),
  setor_2000   = "530010805150348",
  novo_codigo  = "5300108051503481"
)

# Unir setores de 2010 que formam o setor 530010805150349 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150349", "530010805150397"),
  setor_2000   = "530010805150349",
  novo_codigo  = "5300108051503491"
)

# Unir setores de 2010 que formam o setor 530010805150342 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150342", "530010805150395"),
  setor_2000   = "530010805150342",
  novo_codigo  = "5300108051503421"
)

# Unir setores de 2010 que formam o setor 530010805150307 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150307", "530010805150500"),
  setor_2000   = "530010805150307",
  novo_codigo  = "5300108051503071"
)

# Unir setores de 2010 que formam o setor 530010805150305 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150305", "530010805150499"),
  setor_2000   = "530010805150305",
  novo_codigo  = "5300108051503051"
)

# Unir setores de 2010 que formam o setor 530010805150279 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150279", "530010805150451"),
  setor_2000   = "530010805150279",
  novo_codigo  = "5300108051502791"
)

# Unir setores de 2010 que formam o setor 530010805150280 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150280", "530010805150452"),
  setor_2000   = "530010805150280",
  novo_codigo  = "5300108051502801"
)

# Unir setores de 2010 que formam o setor 530010805150283 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150283", "530010805150496"),
  setor_2000   = "530010805150283",
  novo_codigo  = "5300108051502831"
)

# Unir setores de 2010 que formam o setor 530010805150284 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150284", "530010805150453"),
  setor_2000   = "530010805150284",
  novo_codigo  = "5300108051502841"
)

# Unir setores de 2010 que formam o setor 530010805150285 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150285", "530010805150454"),
  setor_2000   = "530010805150285",
  novo_codigo  = "5300108051502851"
)

# Unir setores de 2010 que formam o setor 530010805150286 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150286", "530010805150388"),
  setor_2000   = "530010805150286",
  novo_codigo  = "5300108051502861"
)

# Unir setores de 2010 que formam o setor 530010805150291 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150291", "530010805150497"),
  setor_2000   = "530010805150291",
  novo_codigo  = "5300108051502911"
)

# Unir setores de 2010 que formam o setor 530010805150293 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150293", "530010805150498"),
  setor_2000   = "530010805150293",
  novo_codigo  = "5300108051502931"
)

# Unir setores de 2010 que formam o setor 530010805150268 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150268", "530010805150446"),
  setor_2000   = "530010805150268",
  novo_codigo  = "5300108051502681"
)

# Unir setores de 2010 que formam o setor 530010805150269 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150269", "530010805150447"),
  setor_2000   = "530010805150269",
  novo_codigo  = "5300108051502691"
)

# Unir setores de 2010 que formam o setor 530010805150276 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150276", "530010805150448"),
  setor_2000   = "530010805150276",
  novo_codigo  = "5300108051502761"
)

# Unir setores de 2010 que formam o setor 530010805150277 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150277", "530010805150449"),
  setor_2000   = "530010805150277",
  novo_codigo  = "5300108051502771"
)

# Unir setores de 2010 que formam o setor 530010805150278 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150278", "530010805150450"),
  setor_2000   = "530010805150278",
  novo_codigo  = "5300108051502781"
)

# Unir setores de 2010 que formam o setor 530010805150244 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150244", "530010805150442"),
  setor_2000   = "530010805150244",
  novo_codigo  = "5300108051502441"
)

# Unir setores de 2010 que formam o setor 530010805150248 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150248", "530010805150495"),
  setor_2000   = "530010805150248",
  novo_codigo  = "5300108051502481"
)

# Unir setores de 2010 que formam o setor 530010805150249 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150249", "530010805150443"),
  setor_2000   = "530010805150249",
  novo_codigo  = "5300108051502491"
)

# Unir setores de 2010 que formam o setor 530010805150251 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150251", "530010805150444"),
  setor_2000   = "530010805150251",
  novo_codigo  = "5300108051502511"
)

# Unir setores de 2010 que formam o setor 530010805150258 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150258", "530010805150445"),
  setor_2000   = "530010805150258",
  novo_codigo  = "5300108051502581"
)

# Unir setores de 2010 que formam o setor 530010805150229 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150229", "530010805150440"),
  setor_2000   = "530010805150229",
  novo_codigo  = "5300108051502291"
)

# Unir setores de 2010 que formam o setor 530010805150232 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150232", "530010805150441"),
  setor_2000   = "530010805150232",
  novo_codigo  = "5300108051502321"
)

# Unir setores de 2010 que formam o setor 530010805150241 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150241", "530010805150540"),
  setor_2000   = "530010805150241",
  novo_codigo  = "5300108051502411"
)

# Unir setores de 2010 que formam o setor 530010805150206 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150206", "530010805150438"),
  setor_2000   = "530010805150206",
  novo_codigo  = "5300108051502061"
)

# Unir setores de 2010 que formam o setor 530010805150207 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150207", "530010805150494"),
  setor_2000   = "530010805150207",
  novo_codigo  = "5300108051502071"
)

# Unir setores de 2010 que formam o setor 530010805150201 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150201", "530010805150435"),
  setor_2000   = "530010805150201",
  novo_codigo  = "5300108051502011"
)

# Unir setores de 2010 que formam o setor 530010805150204 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150204", "530010805150436"),
  setor_2000   = "530010805150204",
  novo_codigo  = "5300108051502041"
)

# Unir setores de 2010 que formam o setor 530010805150205 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150205", "530010805150437"),
  setor_2000   = "530010805150205",
  novo_codigo  = "5300108051502051"
)

# Unir setores de 2010 que formam o setor 530010805150176 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150520", "530010805150176"),
  setor_2000   = "530010805150176",
  novo_codigo  = "5300108051501761"
)

# Unir setores de 2010 que formam o setor 530010805150182 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150182", "530010805150434"),
  setor_2000   = "530010805150182",
  novo_codigo  = "5300108051501821"
)

# Unir setores de 2010 que formam o setor 530010805150169 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150169", "530010805150431"),
  setor_2000   = "530010805150169",
  novo_codigo  = "5300108051501691"
)

# Unir setores de 2010 que formam o setor 530010805150170 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150170", "530010805150432"),
  setor_2000   = "530010805150170",
  novo_codigo  = "5300108051501701"
)

# Unir setores de 2010 que formam o setor 530010805150171 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150171", "530010805150433"),
  setor_2000   = "530010805150171",
  novo_codigo  = "5300108051501711"
)

# Unir setores de 2010 que formam o setor 530010805150175 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150175", "530010805150541", "530010805150542", "530010805150543", "530010805150544", "530010805150545", "530010805150538", "530010805150539"),
  setor_2000   = "530010805150175",
  novo_codigo  = "5300108051501751"
)

# Unir setores de 2010 que formam o setor 530010805150390 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805150458", "530010805150464", "530010805150465", "530010805150462", "530010805150463", "530010805150466", "530010805150460", "530010805150461"),
  setor_2000   = "530010805150390",
  novo_codigo  = "5300108051503901"
)

# Unir setores de 2010 que formam o setor 530010805070152-0155 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070170", "530010805070171"),
  setor_2000   = "530010805070152-0155",
  novo_codigo  = "530010805070152-01551"
)

# Unir setores de 2010 que formam o setor 530010805100140 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805100233", "530010805100212"),
  setor_2000   = "530010805100140",
  novo_codigo  = "5300108051001401"
)

# Unir setores de 2010 que formam o setor 530010805110129 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110129", "530010805110257"),
  setor_2000   = "530010805110129",
  novo_codigo  = "5300108051101291"
)

# Unir setores de 2010 que formam o setor 530010805110126 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110126", "530010805110253", "530010805110254"),
  setor_2000   = "530010805110126",
  novo_codigo  = "5300108051101261"
)

# Unir setores de 2010 que formam o setor 530010805110121 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110121", "530010805110250"),
  setor_2000   = "530010805110121",
  novo_codigo  = "5300108051101211"
)

# Unir setores de 2010 que formam o setor 530010805110059 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805110059", "530010805110223"),
  setor_2000   = "530010805110059",
  novo_codigo  = "5300108051100591"
)

# Unir setores de 2010 que formam o setor 530010805300037 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805300037", "530010805300069"),
  setor_2000   = "530010805300037",
  novo_codigo  = "5300108053000371"
)

# Unir setores de 2010 que formam o setor 530010805070111 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070111", "530010805070227"),
  setor_2000   = "530010805070111",
  novo_codigo  = "5300108050701111"
)

# Unir setores de 2010 que formam o setor 530010805070085 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070085", "530010805070148"),
  setor_2000   = "530010805070085",
  novo_codigo  = "5300108050700851"
)

# Unir setores de 2010 que formam o setor 530010805070095 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070095", "530010805070149"),
  setor_2000   = "530010805070095",
  novo_codigo  = "5300108050700951"
)

# Unir setores de 2010 que formam o setor 530010805070094 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070094", "530010805070194"),
  setor_2000   = "530010805070094",
  novo_codigo  = "5300108050700941"
)

# Unir setores de 2010 que formam o setor 530010805070099 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805070099", "530010805070197"),
  setor_2000   = "530010805070099",
  novo_codigo  = "5300108050700991"
)

# Unir setores de 2010 que formam o setor 530010805120005 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805120005", "530010805120048"),
  setor_2000   = "530010805120005",
  novo_codigo  = "5300108051200051"
)

# Unir setores de 2010 que formam o setor 530010805200036 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200146", "530010805200036"),
  setor_2000   = "530010805200036",
  novo_codigo  = "5300108052000361"
)

# Unir setores de 2010 que formam o setor 530010805200046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200153", "530010805200046"),
  setor_2000   = "530010805200046",
  novo_codigo  = "5300108052000461"
)

# Unir setores de 2010 que formam o setor 530010805200017 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200017", "530010805200080"),
  setor_2000   = "530010805200017",
  novo_codigo  = "5300108052000171"
)

# Unir setores de 2010 que formam o setor 530010805200018 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805200018", "530010805200081"),
  setor_2000   = "530010805200018",
  novo_codigo  = "5300108052000181"
)

# Unir setores de 2010 que formam o setor 530010805060093 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060093", "530010805060394"),
  setor_2000   = "530010805060093",
  novo_codigo  = "5300108050600931"
)

# Unir setores de 2010 que formam o setor 530010805060028 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060028", "530010805060298"),
  setor_2000   = "530010805060028",
  novo_codigo  = "5300108050600281"
)

# Unir setores de 2010 que formam o setor 530010805060091 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060091", "530010805060393"),
  setor_2000   = "530010805060091",
  novo_codigo  = "5300108050600911"
)

# Unir setores de 2010 que formam o setor 530010805060030 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060030", "530010805060299"),
  setor_2000   = "530010805060030",
  novo_codigo  = "5300108050600301"
)

# Unir setores de 2010 que formam o setor 530010805060083 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060083", "530010805060391"),
  setor_2000   = "530010805060083",
  novo_codigo  = "5300108050600831"
)

# Unir setores de 2010 que formam o setor 530010805060049 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060049", "530010805060384"),
  setor_2000   = "530010805060049",
  novo_codigo  = "5300108050600491"
)

# Unir setores de 2010 que formam o setor 530010805060050 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060050", "530010805060310"),
  setor_2000   = "530010805060050",
  novo_codigo  = "5300108050600501"
)

# Unir setores de 2010 que formam o setor 530010805060046 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060046", "530010805060308"),
  setor_2000   = "530010805060046",
  novo_codigo  = "5300108050600461"
)

# Unir setores de 2010 que formam o setor 530010805060258 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060258", "530010805060353", "530010805060354", "530010805060355"),
  setor_2000   = "530010805060258",
  novo_codigo  = "5300108050602581"
)

# Unir setores de 2010 que formam o setor 530010805060262 de 2000 e substituí-los pelo novo setor
censo_sf_corrigido <- unir_setores_temporais(
  df = censo_sf_corrigido,
  setores_2010 = c("530010805060262", "530010805060419", "530010805060420"),
  setor_2000   = "530010805060262",
  novo_codigo  = "5300108050602621"
)

return(censo_sf_corrigido)
}

# 
# st_write(censo_sf_corrigido, "censo_sf_corrigido.geojson", delete_dsn = TRUE)

# Excluir observações irrelevantes
#censo_sf_corrigido <- censo_sf_corrigido %>%
#  filter(!(code_tract %in% c("530010805070001-0142", "530010805250001-0113", "530010805200001-0073")))
