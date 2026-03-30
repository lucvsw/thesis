# Este script pega os dados das malhas censitárias fornecidos pelo IBGE e une com os shapefiles dos dois anos

# Obtendo os dataframes dos dados a serem mesclados com o shapefile do censo de 2000
get_censo_2000_DF <- function() {
  # Dados da planilha básica de 2000
  censo_DF_2000 <- read_excel(here("censo_2000_basico.XLS"))
  
  # Dados da planilha de indivíduos responsáveis de 2000
  censo_DF_2000_responsavel <- read_excel(here("censo_2000_Responsavel1.XLS"))
  
  # Dados da planilha de pessoas de 2000
  censo_DF_2000_pessoa <- read_excel(here("censo_2000_Pessoa1.XLS"))
  
  # Dados da planilha de instrução de 2000
  censo_DF_2000_instrucao <- read_excel(here("censo_2000_Instrucao1.XLS"))
  
  # Dados da planilha de instrução de 2000
  censo_DF_2000_domicilio <- read_excel(here("Domicilio_DF.XLS"))

  # Dados de moradores de 2000 (total e por sexo) — para prop_mulheres
  censo_DF_2000_morador <- read_excel(
    here("dados/Dados setores sensitarios DF/2000/Distrito Federal/Morador_DF.XLS")
  )

  return(list(
    censo_DF_2000 = censo_DF_2000,
    censo_DF_2000_responsavel = censo_DF_2000_responsavel,
    censo_DF_2000_pessoa = censo_DF_2000_pessoa,
    censo_DF_2000_instrucao = censo_DF_2000_instrucao,
    censo_DF_2000_domicilio = censo_DF_2000_domicilio,
    censo_DF_2000_morador = censo_DF_2000_morador
  ))
}

# Mesclando os dados com o shapefile para 2000
unir_dados_sf_2000 <- function(censo_sf_2000_completo, dados_lista) {
  # Extrair os objetos da lista
  censo_DF_2000 <- dados_lista$censo_DF_2000
  censo_DF_2000_responsavel <- dados_lista$censo_DF_2000_responsavel
  censo_DF_2000_pessoa <- dados_lista$censo_DF_2000_pessoa
  censo_DF_2000_instrucao <- dados_lista$censo_DF_2000_instrucao
  censo_DF_2000_domicilio <- dados_lista$censo_DF_2000_domicilio
  censo_DF_2000_morador <- dados_lista$censo_DF_2000_morador
  
  censo_2000_completo <- censo_sf_2000_completo %>%
    left_join(censo_DF_2000 %>% dplyr::select(Cod_setor, Var01, Var02, Var03, Var05, Var06, Var12),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(censo_DF_2000_responsavel %>% dplyr::select(Cod_setor, V0595, V0611, V0577, V0402, V0509),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(censo_DF_2000_pessoa %>% dplyr::select(Cod_setor, V1461, V1462, V1463, V1464),
              by = c("code_tract" = "Cod_setor")) %>% 
    left_join(censo_DF_2000_instrucao %>% dplyr::select(Cod_setor, V2249),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      censo_DF_2000_domicilio %>%
        transmute(
          Cod_setor,
          V0007,
          dom_5mais = as.numeric(V0060) + as.numeric(V0061) + as.numeric(V0062) +
                      as.numeric(V0063) + as.numeric(V0064) + as.numeric(V0065)
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      censo_DF_2000_morador %>%
        transmute(
          Cod_setor = as.character(Cod_setor),
          total_moradores = as.numeric(V0237),
          moradores_homens = as.numeric(V0292)
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    rename(
      domicilios = Var01,
      renda_total = Var02,
      renda_percapita = Var03,
      renda_positiva = Var05,
      renda_media_positiva = Var06,
      pop = Var12,
      ens_superior = V0595,
      sem_renda = V0611,
      idade_65 = V1461,
      idade_70 = V1462,
      idade_75 = V1463,
      idade_80 = V1464,
      analfabetos = V2249,
      apartamentos = V0007,
      ens_sup_completo = V0577,
      resp_heads_total = V0402,
      resp_heads_alfab = V0509
    ) %>%
    mutate(ano = 2000)
  
  return(censo_2000_completo)
}

###############################################################################

# Obtendo os dataframes dos dados a serem mesclados com o shapefile da malha de 2010
get_censo_2010_DF <- function() {
  # Dados da planilha básica de 2010
  censo_DF_2010 <- read_excel(here("censo_2010_basico.XLS"))
  censo_DF_2010 <- censo_DF_2010 %>%
    mutate(Cod_setor = as.character(Cod_setor))
  
  # Dados da planilha de responsável/renda
  censo_DF_2010_responsavel <- read_excel(here("censo_2010_ResponsavelRenda.XLS"))
  censo_DF_2010_responsavel <- censo_DF_2010_responsavel %>%
    mutate(Cod_setor = as.character(Cod_setor))
  
  # Dados da planilha de domicilio (estrutura e tipo)
  censo_DF_2010_domicilio <- read_excel(here("Domicilio01_DF.XLS"))
  censo_DF_2010_domicilio <- censo_DF_2010_domicilio %>%
    mutate(Cod_setor = as.character(Cod_setor))

  # Dados de moradores de 2010 (total e por sexo) — para prop_mulheres
  base2010 <- here("dados/Dados setores sensitarios DF/2010/EXCEL")
  censo_DF_2010_domicilio02 <- read_excel(file.path(base2010, "Domicilio02_DF.xls"))
  censo_DF_2010_domicilio02 <- censo_DF_2010_domicilio02 %>%
    mutate(Cod_setor = as.character(Cod_setor))

  # Dados de responsáveis de 2010 (analfabetismo) — para prop_analf_resp
  censo_DF_2010_resp02 <- read_excel(file.path(base2010, "Responsavel02_DF.xls"))
  censo_DF_2010_resp02 <- censo_DF_2010_resp02 %>%
    mutate(Cod_setor = as.character(Cod_setor))

  return(list(
    censo_DF_2010 = censo_DF_2010,
    censo_DF_2010_responsavel = censo_DF_2010_responsavel,
    censo_DF_2010_domicilio = censo_DF_2010_domicilio,
    censo_DF_2010_domicilio02 = censo_DF_2010_domicilio02,
    censo_DF_2010_resp02 = censo_DF_2010_resp02
  ))
}

# Mesclando os dados com o shapefile de 2010
unir_dados_sf_2010 <- function(censo_sf_2010, dados_lista, censo_2000_completo) {
  censo_DF_2010 <- dados_lista$censo_DF_2010
  censo_DF_2010_responsavel <- dados_lista$censo_DF_2010_responsavel
  censo_DF_2010_domicilio <- dados_lista$censo_DF_2010_domicilio
  censo_DF_2010_domicilio02 <- dados_lista$censo_DF_2010_domicilio02
  censo_DF_2010_resp02 <- dados_lista$censo_DF_2010_resp02
  
  # Adicionando os dados ao shapefile de 2010
  censo_2010_completo <- censo_sf_2010 %>%
    left_join(censo_DF_2010 %>% dplyr::select(Cod_setor, V001, V002),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(censo_DF_2010_responsavel %>% dplyr::select(Cod_setor, V021, V022),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      censo_DF_2010_domicilio %>%
        transmute(
          Cod_setor,
          V005,
          dom_5mais = suppressWarnings(
            as.numeric(V054) + as.numeric(V055) + as.numeric(V056) +
            as.numeric(V057) + as.numeric(V058) + as.numeric(V059)
          )
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      censo_DF_2010_domicilio02 %>%
        transmute(
          Cod_setor,
          total_moradores = suppressWarnings(as.numeric(V001)),
          moradores_homens = suppressWarnings(as.numeric(V045))
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      censo_DF_2010_resp02 %>%
        transmute(
          Cod_setor,
          resp_heads_total = suppressWarnings(as.numeric(V001)),
          resp_heads_alfab = suppressWarnings(as.numeric(V093))
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    rename(domicilios = V001,
           pop = V002,
           renda_positiva = V021,
           renda_total = V022,
           apartamentos = V005) %>%
    mutate(
      ano = 2010,
      # Algumas linhas das variáveis abaixo possuem valor "X", substitui "X" por "0" (como string), depois converti os valores das variáveis para número
      renda_total = parse_number(
        if_else(renda_total == "X", "0", renda_total),
        locale = locale(decimal_mark = ",")
      ),
      renda_positiva = parse_number(
        if_else(renda_positiva == "X", "0", renda_positiva),
        locale = locale(decimal_mark = ",")
      ),
      apartamentos = parse_number(
        if_else(apartamentos == "X", "0", apartamentos),
        locale = locale(decimal_mark = ",")
      ),
    )
  
  # Verificar colunas que existem em 2000 mas não em 2010
  colunas_faltantes <- setdiff(names(censo_2000_completo), names(censo_2010_completo))
  
  # Criar essas colunas no censo_2010_completo com NA
  for (col in colunas_faltantes) {
    censo_2010_completo[[col]] <- NA
  }
  
  return(censo_2010_completo)
}

# Unir os dados do censo de 2000 e 2010 e transformar numa série temporal de dois anos
preparar_para_compatibilizar <- function(censo_2000_completo, censo_2010_completo) {
  # Unir os dados
  censo_bsb_serie_temporal <- bind_rows(censo_2010_completo, censo_2000_completo)
  
  # Remover duplicatas
  censo_sf_para_corrigir <- censo_bsb_serie_temporal %>%
    distinct(code_tract, ano, pop, geom, .keep_all = TRUE) # Se duas linhas tiverem exatamente os mesmos valores nessas colunas, uma delas será removida
  
  return(censo_sf_para_corrigir)
}

