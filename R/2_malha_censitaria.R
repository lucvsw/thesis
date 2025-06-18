### ESTE ARQUIVO É A VERSÃO MAIS ATUALIZADA DE "dados_censitarios", AQUI, É FEITA A UNIÃO ENTRE OS DADOS CENSITÁRIOS COM OS SHAPEFILES DOS SETORES CENSITÁRIOS DE BRASÍLIA. ALÉM DISSO, TAMBÉM SÃO FEITAS COMPARAÇÕES ENTRE AS MALHAS CENSITÁRIAS DE 2000 E 2010, PORQUE OS SETORES CENSITÁRIOS SÃO DIFERENTES ENTRE ESSES DOIS ANOS: NOVOS SETORES FORAM CRIADOS EM 2010 E AS GEOMETRIAS DOS SETORES QUE PERMANECEM DE 2000 PARA 2010 SÃO DIFERENTES.

# Função para baixar a planilha de dados associadas aos setores censitários de Brasília
get_dados_censitarios <- function() {
      # Função para baixar e ler arquivos Excel do GitHub
      baixar_e_ler_excel <- function(url) {
        temp_file <- tempfile(fileext = ".xls")  # Criar um arquivo temporário
        download.file(url, temp_file, mode = "wb")  # Baixar o arquivo
        read_excel(temp_file)  # Ler o arquivo Excel
      }
      
      # URLs dos arquivos
      url_2010 <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/dados_censitarios/censo_DF_2010_RAW.XLS"
      url_2010_1 <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/dados_censitarios/ResponsavelRenda_DF.XLS"
      url_2000 <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/dados_censitarios/censo_DF_2000_RAW.XLS"
      responsavel_2000 <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/dados_censitarios/Responsavel1_DF.XLS"
      pessoa_2000 <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/dados_censitarios/Pessoa1_DF.XLS"
      instrucao_2000 <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/dados_censitarios/Instrucao1_DF.XLS"
      
      # Importar os dados
      censo_DF_2010 <- baixar_e_ler_excel(url_2010) %>%
        dplyr::mutate(Cod_setor = as.character(Cod_setor))
      censo_DF_2010_1 <- baixar_e_ler_excel(url_2010_1) %>%
        dplyr::mutate(Cod_setor = as.character(Cod_setor))
      censo_DF_2000 <- baixar_e_ler_excel(url_2000) %>%
        dplyr::mutate(Cod_setor = as.character(Cod_setor))
      censo_DF_2000_responsavel <- baixar_e_ler_excel(responsavel_2000) %>%
        dplyr::mutate(Cod_setor = as.character(Cod_setor))
      censo_DF_2000_pessoa <- baixar_e_ler_excel(pessoa_2000) %>%
        dplyr::mutate(Cod_setor = as.character(Cod_setor))
      censo_DF_2000_instrucao <- baixar_e_ler_excel(instrucao_2000) %>%
        dplyr::mutate(Cod_setor = as.character(Cod_setor))
      
      # Retornar lista com os data frames
      list(
        censo_DF_2010 = censo_DF_2010,
        censo_DF_2010_1 = censo_DF_2010_1,
        censo_DF_2000 = censo_DF_2000,
        censo_DF_2000_responsavel = censo_DF_2000_responsavel,
        censo_DF_2000_pessoa = censo_DF_2000_pessoa,
        censo_DF_2000_instrucao = censo_DF_2000_instrucao
      )
}

# Unir os dados e os shapefiles do censo de 2000
unir_censo_2000 <- function(censo_DF_2000, censo_DF_2000_responsavel, censo_DF_2000_pessoa, censo_DF_2000_instrucao) {
      # Ler malha urbana
      censo_bsb_2000 <- read_census_tract(code_tract = "DF", year = 2000)
      
      # Ler malha rural e selecionar colunas importantes
      censo_bsb_2000_rural <- read_census_tract(code_tract = "DF", year = 2000, zone = "rural") %>%
        st_as_sf() %>%
        dplyr::select(code_tract, code_muni, code_state, geom)
      
      # Unir urbano + rural
      censo_bsb_2000_completo <- rbind(censo_bsb_2000_rural, censo_bsb_2000)
      
      # Fazer join dos dados do censo no shapefile completo
      censo_bsb_2000_completo <- censo_bsb_2000_completo %>%
        left_join(
          censo_DF_2000 %>% dplyr::select(Cod_setor, Var01, Var02, Var03, Var05, Var06, Var12),
          by = c("code_tract" = "Cod_setor")
        ) %>%
        left_join(censo_DF_2000_responsavel %>% dplyr::select(Cod_setor, V0595, V0611),
                  by = c("code_tract" = "Cod_setor")) %>%
        left_join(censo_DF_2000_pessoa %>% dplyr::select(Cod_setor, V1461, V1462, V1463, V1464),
                  by = c("code_tract" = "Cod_setor")) %>% 
        left_join(censo_DF_2000_instrucao %>% dplyr::select(Cod_setor, V2249),
                  by = c("code_tract" = "Cod_setor")) %>%
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
          analfabetos = V2249
        )
      return(censo_bsb_2000_completo)
}

################################################################################

# Unir os dados e o shapefile do censo de 2010
unir_censo_2010 <- function(censo_DF_2010, censo_DF_2010_1) {
      ### Shapefile da malha censitária de 2010.
      censo_bsb_2010 <- read_census_tract(code_tract = "DF" , year = 2010)
      
      # Adicionando os dados ao shapefile de 2010
      censo_bsb_2010 <- censo_bsb_2010 %>%
        left_join(censo_DF_2010 %>% dplyr::select(Cod_setor, V001, V002),
                  by = c("code_tract" = "Cod_setor")) %>%
        left_join(censo_DF_2010_1 %>% dplyr::select(Cod_setor, V021, V022),
                  by = c("code_tract" = "Cod_setor")) %>%
        rename(domicilios = V001,
               pop = V002,
               renda_positiva = V021,
               renda_total = V022) %>%
        mutate(
          # Algumas linhas das variáveis abaixo possuem valor "X", substitui "X" por "0" (como string), depois converti os valores das variáveis para número
          renda_total = parse_number(
            if_else(renda_total == "X", "0", renda_total),
            locale = locale(decimal_mark = ",")
          ),
          renda_positiva = parse_number(
            if_else(renda_positiva == "X", "0", renda_positiva),
            locale = locale(decimal_mark = ",")
          ),
        )
}