library(targets) 

# Opções target:
tar_option_set(
  packages = c("httr", "sf", "ggplot2", "geobr", "readxl", "dplyr", "readr", "units", "tidyr", "AER", "fixest", "magrittr", "here", "terra", "censobr", "arrow"), # Pacotes.
  format = "rds", 
)

tar
tar_source(
  files = list.files("R", pattern = "\\.R$", full.names = TRUE),
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
)

# Target list:
list(
  tar_target(estacoes_sf, get_estacoes_sf()),
  tar_target(linhas_sf, get_linhas_sf()),
  tar_target(projeto_metro_sf, get_projeto()),
  tar_target(RAs_sf, get_RAs()),
  tar_target(rodovias_sf, get_rodovias_sf()),
  tar_target(censo_sf_2000_completo, get_censo_sf_2000()),
  tar_target(censo_sf_2010, get_censo_sf_2010()),
  tar_target(censo_2000_DFs, get_censo_2000_DF()),
  tar_target(raca_2000_DF, get_raca_2000_DF()),
  tar_target(censo_2000_completo, unir_dados_sf_2000(censo_sf_2000_completo, censo_2000_DFs, raca_2000_DF)),
  tar_target(censo_2010_DFs, get_censo_2010_DF()),
  tar_target(censo_2010_completo, unir_dados_sf_2010(censo_sf_2010, censo_2010_DFs, censo_2000_completo)),
  tar_target(censo_para_compatibilizar, preparar_para_compatibilizar(censo_2000_completo, censo_2010_completo)),
  tar_target(censo_compatibilizado, uniao_setores(censo_para_compatibilizar)),
  tar_target(censo_com_novas_variaveis, novas_variaveis(censo_compatibilizado, linhas_sf, projeto_metro_sf, estacoes_sf, RAs_sf, rodovias_sf)),
  tar_target(duplicatas_excluidas, excluir_duplicatas(censo_com_novas_variaveis)),
  tar_target(censo_variacoes, criar_variacoes(duplicatas_excluidas)),
  tar_target(censo_final, criar_coeficientes(censo_variacoes)),
  tar_target(tabela_descritiva, tabela_estatisticas_descr(censo_final)),
  tar_target(resultados_importancia_covariaveis, analise_importancia_covariaveis(censo_final)),
  tar_target(amostra_regressoes, preparar_amostra_regressoes(censo_final)),
  tar_target(resultados_regressoes, rodar_regressoes(amostra_regressoes)),
  tar_target(resultados_robustez_amostragem, robustez_amostragem(amostra_regressoes)),
  tar_target(resultados_robustez_threshold, robustez_threshold(amostra_regressoes)),
  tar_target(resultados_robustez_intensidade, robustez_intensidade(amostra_regressoes)),

  # Heterogeneidade por RA (10_heterogeneidade_RAs.R)
  tar_target(resultados_heterog_RAs,  rodar_heterogeneidade_RAs(amostra_regressoes)),
  tar_target(resultados_iv_por_RA,    rodar_iv_por_RA(amostra_regressoes)),

  # Sorting: população, domicílios, renda por domicílio, prop. apartamentos (11_sorting.R)
  tar_target(resultados_populacao,        rodar_regressoes_populacao(amostra_regressoes)),
  tar_target(resultados_domicilios,       rodar_regressoes_domicilios(amostra_regressoes, censo_final)),
  tar_target(resultados_renda_domicilio,  rodar_regressoes_renda_domicilio(amostra_regressoes, censo_final)),
  tar_target(resultados_prop_apt,         rodar_regressoes_prop_apartamentos(amostra_regressoes, censo_final)),

  # Urbanização MapBiomas (4b + 12)
  tar_target(censo_urbanizacao,              adicionar_prop_urbana(censo_final)),
  tar_target(amostra_urbanizacao,            preparar_amostra_urbanizacao(censo_urbanizacao)),
  tar_target(resultados_regressoes_urbanizacao, rodar_regressoes_urbanizacao(amostra_urbanizacao)),

  # Hipótese 2 — Efeito de composição / gentrificação (13)
  tar_target(amostra_mecanismos,             preparar_amostra_mecanismos(censo_final)),
  tar_target(resultados_regressoes_mecanismos, rodar_regressoes_mecanismos(amostra_mecanismos))
  )
