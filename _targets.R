# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("httr", "sf", "ggplot2", "geobr", "readxl", "dplyr", "readr", "units", "tidyr", "AER", "fixest", "magrittr"), # Packages that your targets need for their tasks.
  format = "rds", # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(
  files = list.files("R", pattern = "\\.R$", full.names = TRUE),
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
)

# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(estacoes_sf, get_estacoes()),
  tar_target(linhas_sf, get_linhas()),
  tar_target(RAs_sf, get_RAs()),
  tar_target(projeto_metro_sf, get_projeto()),
  tar_target(setores2010_sf, get_setores2010()),
  tar_target(setores2000_sf, st_transform(get_setores2000(), st_crs(setores2010_sf))),
  tar_target(setores2000_rurais_sf, st_transform(get_setores2000_rurais(), st_crs(setores2010_sf))),
  tar_target(brasilia_sf, get_brasilia()),
  tar_target(dados_censitarios_raw, get_dados_censitarios()),
  tar_target(censo_DF_2010, dados_censitarios_raw$censo_DF_2010),
  tar_target(censo_DF_2010_1, dados_censitarios_raw$censo_DF_2010_1),
  tar_target(censo_DF_2000, dados_censitarios_raw$censo_DF_2000),
  tar_target(censo_DF_2000_responsavel, dados_censitarios_raw$censo_DF_2000_responsavel),
  tar_target(censo_DF_2000_pessoa, dados_censitarios_raw$censo_DF_2000_pessoa),
  tar_target(censo_DF_2000_instrucao, dados_censitarios_raw$censo_DF_2000_instrucao),
  tar_target(censo_2000_unido, unir_censo_2000(censo_DF_2000, censo_DF_2000_responsavel, censo_DF_2000_pessoa, censo_DF_2000_instrucao)),
  tar_target(censo_2010_unido, unir_censo_2010(censo_DF_2010, censo_DF_2010_1)),
  tar_target(censo_unido, preparar_censo_bsb(censo_2000_unido, censo_2010_unido)),
  #tar_target(obter_censo_unido, get_censo_unido()),
  tar_target(censo_compatibilizado, uniao_setores(censo_unido)),
  tar_target(censo_com_novas_variaveis, novas_variaveis(censo_compatibilizado, linhas_sf, projeto_metro_sf)),
  tar_target(ivreg_2000, regressao_com_ivreg_2000(censo_com_novas_variaveis)),
  tar_target(ivreg_2010, regressao_com_ivreg_2010(censo_com_novas_variaveis))
  )
