# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("httr", "sf", "ggplot2", "geobr", "readxl", "dplyr", "readr", "units", "tidyr", "AER", "fixest", "magrittr", "here"), # Packages that your targets need for their tasks.
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
  tar_target(estacoes_sf, get_estacoes_sf()),
  tar_target(linhas_sf, get_linhas_sf()),
  tar_target(projeto_metro_sf, get_projeto()),
  tar_target(censo_sf_2000_completo, get_censo_sf_2000()),
  tar_target(censo_sf_2010, get_censo_sf_2010()),
  tar_target(censo_2000_DFs, get_censo_2000_DF()),
  tar_target(censo_2000_completo, unir_dados_sf_2000(censo_sf_2000_completo, censo_2000_DFs)),
  tar_target(censo_2010_DFs, get_censo_2010_DF()),
  tar_target(censo_2010_completo, unir_dados_sf_2010(censo_sf_2010, censo_2010_DFs, censo_2000_completo)),
  tar_target(censo_para_compatibilizar, preparar_para_compatibilizar(censo_2000_completo, censo_2010_completo)),
  tar_target(censo_compatibilizado, uniao_setores(censo_para_compatibilizar))
  # tar_target(censo_com_novas_variaveis, novas_variaveis(censo_compatibilizado, linhas_sf, projeto_metro_sf)),
  # # tar_target(ivreg_2000, regressao_com_ivreg_2000(censo_com_novas_variaveis)),
  # # tar_target(ivreg_2010, regressao_com_ivreg_2010(censo_com_novas_variaveis))
  # tar_target(duplicatas_excluidas, excluir_duplicatas(censo_com_novas_variaveis)),
  # tar_target(censo_final, criar_variacoes(duplicatas_excluidas))
  )
