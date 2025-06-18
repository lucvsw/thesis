### NESTE ARQUIVO, É CRIADO UM DATAFRAME COM IDENTIFICAR TEMPORAL QUE UNE OS DOIS DATAFRAMES DOS CENSOS DE 2000 E 2010 CRIADOS NO ARQUIVO "malha_censitária"

# Unir os dataframes dos censos de 2000 e 2010
preparar_censo_bsb <- function(censo_2000_unido, censo_2010_unido) {
      # Limpar censo de 2000
      censo_2000_unido <- censo_2000_unido %>%
        select(-code_state, -code_muni) %>%
        mutate(ano = 2000)
      
      # Limpar censo de 2010
      censo_2010_unido <- censo_2010_unido %>%
        select(-zone, -code_muni, -name_muni, -name_neighborhood, 
               -code_neighborhood, -code_subdistrict, -name_subdistrict, 
               -code_district, -name_district, -code_state) %>%
        mutate(ano = 2010, 
               renda_percapita = NA,
               renda_media_positiva = NA,
               ens_superior = NA,
               sem_renda = NA,
               idade_65 = NA,
               idade_70 = NA,
               idade_75 = NA,
               idade_80 = NA,
               analfabetos = NA)
      
      # Unir os dados
      censo_bsb_serie_temporal <- bind_rows(censo_2000_unido, censo_2010_unido)
      
      # Remover duplicatas
      censo_bsb_serie_temporal <- censo_bsb_serie_temporal %>%
        distinct(code_tract, ano, pop, geom, .keep_all = TRUE)
      
      # Salvando como gpkg
      st_write(censo_bsb_serie_temporal, "censo_bsb_serie_temporal_mod.gpkg", layer = "censo_bsb", delete_layer = TRUE)
      censo_sf_para_corrigir <- st_read("censo_bsb_serie_temporal_mod.gpkg", layer = "censo_bsb", quiet = TRUE)
      
      return(censo_sf_para_corrigir)
}


# # Filtrar os setores de 2000 e 2010 para verificar quais são os setores de 2000 que se repetem em 2010
# setores_2000 <- censo_bsb_serie_temporal_mod %>% 
#   filter(ano == 2000) %>% 
#   pull(code_tract)
# 
# setores_2010 <- censo_bsb_serie_temporal_mod %>% 
#   filter(ano == 2010) %>% 
#   pull(code_tract)
# 
# # Encontrar os setores que se repetem
# setores_repetidos <- intersect(setores_2000, setores_2010)
# 
# # Contar quantos setores se repetem
# quantidade_repetidos <- length(setores_repetidos)
# quantidade_repetidos
# 
# ## Plots
# censo_2000 <- censo_bsb_serie_temporal_mod %>%
#   filter(ano == 2000)
# # Plot do ano 2000
# ggplot() +
#   geom_sf(data = censo_2000, aes(fill = pop), color = NA) +
#   scale_fill_viridis_c(name = "População", option = "magma", direction = -1, na.value = "gray90") +
#   geom_sf(data = linhas_sf, color = "#03396c", lwd = 0.4) +
#   geom_sf(data = projeto_sf, color = "red", lwd = 0.5) +
#   theme_void() +
#   labs(title = "População por Setor Censitário em 2000") + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
# censo_2010 <- censo_bsb_serie_temporal_mod %>%
#   filter(ano == 2010)
# # Plot do ano 2010
# ggplot() +
#   geom_sf(data = censo_2010, aes(fill = pop), color = NA) +
#   scale_fill_viridis_c(name = "População", option = "magma", direction = -1, na.value = "gray90") +
#   geom_sf(data = linhas_sf, color = "#03396c", lwd = 0.4) +
#   geom_sf(data = projeto_sf, color = "red", lwd = 0.5) +
#   theme_void() +
#   labs(title = "População por Setor Censitário em 2010") + 
#   theme(plot.title = element_text(hjust = 0.5))
