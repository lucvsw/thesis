# =============================================================================
# 4b_urbanizacao_mapbiomas.R — Adiciona proporção de área urbana (MapBiomas)
#                              por setor censitário ao censo_final
# =============================================================================

adicionar_prop_urbana <- function(censo_final) {

  library(terra)

  r2000_full <- rast(here::here("dados/MapBiomas - Urbanizacao/2000.tif"))
  r2010_full <- rast(here::here("dados/MapBiomas - Urbanizacao/2010.tif"))

  ext_df <- ext(-48.28, -47.28, -16.08, -15.48)
  r2000 <- crop(r2000_full, ext_df)
  r2010 <- crop(r2010_full, ext_df)
  rm(r2000_full, r2010_full); gc()

  # Binarizar: 1 = Infraestrutura Urbana (classe 24), 0 = demais, NA = nodata (0)
  urb2000 <- ifel(r2000 == 0, NA, ifel(r2000 == 24, 1L, 0L))
  urb2010 <- ifel(r2010 == 0, NA, ifel(r2010 == 24, 1L, 0L))

  censo_wgs <- st_transform(censo_final, 4326)

  setores_2000 <- censo_wgs %>% filter(ano == 2000)
  setores_2010 <- censo_wgs %>% filter(ano == 2010)

  prop_2000 <- terra::extract(urb2000, vect(setores_2000), fun = "mean", na.rm = TRUE)
  prop_2010 <- terra::extract(urb2010, vect(setores_2010), fun = "mean", na.rm = TRUE)

  prop_df <- bind_rows(
    setores_2000 %>% st_drop_geometry() %>%
      select(code_tract, ano) %>%
      mutate(prop_urbano_mapbiomas = prop_2000[, 2]),
    setores_2010 %>% st_drop_geometry() %>%
      select(code_tract, ano) %>%
      mutate(prop_urbano_mapbiomas = prop_2010[, 2])
  )

  censo_final %>%
    left_join(prop_df, by = c("code_tract", "ano"))
}
