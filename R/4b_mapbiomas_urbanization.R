# =============================================================================
# 4b_mapbiomas_urbanization.R — Adds the urban-area share (MapBiomas)
#                              by census tract to census_final
# =============================================================================

add_urban_share <- function(census_final) {

  library(terra)

  r2000_full <- rast(here::here("data", "raw", "mapbiomas", "2000.tif"))
  r2010_full <- rast(here::here("data", "raw", "mapbiomas", "2010.tif"))

  ext_df <- ext(-48.28, -47.28, -16.08, -15.48)
  r2000 <- crop(r2000_full, ext_df)
  r2010 <- crop(r2010_full, ext_df)
  rm(r2000_full, r2010_full); gc()

  # Binarize: 1 = Urban Infrastructure (class 24), 0 = other, NA = nodata (0)
  urb2000 <- ifel(r2000 == 0, NA, ifel(r2000 == 24, 1L, 0L))
  urb2010 <- ifel(r2010 == 0, NA, ifel(r2010 == 24, 1L, 0L))

  census_wgs <- st_transform(census_final, 4326)

  tracts_2000 <- census_wgs %>% filter(year == 2000)
  tracts_2010 <- census_wgs %>% filter(year == 2010)

  share_2000 <- terra::extract(urb2000, vect(tracts_2000), fun = "mean", na.rm = TRUE)
  share_2010 <- terra::extract(urb2010, vect(tracts_2010), fun = "mean", na.rm = TRUE)

  share_df <- bind_rows(
    tracts_2000 %>% st_drop_geometry() %>%
      select(code_tract, year) %>%
      mutate(share_urban_mapbiomas = share_2000[, 2]),
    tracts_2010 %>% st_drop_geometry() %>%
      select(code_tract, year) %>%
      mutate(share_urban_mapbiomas = share_2010[, 2])
  )

  census_final %>%
    left_join(share_df, by = c("code_tract", "year"))
}
