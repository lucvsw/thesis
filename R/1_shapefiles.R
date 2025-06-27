# Script para carregar os shapefiles a serem utilizados
## O sistema de referência espacial (CRS) utilizado é SIRGAS 2000 / UTM zone 23S (EPSG: 31983), que mede a distância em metros

# Carregar estações de metrô
get_estacoes_sf <- function() {
  estacoes_sf <- st_read(here("shapefiles", "estacao_de_metro", "estacao_de_metro.shp"))
  return(estacoes_sf)
}

# Carregar linhas de metro
get_linhas_sf <- function() {
  linhas_sf <- st_read(here("shapefiles", "linha_de_metro", "linha_de_metro.shp"))
  return(linhas_sf)
}

# Carregar projeto descartado do metro
get_projeto <- function() {
  projeto_metro_sf <- st_read(here("shapefiles", "projeto_metro", "POLYLINE.shp"))
  return(projeto_metro_sf)
}

# Carregar setores censitários de 2000
get_censo_sf_2000 <- function() {
  # Shapefile do censo de 2000 - rural
  censo_sf_2000_rural <- read_census_tract(code_tract = "DF", year = 2000, zone = "rural") %>%
    select(-zone)
  
  # Shapefile do censo de 2000 - urbano
  censo_sf_2000_urbano <- read_census_tract(code_tract = "DF", year = 2000)
  
  # Unindo os dois shapefiles
  censo_2000_completo <- rbind(censo_sf_2000_rural, censo_sf_2000_urbano)
  
  # Utilizando CRS padrão
  crs_padrao <- 31983
  censo_sf_2000_completo <- st_transform(censo_2000_completo, crs_padrao)
  
  # Excluindo colunas desnecessárias
  censo_sf_2000_completo <- censo_sf_2000_completo %>%
    select(-code_muni, -code_state)
  
  return(censo_sf_2000_completo)
}

# Carregar setores censitários de 2010
get_censo_sf_2010 <- function() {
  # Obtendo os shapefiles do geobr
  censo_sf_2010 <- read_census_tract(code_tract = "DF", year = 2010)
  
  # Utilizando o CRS padrão
  crs_padrao <- 31983
  censo_sf_2010 <- st_transform(censo_sf_2010, crs_padrao)
  
  # Excluindo colunas desnecessárias
  censo_sf_2010 <- censo_sf_2010 %>%
    select(-zone, -code_muni, -name_muni, -name_neighborhood, -code_neighborhood, -code_subdistrict, -code_subdistrict, -name_district, -code_district, -code_state, -name_subdistrict) 
  
  return(censo_sf_2010)
}
