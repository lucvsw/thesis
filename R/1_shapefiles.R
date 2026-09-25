# Script to load the shapefiles used in the analysis
## The coordinate reference system (CRS) is SIRGAS 2000 / UTM zone 23S (EPSG: 31983), which measures distances in meters

# Load subway stations
# (the source shapefile uses Portuguese field names; they are renamed to English here)
get_stations_sf <- function() {
  stations_sf <- st_read(here("data", "raw", "shapefiles", "subway_stations", "subway_stations.shp")) %>%
    rename(station_id = mto_num_es, station_name = mto_nome_e, status = mto_situac, remarks = mto_observ)
  return(stations_sf)
}

# Load subway lines
get_lines_sf <- function() {
  lines_sf <- st_read(here("data", "raw", "shapefiles", "subway_lines", "subway_lines.shp"))
  return(lines_sf)
}

# Load the planned (discarded) subway alignment
get_planned_alignment <- function() {
  planned_alignment_sf <- st_read(here("data", "raw", "shapefiles", "planned_alignment", "planned_alignment.shp"))
  return(planned_alignment_sf)
}

# Load administrative regions (RAs)
# (the source shapefile uses Portuguese field names; they are renamed to English here)
get_RAs <- function() {
  RAs_sf <- st_read(here("data", "raw", "shapefiles", "administrative_regions", "administrative_regions.shp")) %>%
    rename(ra_id = ra_cira, ra_code = ra_codigo, ra_name = ra_nome, ra_area_km2 = ra_areakm2)
  return(RAs_sf)
}

# Load highways
get_highways_sf <- function() {
  highways_sf <- st_read(here("data", "raw", "shapefiles", "highways", "highways.shp"))
  return(highways_sf)
}

# Load census tracts for 2000
get_census_sf_2000 <- function() {
  # 2000 census shapefile - rural
  census_sf_2000_rural <- read_census_tract(code_tract = "DF", year = 2000, zone = "rural") %>%
    select(-zone)
  
  # 2000 census shapefile - urban
  census_sf_2000_urban <- read_census_tract(code_tract = "DF", year = 2000)
  
  # Binding the two shapefiles
  census_2000_full <- rbind(census_sf_2000_rural, census_sf_2000_urban)
  
  # Using the default CRS
  default_crs <- 31983
  census_sf_2000_full <- st_transform(census_2000_full, default_crs)
  
  # Dropping unnecessary columns
  census_sf_2000_full <- census_sf_2000_full %>%
    select(-code_muni, -code_state)
  
  return(census_sf_2000_full)
}

# Load census tracts for 2010
get_census_sf_2010 <- function() {
  # Getting the shapefiles from geobr
  census_sf_2010 <- read_census_tract(code_tract = "DF", year = 2010)
  
  # Using the default CRS
  default_crs <- 31983
  census_sf_2010 <- st_transform(census_sf_2010, default_crs)
  
  # Dropping unnecessary columns
  census_sf_2010 <- census_sf_2010 %>%
    select(-zone, -code_muni, -name_muni, -name_neighborhood, -code_neighborhood, -code_subdistrict, -code_subdistrict, -name_district, -code_district, -code_state, -name_subdistrict) 
  
  return(census_sf_2010)
}
