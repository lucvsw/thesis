# URLs para os arquivos do shapefile de ESTAÇÕES de METRO no GitHub
get_estacoes <- function() {
  # URLs para os arquivos do shapefile de ESTAÇÕES de METRO no GitHub
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/estacao_de_metro/"
  files <- c("estacao_de_metro.shp",
             "estacao_de_metro.shx",
             "estacao_de_metro.dbf",
             "estacao_de_metro.prj")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "estacao_de_metro.shp")
  st_read(shapefile_path, quiet = TRUE) |>
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# URLs para os arquivos do shapefile de LINHAS de METRO no GitHub
get_linhas <- function() {
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/linha_de_metro/"
  files <- c("linha_de_metro.shp",
             "linha_de_metro.shx",
             "linha_de_metro.dbf",
             "linha_de_metro.prj")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "linha_de_metro.shp")
  st_read(shapefile_path, quiet = TRUE) |>
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# URLs para os arquivos do shapefile das regiões administrativas
get_RAs <- function() {
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/regioes_administrativas/"
  files <- c("regioes_administrativas.shp",
             "regioes_administrativas.shx",
             "regioes_administrativas.dbf",
             "regioes_administrativas.prj")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "regioes_administrativas.shp")
  st_read(shapefile_path, quiet = TRUE) |>
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# URLs para os arquivos do shapefile do PROJETO do METRÔ do Distrito Federal
get_projeto <- function() {
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/projeto_metro/"
  files <- c("POLYLINE.shp",
             "POLYLINE.shx",
             "POLYLINE.dbf",
             "POLYLINE.prj")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "POLYLINE.shp")
  st_read(shapefile_path, quiet = TRUE) |>
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# URLs para os arquivos do shapefile dos setores censitários do Distrito Federal em 2010
get_setores2010 <- function() {
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/shapefile_setores_censitarios_2010/"
  files <- c("censo_2010.shp",
             "censo_2010.shx",
             "censo_2010.dbf",
             "censo_2010.prj")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "censo_2010.shp")
  st_read(shapefile_path, quiet = TRUE) |>
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# URLs para os arquivos do shapefile dos setores censitários do Distrito Federal em 2000
get_setores2000 <- function() {
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/shapefile_setores_censitarios_2000/"
  files <- c("censo_2000.shp",
             "censo_2000.shx",
             "censo_2000.dbf",
             "censo_2000.prj")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "censo_2000.shp")
  
  # Leitura e correção do CRS de origem (SIRGAS 2000 / UTM 23S)
  st_read(shapefile_path, quiet = TRUE) |>
    st_set_crs(31983) |> #EPSG do SIRGAS 2000 / UTM 23S
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# URLs para os arquivos do shapefile dos setores censitários RURAIS do Distrito Federal em 2000
get_setores2000_rurais <- function() {
  base_url <- "https://raw.githubusercontent.com/lucvsw/Doutorado/main/tese/files/shapefile_setores_censitarios_rural_2000/"
  files <- c("53SE500G.shp",
             "53SE500G.shx",
             "53SE500G.dbf")
  
  # Função para baixar e salvar arquivos temporariamente
  download_shapefile <- function(url, destfile) {
    GET(url, write_disk(destfile, overwrite = TRUE))
  }
  
  # Criar um diretório temporário
  temp_dir <- tempdir()
  
  # Baixar cada arquivo do shapefile
  lapply(files, function(file) {
    download_shapefile(paste0(base_url, file), file.path(temp_dir, file))
  })
  
  # Ler o shapefile usando sf::st_read
  shapefile_path <- file.path(temp_dir, "53SE500G.shp")
  st_read(shapefile_path, quiet = TRUE) |>
    st_set_crs(4326) |>
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

##################################################################

# Shapefile de Brasília utilizando geobr
get_brasilia <- function(crs_target = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") {
  read_municipality(code_muni = 5300108, year = 2015, simplified = TRUE, showProgress = FALSE) |>
    st_transform(crs = crs_target) #Transformar as coordenadas para a projeção Mollweide (ESRI:54009)
}