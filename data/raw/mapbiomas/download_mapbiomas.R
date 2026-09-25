# Fetches the MapBiomas Collection 9 land-cover rasters (2000 and 2010) used by the
# urbanization targets (R/4b_mapbiomas_urbanization.R).
#
# Only the window that covers the Federal District is read from the public MapBiomas
# bucket (about 1 MB per year); the national rasters (about 1 GB each) are not downloaded.
#
# Source: MapBiomas Brasil, Collection 9, annual land cover and land use
#         https://brasil.mapbiomas.org/en/colecoes-mapbiomas/
# Run from the repository root:  Rscript data/raw/mapbiomas/download_mapbiomas.R

library(terra)

out_dir <- here::here("data", "raw", "mapbiomas")
window  <- ext(-48.28, -47.28, -16.08, -15.48)   # same window used in R/4b_mapbiomas_urbanization.R

for (year in c(2000, 2010)) {
  url <- sprintf(
    "/vsicurl/https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_%d.tif",
    year
  )
  raster <- crop(rast(url), window)
  writeRaster(raster, file.path(out_dir, paste0(year, ".tif")),
              datatype = "INT1U", overwrite = TRUE, gdal = "COMPRESS=LZW")
  message(year, ": saved ", paste(dim(raster)[1:2], collapse = " x "), " pixels")
}
