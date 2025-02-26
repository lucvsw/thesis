library(terra)
library(sf)
library(ggplot2)

# Base file path for the files
base_path <- "/Users/lucas/Desktop/Lucas/Doutorado/Tese/R/Dados/Nighttime lights/DMSP/"

# List of available years
years <- 1992:2013

# Loop to process each file
for (year in years) {
  
  # Construct the file path
  file_path <- file.path(base_path, paste0("DMSP_", year, ".v4/", year, ".tif"))
  
  # Check if the file exists before trying to load it
  if (file.exists(file_path)) {
    
    # Read the raster file
    nightlights <- rast(file_path)
    
    # Transform the raster to Mollweide projection
    nightlights_moll <- project(nightlights, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # Crop and mask using the Brasília shapefile
    nightlights_bsb <- crop(nightlights_moll, vect(bsb_sf))  # Adjust the extent
    nightlights_bsb <- mask(nightlights_bsb, vect(bsb_sf))    # Keep only Brasília
    
    # Dynamically assign the object name
    assign(paste0("nightlights_", year), nightlights_bsb)
    
    # Progress message
    print(paste("Processed:", year))
    
  } else {
    warning(paste("File not found for year:", year))
  }
}

################################################################

### PLOTS ###
# Define sample size
size.sample <- 2 * 10^5

# Create a list to store the plots
plots_list <- list()

# Loop to generate plots for each year
for (year in years) {
  
  # Check if the raster object for the year exists
  raster_name <- paste0("nightlights_", year)
  
  if (exists(raster_name)) {
    
    # Retrieve the corresponding raster
    nightlights_bsb <- get(raster_name)
    
    # Sample the raster data
    df_raster <- as.data.frame(spatSample(nightlights_bsb, size.sample, xy = TRUE, values = TRUE))
    colnames(df_raster) <- c("x", "y", "value")  # Rename columns
    
    # Create the plot
    p <- ggplot() +
      geom_tile(data = df_raster, aes(x = x, y = y, fill = value), color = NA) +
      scale_fill_gradient(low = "black", high = "white", trans = "sqrt") +
      geom_sf(data = adm_sf, fill = "transparent", color = "darkgrey", lwd = .2) +
      geom_sf(data = linhas_sf, color = "darkred", lwd = 0.4) +
      theme_void() +
      ggtitle(paste("Nighttime Lights - Brasília", year))
    
    # Store the plot in the list
    plots_list[[as.character(year)]] <- p
    
    # Progress message
    print(paste("Map generated for year:", year))
    
  } 
}

# Example: to view the map for 1992
print(plots_list[["1992"]])

##################################################

#### SAVING BRASÍLIA TIF FILES ####
# Create output folder (if necessary)
output_dir <- "/Users/lucas/Desktop/Lucas/Doutorado/Tese/R/Dados/Nighttime lights/DMSP/Brasilia"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Loop to save each raster as a TIF file
for (year in years) {
  
  # Raster object name
  raster_name <- paste0("nightlights_", year)
  
  # Retrieve the corresponding raster
  nightlights_bsb <- get(raster_name)
  
  # Define the output file path
  output_file <- file.path(output_dir, paste0("nightlights_bsb_", year, ".tif"))
  
  # Save the raster as a TIF (using filetype instead of format)
  writeRaster(nightlights_bsb, filename = output_file, filetype = "GTiff", overwrite = TRUE)
  
  # Progress message
  print(paste("File saved:", output_file))
}

# These cropped tif files for Brasilia will make future manipulations easy. These files can be found on my GitHub.