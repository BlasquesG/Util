# use example

# soil_data <- download_soil_data(
#  grid = Grid_0.01_São.Paulo_Brazil,
#  variables = c("bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "cec", "phh2o", "sand", "silt", "soc"),
#  depth_ranges = c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"),
#  save = TRUE
#)

download_soil_data <- function(grid, variables, depth_ranges, save = FALSE) {
  
  install_and_load <- function(package) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
  
  
  required_packages <- c("httr", "dplyr", "foreach", "doParallel", "raster")
  
  
  lapply(required_packages, install_and_load)
  
  
  generate_soil_data_url <- function(variable, depth_range, lat_min, lat_max, lon_min, lon_max) {
    base_url <- "https://maps.isric.org/mapserv?map=/map/%s.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s_%s_mean&FORMAT=image/tiff&SUBSET=long(%f,%f)&SUBSET=lat(%f,%f)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
    url <- sprintf(base_url, variable, variable, depth_range, lon_min, lon_max, lat_min, lat_max)
    return(url)
  }
  
  
  lat_min <- min(grid$LAT, na.rm = TRUE)
  lat_max <- max(grid$LAT, na.rm = TRUE)
  lon_min <- min(grid$LON, na.rm = TRUE)
  lon_max <- max(grid$LON, na.rm = TRUE)
  
  
  output_dir <- "soil_data"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  
  
  foreach(variable = variables, .packages = c("httr", "dplyr")) %:%
    foreach(depth_range = if (variable == "ocs") "0-30cm" else depth_ranges, .packages = c("httr")) %dopar% {
      url <- generate_soil_data_url(variable, depth_range, lat_min, lat_max, lon_min, lon_max)
      
      
      output_file <- file.path(output_dir, sprintf("%s_%s_mean.tiff", variable, depth_range))
      
      
      response <- GET(url)
      if (status_code(response) == 200) {
        writeBin(content(response, "raw"), output_file)
        sprintf("Data downloaded and saved to %s", output_file)
      } else {
        warning(sprintf("Failed to download data for variable: %s at depth range: %s", variable, depth_range))
      }
    }
  
  
  extract_soil_data <- function(grid, variables, depth_ranges, output_dir) {
    results <- data.frame(LAT = grid$LAT, LON = grid$LON)
    
    extracted_values <- foreach(variable = variables, .combine = cbind, .packages = c("raster", "dplyr"), .export = c("generate_column_name")) %:%
      foreach(depth_range = if (variable == "ocs") "0-30cm" else depth_ranges, .combine = cbind, .packages = c("raster")) %dopar% {
        
        tiff_file <- file.path(output_dir, sprintf("%s_%s_mean.tiff", variable, depth_range))
        
        if (file.exists(tiff_file)) {
          
          raster_data <- raster(tiff_file)
          
          
          coords <- cbind(grid$LON, grid$LAT)
          values <- extract(raster_data, coords)
          
          
          values[values == 0] <- NA
          
          
          col_name <- generate_column_name(variable, depth_range)
          setNames(data.frame(values), col_name)
        } else {
          warning(sprintf("File not found: %s", tiff_file))
          col_name <- generate_column_name(variable, depth_range)
          setNames(data.frame(rep(NA, nrow(grid))), col_name)
        }
      }
    
    results <- cbind(results, extracted_values)
    
    return(results)
  }
  
  
  soil_data <- extract_soil_data(grid, variables, depth_ranges, output_dir)
  
  
  unlink(output_dir, recursive = TRUE)
  
  
  stopCluster(cl)
  
  
  if (save) {
    write.csv(soil_data, "soil_data_extracted.csv", row.names = FALSE)
    message("Dataframe saved to soil_data_extracted.csv")
  }
  
  
  return(soil_data)
}
