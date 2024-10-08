#' Generate a grid of points for a specified geographical area or a shapefile
#'
#' @param resolution Numeric. Resolution of the grid.
#' @param state Character. Name of the state.
#' @param country Character. Name of the country.
#' @param shapefile_path Character. Path to the shapefile (if any).
#' @param save Logical. If TRUE, saves the grid points to a CSV file.
#' @param plot Logical. If TRUE, plots the grid points.
#'
#' @return A list containing the grid name and the data frame of grid points.
#' @examples
#' # To generate grid for a state
#' generate_grid(resolution = c(0.01), state = c("São Paulo"), country = "Brazil", save = FALSE, plot = FALSE)
#'
#' # To generate grid for a country
#' generate_grid(resolution = 0.01, country = "Brazil", save = FALSE, plot = TRUE)
#'
#' # To generate grid for a shapefile
#' generate_grid(resolution = 0.01, shapefile_path = "path/to/shapefile.shp", save = FALSE, plot = TRUE)

generate_grid <- function(resolution, state = NULL, country = NULL, shapefile_path = NULL, save = FALSE, plot = FALSE) {
  # Loading packages
  required_packages <- c("sf", "dplyr", "rnaturalearth", "ggplot2")
  sapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  })
  
  # Grid resolution 
  resolution <- as.numeric(resolution)
  
  # Specified geographical area 
  area <- NULL
  grid_name <- NULL
  if (!is.null(shapefile_path)) {
    area <- st_read(shapefile_path)
    grid_name <- paste0("Grid_", resolution, "_Shapefile")
  } else if (!is.null(state) && !is.null(country)) {
    state_data <- ne_states(country = country, returnclass = "sf")
    area <- state_data[state_data$name == state, ]
    if (nrow(area) == 0) stop("No such state found in the data")
    grid_name <- paste0("Grid_", resolution, "_", state, "_", country)
  } else if (!is.null(country)) {
    country_data <- ne_countries(scale = "medium", returnclass = "sf")
    area <- country_data[country_data$admin == country, ]
    if (nrow(area) == 0) stop("No such country found in the data")
    grid_name <- paste0("Grid_", resolution, "_", country)
  } else {
    stop("You must specify either a country, both a state and a country, or a shapefile path")
  }
  
  # Bounding box 
  bbox <- st_bbox(area)
  lon_seq <- seq(from = bbox["xmin"], to = bbox["xmax"], by = resolution)
  lat_seq <- seq(from = bbox["ymin"], to = bbox["ymax"], by = resolution)
  
  # Grid 
  grid_points <- expand.grid(lon = lon_seq, lat = lat_seq)
  grid_points_sf <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)
  grid_points_sp <- st_intersection(grid_points_sf, st_transform(area, crs = 4326))
  
  grid_points_df <- as.data.frame(st_coordinates(grid_points_sp))
  names(grid_points_df) <- c("LON", "LAT")
  
  grid_points_df$LON <- format(as.numeric(grid_points_df$LON), digits = 8, nsmall = 8)
  grid_points_df$LAT <- format(as.numeric(grid_points_df$LAT), digits = 8, nsmall = 8)
  
  # Save
  if (save) {
    write.csv(grid_points_df, file = paste0(grid_name, ".csv"), row.names = FALSE)
  }
  
  # Plot
  if (plot) {
    p <- ggplot() +
      geom_sf(data = area, fill = NA, color = "black") +
      geom_point(data = grid_points_df, aes(x = as.numeric(LON), y = as.numeric(LAT)), color = "blue", size = 0.5) +
      coord_sf() +
      theme_minimal() +
      labs(title = "Grid Points", x = "Longitude", y = "Latitude")
    print(p)
  }
  
  assign(grid_name, grid_points_df, envir = .GlobalEnv)
  
  invisible(list(grid_name = grid_name, grid_points_df = grid_points_df))
}



