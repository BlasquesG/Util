#' Generate a grid of points for a specified geographical area or shapefile
#'
#' @param resolution Numeric. Resolution of the grid (distance between points in degrees).
#' @param state Character. Name of the state (optional, must be paired with a country).
#' @param country Character. Name of the country (optional).
#' @param shapefile_path Character. Path to the shapefile (optional, overrides state and country if provided).
#' @param save Logical. If TRUE, saves the generated grid points to a CSV file.
#' @param plot Logical. If TRUE, plots the grid points over the area of interest.
#' @param crs Numeric. Coordinate reference system (default is 4326 for WGS84).
#'
#' @return A list containing the grid name and the data frame of grid points (latitude and longitude).
#' @examples
#' # Generate a grid for a specific state
#' generate_grid(resolution = 0.01, state = "São Paulo", country = "Brazil", save = FALSE, plot = FALSE)
#'
#' # Generate a grid for an entire country
#' generate_grid(resolution = 0.01, country = "Brazil", save = FALSE, plot = TRUE)
#'
#' # Generate a grid using a shapefile
#' generate_grid(resolution = 0.01, shapefile_path = "path/to/shapefile.shp", save = TRUE, plot = TRUE)



TPE_grid <- function(resolution, state = NULL, country = NULL, shapefile_path = NULL, save = FALSE, plot = FALSE, crs = 4326) {
  
  required_packages <- c("sf", "dplyr", "rnaturalearth", "ggplot2")
  sapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  })
  
  
  resolution <- as.numeric(resolution)
  
  
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
  
  
  bbox <- st_bbox(area)
  lon_seq <- seq(from = bbox["xmin"], to = bbox["xmax"], by = resolution)
  lat_seq <- seq(from = bbox["ymin"], to = bbox["ymax"], by = resolution)
  
  grid_points <- NULL
  for (i in seq_along(lon_seq)) {
    grid_points <- rbind(grid_points, expand.grid(lon = lon_seq[i], lat = lat_seq))
  }
  
  grid_points_sf <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)
  
  
  grid_points_sp <- st_intersection(grid_points_sf, st_transform(area, crs = 4326))
  
  
  if (!is.null(crs)) {
    grid_points_sp <- st_transform(grid_points_sp, crs = crs)
  }
  
  
  grid_points_df <- as.data.frame(st_coordinates(grid_points_sp))
  names(grid_points_df) <- c("LON", "LAT")
  grid_points_df$LON <- format(as.numeric(grid_points_df$LON), digits = 8, nsmall = 8)
  grid_points_df$LAT <- format(as.numeric(grid_points_df$LAT), digits = 8, nsmall = 8)
  
  
  if (save) {
    write.csv(grid_points_df, file = paste0(grid_name, ".csv"), row.names = FALSE)
  }
  
  if (plot) {
    p <- ggplot() +
      geom_sf(data = st_transform(area, crs = crs), fill = NA, color = "black") +
      geom_point(data = grid_points_df, aes(x = as.numeric(LON), y = as.numeric(LAT)), color = "blue", size = 0.5) +
      coord_sf(crs = crs) +
      theme_minimal() +
      labs(title = "Grid Points", x = "Longitude", y = "Latitude")
    print(p)
  }
  
  
  assign(grid_name, grid_points_df, envir = .GlobalEnv)
  invisible(list(grid_name = grid_name, grid_points_df = grid_points_df))
}
