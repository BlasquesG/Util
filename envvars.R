#' Retrieve NASA POWER Data for Specified Locations
#'
#' @param startday Character. Start date in YYYYMMDD format.
#' @param endday Character. End date in YYYYMMDD format.
#' @param latitudes Numeric vector. Latitudes of the points.
#' @param longitudes Numeric vector. Longitudes of the points.
#' @param TbaseInf Numeric. Lower base temperature for degree days calculation.
#' @param TbaseSup Numeric. Upper base temperature for degree days calculation.
#' @param save_csv Logical. If TRUE, saves the data to a CSV file.
#' @param DoMean Logical. If TRUE, calculates the mean for each location.
#' @param selected_parameters Character vector. Parameters to retrieve from NASA POWER API. If NULL, all available parameters are retrieved.
#'
#' @return A data frame containing the retrieved data with calculated variables.
#' @examples
#' # Retrieve data for a specific grid of points in Brazil
#' get_NASA_features(startday = "20000101", endday = "20240630", 
#'                   latitudes = Grid_0.5_Brazil$LAT, 
#'                   longitudes = Grid_0.5_Brazil$LON, 
#'                   TbaseInf = 10, TbaseSup = 30, 
#'                   save_csv = TRUE, DoMean = TRUE, 
#'                   selected_parameters = NULL)
#'
#' # Retrieve data for a specific grid without saving CSV and without calculating mean
#' get_NASA_features(startday = "20000101", endday = "20240630", 
#'                   latitudes = Grid_0.5_Brazil$LAT, 
#'                   longitudes = Grid_0.5_Brazil$LON, 
#'                   TbaseInf = 10, TbaseSup = 30, 
#'                   save_csv = FALSE, DoMean = FALSE, 
#'                   selected_parameters = c("T2M", "PRECTOTCORR"))


get_NASA_features <- function(startday, endday, latitudes, longitudes, TbaseInf, TbaseSup, save_csv = FALSE, DoMean = FALSE, selected_parameters = NULL) {
  
  required_packages <- c("httr", "dplyr", "readr", "purrr", "stringi", "future.apply")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  
  community <- "ag"
  header <- "true"
  time_standard <- "utc"
  
  
  user <- stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
  
  
  all_parameters <- c("TS", "T2M", "T2MDEW", "T2MWET", "TS_MAX", "T2M_MAX", 
                      "T2M_MIN", "CLOUD_AMT", "T2M_RANGE", "TOA_SW_DWN", 
                      "ALLSKY_SFC_UVA", "ALLSKY_SFC_UVB", "ALLSKY_SRF_ALB", 
                      "ALLSKY_SFC_SW_DNI", "ALLSKY_SFC_SW_DWN", "CLRSKY_SFC_SW_DWN", 
                      "ALLSKY_SFC_PAR_TOT", "ALLSKY_SFC_SW_DIFF", "CLRSKY_SFC_PAR_TOT", 
                      "ALLSKY_SFC_UV_INDEX", "PS", "QV2M", "RH2M", "WD2M", 
                      "WS2M", "WD10M", "WS10M", "TS_MIN", "GWETTOP", "GWETPROF", 
                      "GWETROOT", "WS2M_MAX", "WS2M_MIN", "WS10M_MAX", 
                      "WS10M_MIN", "WS2M_RANGE", "PRECTOTCORR", 
                      "WS10M_RANGE", "ALLSKY_SFC_LW_DWN")
  
  
  param_names <- c(
    "PS" = "Surface Pressure (kPa)",
    "QV2M" = "Specific Humidity at 2 Meters (g/kg)",
    "RH2M" = "Relative Humidity at 2 Meters (%)",
    "WD2M" = "Wind Direction at 2 Meters (Degrees)",
    "WS2M" = "Wind Speed at 2 Meters (m/s)",
    "WD10M" = "Wind Direction at 10 Meters (Degrees)",
    "WS10M" = "Wind Speed at 10 Meters (m/s)",
    "TS_MIN" = "Earth Skin Temperature Minimum (C)",
    "GWETTOP" = "Surface Soil Wetness (1)",
    "GWETPROF" = "Profile Soil Moisture (1)",
    "GWETROOT" = "Root Zone Soil Wetness (1)",
    "WS2M_MAX" = "Wind Speed at 2 Meters Maximum (m/s)",
    "WS2M_MIN" = "Wind Speed at 2 Meters Minimum (m/s)",
    "WS10M_MAX" = "Wind Speed at 10 Meters Maximum (m/s)",
    "WS10M_MIN" = "Wind Speed at 10 Meters Minimum (m/s)",
    "FROST_DAYS" = "Frost Days (Days)",
    "WS2M_RANGE" = "Wind Speed at 2 Meters Range (m/s)",
    "PRECTOTCORR" = "Precipitation Corrected (mm/day)",
    "WS10M_RANGE" = "Wind Speed at 10 Meters Range (m/s)",
    "PRECTOTCORR_SUM" = "Precipitation Corrected Sum (mm)",
    "ALLSKY_SFC_LW_DWN" = "All Sky Surface Longwave Downward Irradiance (W/m^2)",
    "TS" = "Earth Skin Temperature (C)",
    "T2M" = "Temperature at 2 Meters (C)",
    "T2MDEW" = "Dew/Frost Point at 2 Meters (C)",
    "T2MWET" = "Wet Bulb Temperature at 2 Meters (C)",
    "TS_MAX" = "Earth Skin Temperature Maximum (C)",
    "T2M_MAX" = "Temperature at 2 Meters Maximum (C)",
    "T2M_MIN" = "Temperature at 2 Meters Minimum (C)",
    "CLOUD_AMT" = "Cloud Amount (%)",
    "T2M_RANGE" = "Temperature at 2 Meters Range (C)",
    "TOA_SW_DWN" = "Top-Of-Atmosphere Shortwave Downward Irradiance (MJ/m^2/day)",
    "ALLSKY_SFC_UVA" = "All Sky Surface UVA Irradiance (W/m^2)",
    "ALLSKY_SFC_UVB" = "All Sky Surface UVB Irradiance (W/m^2)",
    "ALLSKY_SRF_ALB" = "All Sky Surface Albedo (dimensionless)",
    "ALLSKY_SFC_SW_DNI" = "All Sky Surface Shortwave Downward Direct Normal Irradiance (MJ/m^2/day)",
    "ALLSKY_SFC_SW_DWN" = "All Sky Surface Shortwave Downward Irradiance (MJ/m^2/day)",
    "CLRSKY_SFC_SW_DWN" = "Clear Sky Surface Shortwave Downward Irradiance (MJ/m^2/day)",
    "ALLSKY_SFC_PAR_TOT" = "All Sky Surface PAR Total (W/m^2)",
    "ALLSKY_SFC_SW_DIFF" = "All Sky Surface Shortwave Diffuse Irradiance (MJ/m^2/day)",
    "CLRSKY_SFC_PAR_TOT" = "Clear Sky Surface PAR Total (W/m^2)",
    "ALLSKY_SFC_UV_INDEX" = "All Sky Surface UV Index (dimensionless)"
  )
  
  
  parameters <- if (is.null(selected_parameters)) all_parameters else selected_parameters
  
  
  if (length(startday) > 1 || length(latitudes) > 1) {
    
    results <- lapply(1:length(latitudes), function(i) {
      
      date_range <- paste(startday[i], endday[i], sep = "_")
      
      result <- get_NASA_features(
        startday = startday[i],
        endday = endday[i],
        latitudes = latitudes[i],
        longitudes = longitudes[i],
        TbaseInf = TbaseInf,
        TbaseSup = TbaseSup,
        save_csv = FALSE,
        DoMean = DoMean,
        selected_parameters = selected_parameters
      )
      
      if (!is.null(result)) {
        result <- result %>% mutate(DATE_RANGE = date_range)
      }
      
      return(result)
    })
    
    
    combined_results <- do.call(rbind, results)
    
    
    combined_results <- combined_results %>% select(DATE_RANGE, everything())
    
    if (save_csv) {
      tryCatch({
        write.csv(combined_results, file = "EnvFeatures.csv", row.names = FALSE)
      }, error = function(e) {
        print(paste("Error saving the file:", e$message))
      })
    }
    
    return(combined_results)
  } else {
    
    all_data <- list()
    
    
    plan(multisession, workers = availableCores())
    
    
    process_point <- function(lat, lon) {
      point_data <- list()
      
      
      param_groups <- split(parameters, ceiling(seq_along(parameters) / 20))
      
      for (i in seq_along(param_groups)) {
        params <- paste(param_groups[[i]], collapse = ",")
        
        
        url <- URLencode(paste0("https://power.larc.nasa.gov/api/temporal/daily/point?",
                                "start=", startday,
                                "&end=", endday,
                                "&latitude=", trimws(lat),
                                "&longitude=", trimws(lon),
                                "&community=", community,
                                "&parameters=", params,
                                "&format=csv",
                                "&user=", user,
                                "&header=", header,
                                "&time-standard=", time_standard))
        
        
        attempt <- 1
        max_attempts <- 10
        success <- FALSE
        
        while (attempt <= max_attempts && !success) {
          
          response <- GET(url)
          
          
          if (response$status_code == 200) {
            
            content_text <- content(response, "text", encoding = "UTF-8")
            lines <- strsplit(content_text, "\n")[[1]]
            data_start <- grep("YEAR", lines)  
            if (length(data_start) == 0) {
              print("Line 'YEAR' not found in header.")
              next
            }
            data_lines <- lines[data_start:length(lines)]
            data <- read_csv(paste(data_lines, collapse = "\n"), show_col_types = FALSE)
            
            
            data[data == -999] <- NA
            
            
            data <- data %>% mutate(LAT = lat, LON = lon)
            
            point_data[[i]] <- data
            success <- TRUE
          } else if (response$status_code == 429) {
            print(paste("Request error: 429 - Too Many Requests. Retrying in 1 minute. Attempt:", attempt))
            Sys.sleep(60)  
            attempt <- attempt + 1
          } else if (response$status_code == 422) {
            print(paste("Request error: 422 - Unprocessable Entity. Latitude:", lat, "Longitude:", lon))
            break
          } else if (response$status_code == 502) {
            print(paste("Request error: 502 - Bad Gateway. Retrying in 1 minute. Attempt:", attempt))
            Sys.sleep(60)  
            attempt <- attempt + 1
          } else {
            
            print(paste("Request error:", response$status_code))
            print(content(response, "text", encoding = "UTF-8"))
            break
          }
        }
        
        if (!success) {
          print(paste("Failed to obtain data after", max_attempts, "attempts. Latitude:", lat, "Longitude:", lon))
        }
      }
      
      if (length(point_data) > 0) {
        combined_point_data <- reduce(point_data, full_join, by = c("YEAR", "DOY", "LAT", "LON"))
        return(combined_point_data)
      } else {
        return(NULL)
      }
    }
    
    
    combined_all_points_data <- process_point(latitudes, longitudes)
    
    if (is.null(combined_all_points_data)) {
      return(NULL)
    }
    
    
    combined_all_points_data <- combined_all_points_data %>% mutate(GD = case_when(
      TbaseSup > T2M_MAX & T2M_MAX > T2M_MIN & T2M_MIN > TbaseInf ~ (T2M_MAX - T2M_MIN) / 2 + T2M_MIN - TbaseInf,
      TbaseSup > T2M_MAX & T2M_MAX > TbaseInf & TbaseInf > T2M_MIN ~ ((T2M_MAX - TbaseInf)^2) / (2 * (T2M_MAX - T2M_MIN)),
      TbaseSup > TbaseInf & TbaseInf > T2M_MAX & T2M_MAX > T2M_MIN ~ 0,
      T2M_MAX > TbaseSup & TbaseSup > T2M_MIN & T2M_MIN > TbaseInf ~ (2 * (T2M_MAX - T2M_MIN) * (T2M_MIN - TbaseInf) + (T2M_MAX - T2M_MIN)^2 - (T2M_MAX - TbaseSup)) / (2 * (T2M_MAX - T2M_MIN)),
      T2M_MAX > TbaseSup & TbaseSup > TbaseInf & TbaseInf > T2M_MIN ~ 0.5 * ((T2M_MAX - TbaseInf)^2 - (T2M_MAX - TbaseSup)^2) / (T2M_MAX - T2M_MIN),
      TRUE ~ 0
    ))
    
    
    combined_all_points_data <- combined_all_points_data %>%
      mutate(
        VPD = (((0.61078 * exp((17.27 * T2M_MIN) / (T2M_MIN + 237.3))) + 
                  (0.61078 * exp((17.27 * T2M_MAX) / (T2M_MAX + 237.3)))) / 2) - 
          (0.61078 * exp((17.27 * T2MDEW) / (T2MDEW + 237.3))),
        psyco = ((((1.013 * 0.001) * PS) / 0.622 * 2.45) * 0.665 * PS * 0.001),
        slope_vapor = 4098 * (0.6108 * exp((17.27 * T2M) / (T2M + 237.3))) / (T2M + 237.2)^2,
        W = slope_vapor / (slope_vapor + psyco),
        ETP = 1.26 * W * (ALLSKY_SFC_SW_DWN - 0) * 0.408,
        PETP = PRECTOTCORR - ETP
      ) %>%
      select(-psyco, -slope_vapor, -W)
    
    
    colnames(combined_all_points_data)[which(names(combined_all_points_data) == "GD")] <- "Degree Days (C)"
    colnames(combined_all_points_data)[which(names(combined_all_points_data) == "VPD")] <- "Vapour Pressure Deficit (kPa)"
    colnames(combined_all_points_data)[which(names(combined_all_points_data) == "ETP")] <- "Potential Evapotranspiration (mm/day)"
    colnames(combined_all_points_data)[which(names(combined_all_points_data) == "PETP")] <- "Precipitation - Potential Evapotranspiration (mm/day)"
    
    
    colnames(combined_all_points_data) <- sapply(colnames(combined_all_points_data), function(x) ifelse(x %in% names(param_names), param_names[[x]], x))
    
    
    combined_all_points_data <- combined_all_points_data %>% select(LAT, LON, everything())
    
    
    if (DoMean) {
      combined_all_points_data <- combined_all_points_data %>%
        select(-YEAR, -DOY) %>%
        group_by(LAT, LON) %>%
        summarize(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
    }
    
    
    combined_all_points_data <- combined_all_points_data %>% mutate(DATE_RANGE = paste(startday, endday, sep = "_"))
    
    
    combined_all_points_data <- combined_all_points_data %>% select(DATE_RANGE, everything())
    
    if (save_csv) {
      
      tryCatch({
        write.csv(combined_all_points_data, file = "EnvFeatures.csv", row.names = FALSE)
      }, error = function(e) {
        print(paste("Error saving the file:", e$message))
      })
    }
    
    return(combined_all_points_data)
  }
}
