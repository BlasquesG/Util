library(meteo)
library(sp)
library(sf)
library(sftime)
library(terra)
library(gstat)
library(plyr)
library(xts)
library(snowfall)
library(doParallel)
library(CAST)
library(ranger)



new_data <- new
data <- na.omit(sera)
colnames(data)


fm.RFSI <- as.formula("Earth.Skin.Temperature..C. ~ bdod_5.15cm + bdod_15.30cm + bdod_30.60cm + cfvo_5.15cm + cfvo_15.30cm + cfvo_30.60cm + clay_5.15cm + clay_15.30cm + clay_30.60cm + nitrogen_5.15cm + nitrogen_15.30cm + nitrogen_30.60cm + ocd_5.15cm + ocd_15.30cm + ocd_30.60cm + ocs_0.30cm + cec_5.15cm + cec_15.30cm + cec_30.60cm + phh2o_5.15cm + phh2o_15.30cm + phh2o_30.60cm + sand_5.15cm + sand_15.30cm + sand_30.60cm + silt_5.15cm + silt_15.30cm + silt_30.60cm + soc_5.15cm + soc_15.30cm + soc_30.60cm + DSR + EVI + NDVI + Mean_PAR_SaoPaulo_5 + Mean_PAR_SaoPaulo_6 + Mean_PAR_SaoPaulo_7 + Mean_Reflectance_SaoPaulo_1 + Mean_Reflectance_SaoPaulo_2 + Mean_Reflectance_SaoPaulo_3 + Mean_Reflectance_SaoPaulo_4 + Mean_Reflectance_SaoPaulo_5 + Mean_Reflectance_SaoPaulo_6 + Mean_Reflectance_SaoPaulo_7 + lat + lon
")

data = st_as_sf(data, coords = c("LAT", "LON"), crs = 4326, agr = "constant")
data = terra::vect(data)
data$id = 1:nrow(data)

rfsi_model <- rfsi(formula = fm.RFSI,
                   data = data,
                   zero.tol = 0,
                   n.obs = 77, 
                   s.crs = st_crs(data), 
                   p.crs = st_crs(data), 
                   cpus = detectCores()-1,
                   progress = TRUE,
                   importance = "impurity",
                   seed = 42,
                   num.trees = 2000,
                   mtry = 5,
                   splitrule = "variance",
                   min.node.size = 5,
                   sample.fraction = 0.95) 

rfsi_model



#################### pred.rfsi ####################
newdata <- as.data.frame(new_data)
newdata$id <- 1:nrow(new_data)
newdata <- terra::rast(new_data)

rfsi_prediction <- pred.rfsi(model = rfsi_model,
                             data = data,
                             obs.col = "Earth.Skin.Temperature..C.",
                             newdata = newdata, 
                             output.format = "SpatRaster", 
                             zero.tol = 0,
                             s.crs = st_crs(data), 
                             newdata.s.crs = st_crs(data), 
                             p.crs = st_crs(data), 
                             cpus = 1,
                             progress = TRUE)


summary(rfsi_prediction)
plot(rfsi_prediction)


pred_df <- as.data.frame(rfsi_prediction, xy = TRUE)
#########ALL

variables <- c("Earth.Skin.Temperature..C.",
               "Temperature.at.2.Meters..C.",
               "Dew.Frost.Point.at.2.Meters..C.",
               "Wet.Bulb.Temperature.at.2.Meters..C.",
               "Earth.Skin.Temperature.Maximum..C.",
               "Temperature.at.2.Meters.Maximum..C.",
               "Temperature.at.2.Meters.Minimum..C.",
               "Cloud.Amount....",
               "Temperature.at.2.Meters.Range..C.",
               "All.Sky.Surface.UVA.Irradiance..W.m.2.",
               "All.Sky.Surface.UVB.Irradiance..W.m.2.",
               "All.Sky.Surface.Albedo..dimensionless.",
               "All.Sky.Surface.Shortwave.Downward.Direct.Normal.Irradiance..MJ.m.2.day.",
               "All.Sky.Surface.Shortwave.Downward.Irradiance..MJ.m.2.day.",
               "Clear.Sky.Surface.Shortwave.Downward.Irradiance..MJ.m.2.day.",
               "All.Sky.Surface.PAR.Total..W.m.2.",
               "All.Sky.Surface.Shortwave.Diffuse.Irradiance..MJ.m.2.day.",
               "Clear.Sky.Surface.PAR.Total..W.m.2.",
               "All.Sky.Surface.UV.Index..dimensionless.",
               "Surface.Pressure..kPa.",
               "Specific.Humidity.at.2.Meters..g.kg.",
               "Relative.Humidity.at.2.Meters....",
               "Wind.Speed.at.2.Meters..m.s.",
               "Earth.Skin.Temperature.Minimum..C.",
               "Surface.Soil.Wetness..1.",
               "Profile.Soil.Moisture..1.",
               "Root.Zone.Soil.Wetness..1.",
               "Wind.Speed.at.2.Meters.Maximum..m.s.",
               "Wind.Speed.at.2.Meters.Minimum..m.s.",
               "Wind.Speed.at.2.Meters.Range..m.s.",
               "Precipitation.Corrected..mm.day.",
               "All.Sky.Surface.Longwave.Downward.Irradiance..W.m.2.",
               "Degree.Days..C.",
               "Vapour.Pressure.Deficit..kPa.",
               "Potential.Evapotranspiration..mm.day.")


output_dir <- "rfsi_predictions"
dir.create(output_dir, showWarnings = FALSE)


all_predictions <- data.frame()


for (var in variables) {
  fm.RFSI <- as.formula(paste(var, "~ bdod_5.15cm + bdod_15.30cm + bdod_30.60cm + cfvo_5.15cm + cfvo_15.30cm + cfvo_30.60cm + clay_5.15cm + clay_15.30cm + clay_30.60cm + nitrogen_5.15cm + nitrogen_15.30cm + nitrogen_30.60cm + ocd_5.15cm + ocd_15.30cm + ocd_30.60cm + ocs_0.30cm + cec_5.15cm + cec_15.30cm + cec_30.60cm + phh2o_5.15cm + phh2o_15.30cm + phh2o_30.60cm + sand_5.15cm + sand_15.30cm + sand_30.60cm + silt_5.15cm + silt_15.30cm + silt_30.60cm + soc_5.15cm + soc_15.30cm + soc_30.60cm + DSR + EVI + NDVI + Mean_PAR_SaoPaulo_5 + Mean_PAR_SaoPaulo_6 + Mean_PAR_SaoPaulo_7 + Mean_Reflectance_SaoPaulo_1 + Mean_Reflectance_SaoPaulo_2 + Mean_Reflectance_SaoPaulo_3 + Mean_Reflectance_SaoPaulo_4 + Mean_Reflectance_SaoPaulo_5 + Mean_Reflectance_SaoPaulo_6 + Mean_Reflectance_SaoPaulo_7 + lat + lon
"))
  
 
  rfsi_model <- rfsi(formula = fm.RFSI,
                     data = data,
                     zero.tol = 0,
                     n.obs = 77, 
                     s.crs = st_crs(data), 
                     p.crs = st_crs(data), 
                     cpus = detectCores()-1,
                     progress = TRUE,
                     importance = "impurity",
                     seed = 42,
                     num.trees = 2000,
                     mtry = 5,
                     splitrule = "variance",
                     min.node.size = 5,
                     sample.fraction = 0.95) 
  
  
  rfsi_prediction <- pred.rfsi(model = rfsi_model,
                               data = data,
                               obs.col = var,
                               newdata = newdata, 
                               output.format = "SpatRaster", 
                               zero.tol = 0,
                               s.crs = st_crs(data), 
                               newdata.s.crs = st_crs(data), 
                               p.crs = st_crs(data), 
                               cpus = 1,
                               progress = TRUE)
  
  
  pred_df <- as.data.frame(rfsi_prediction, xy = TRUE)
  
  
  colnames(pred_df)[3] <- var
  
 
  if (ncol(all_predictions) == 0) {
    all_predictions <- pred_df
  } else {
    all_predictions <- merge(all_predictions, pred_df, by = c("x", "y"))
  }
}


all_pred_file <- paste0(output_dir, "/all_predictions.csv")
write.csv(all_predictions, file = "teste.csv", row.names = FALSE)


