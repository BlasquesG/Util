library(gstat)
library(sp)
library(raster)
library(hydroGOF)
library(sf)
library(rgdal)




field_2016 <- read.csv("C:/Users/gabri/Desktop/Cornos.csv", sep = ';')


hist(field_2016$z)
boxplot(field_2016$z)



coordinates(field_2016) <- ~x+y
plot(field_2016)

spplot(field_2016["z"], main = "C%")


g <- gstat(id="z", formula = z~x+y, data = field_2016)
variog <- gstat::variogram(g)
variog
plot(variog, pch=16, cex=1)


fit_exp <- fit.variogram(variog, vgm(1, "Exp", 15, 6000))   
plot(variog, fit_exp, pch=19, cex=1)
fit_exp

fit_shp <- fit.variogram(variog, vgm(1, "Sph", 12, 4500))   #vgm(psill, model, range, nugget)
plot(variog, fit_shp, pch=16, cex=1)
fit_shp


## Cross-validation - 10-fold
# Exponential
crossval_exp <- krige.cv(z~x+y, locations = field_2016, model = fit_exp, nfold=10)
plot(crossval_exp$var1.pred ~ field_2016$z, cex = 1.2, lwd = 2, xlim = c(0,10), ylim = c(0,10))
abline(0, 1, col = "red", lwd = 2)
lm_exp <- lm(crossval_exp$var1.pred ~ field_2016$z)
abline(lm_exp, col = "green", lwd = 2)
r2_exp <- summary(lm_exp)$r.squared
rmse_exp <- hydroGOF::rmse(crossval_exp$var1.pred, field_2016$z)
print(paste('The R-squared of the exponential model is: ', round(r2_exp, 2)))
print(paste('The RMSE of the exponential model is: ', round(rmse_exp, 2)))

# Spherical
crossval_shp <- krige.cv(z~x+y, locations = field_2016, model = fit_shp, nfold = 10)
plot(crossval_shp$var1.pred ~ field_2016$z, cex = 1.2, lwd = 2, xlim = c(2,10), ylim = c(2,10))
abline(0, 1, col = "red", lwd = 2)
lm_shp <- lm(crossval_shp$var1.pred ~ field_2016$z)
abline(lm_shp, col = "green", lwd = 2)
r2_shp <- summary(lm_shp)$r.squared
rmse_shp <- hydroGOF::rmse(crossval_shp$var1.pred, field_2016$z)
print(paste('The R-squared of the Spherical model is: ', round(r2_shp, 2)))
print(paste('The RMSE of the Spherical model is: ', round(rmse_shp, 2)))



# Kriging
# Convert to sf file
points_sf <- st_as_sf(field_2016, coords = c("x", "y"), crs = 4674) %>% 
  cbind(st_coordinates(.))

# Create a grid 
source("https://raw.githubusercontent.com/BlasquesG/Util/refs/heads/main/GridMaker.R")
generate_grid(resolution = 0.05, country = "Brazil", save = FALSE, plot = F)


grid <- Grid_0.05_Brazil
colnames(grid)<- c("x", "y")
grid$x <- as.numeric(grid$x)
grid$y <- as.numeric(grid$y)
gridded(grid) = ~x+y
plot(grid)

# Exponential kriging
predicted_exp <- krige(formula = z~x+y, locations = field_2016, newdata = grid, model = fit_exp)
plot(predicted_exp)
predicted_exp

# Spherical kriging
predicted_shp <- krige(formula = z~x+y, locations = field_2016, newdata = grid, model = fit_shp)
plot(predicted_shp)

# Plot maps
par(mfrow=c(1,2))
predicted_raster_exp <- raster(predicted_exp)
plot(predicted_raster_exp, main = 'Kriging - Exponential')

predicted_raster_shp <- raster(predicted_shp)
plot(predicted_raster_shp, main = 'Kriging - Spherical')

## Export raster
writeRaster(predicted_raster_shp, 'predicted_2016.tif', overwrite = TRUE)
