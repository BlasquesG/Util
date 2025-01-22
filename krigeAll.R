library(gstat)
library(sp)
library(raster)
library(hydroGOF)
library(sf)

# Dados e grade
data <- soil_data_extracted
grid <- Grid_0.01_São.Paulo_Brazil
colnames(data)
coordinates(data) <- ~LON+LAT

# Função para realizar a krigagem para uma característica e salvar o gráfico e predições
krige_and_save <- function(data, characteristic, grid, folder = "kriging_results") {
  # Definir a fórmula para a krigagem
  formula_str <- paste(characteristic, "~LON+LAT")
  formula <- as.formula(formula_str)
  
  # Ajustar variogramas
  g <- gstat(id = characteristic, formula = formula, data = data)
  variog <- gstat::variogram(g)
  
  fit_models <- list(
    exp = try(fit.variogram(variog, vgm(1, "Exp", 1, 0.4)), silent = TRUE),
    shp = try(fit.variogram(variog, vgm(1, "Sph", 1, 0.4)), silent = TRUE),
    gau = try(fit.variogram(variog, vgm(1, "Gau", 1.5, 0.4)), silent = TRUE),
    mat = try(fit.variogram(variog, vgm(1, "Mat", 1, 0.4)), silent = TRUE)
  )
  
  # Filtrar modelos que convergiram
  fit_models <- fit_models[sapply(fit_models, function(x) !inherits(x, "try-error"))]
  
  if (length(fit_models) == 0) {
    cat("Nenhum modelo convergiu para a característica", characteristic, "\n")
    return(NULL)
  }
  
  # Função para calcular indicadores de desempenho
  calcula_metricas <- function(crossval, dados, characteristic) {
    lm_model <- lm(crossval$var1.pred ~ dados[[characteristic]])
    r2 <- summary(lm_model)$r.squared
    rmse <- rmse(crossval$var1.pred, dados[[characteristic]])
    mae <- mae(crossval$var1.pred, dados[[characteristic]])
    return(list(r2 = r2, rmse = rmse, mae = mae))
  }
  
  # Modelos de krigagem
  resultados <- list()
  
  # Loop para cada modelo
  for (nome in names(fit_models)) {
    # Leave-One-Out Cross-Validation
    crossval <- krige.cv(formula, locations = data, nmax= 15, model = fit_models[[nome]], nfold = 85)
    metrics <- calcula_metricas(crossval, data, characteristic)
    resultados[[nome]] <- metrics
    print(paste('The R-squared of the', nome, 'model for', characteristic, 'is:', round(metrics$r2, 2)))
    print(paste('The RMSE of the', nome, 'model for', characteristic, 'is:', round(metrics$rmse, 2)))
    print(paste('The MAE of the', nome, 'model for', characteristic, 'is:', round(metrics$mae, 2)))
  }
  
  # Normalizar métricas
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  r2_vals <- sapply(resultados, function(x) x$r2)
  rmse_vals <- sapply(resultados, function(x) x$rmse)
  mae_vals <- sapply(resultados, function(x) x$mae)
  
  r2_norm <- normalize(r2_vals)
  rmse_norm <- normalize(rmse_vals)
  mae_norm <- normalize(mae_vals)
  
  # Calcular score combinado (R2 - RMSE - MAE)
  scores <- r2_norm - rmse_norm - mae_norm
  
  # Selecionar o melhor modelo com base no score
  melhor_modelo <- names(resultados)[which.max(scores)]
  cat("O melhor modelo para", characteristic, "é:", melhor_modelo, "\n")
  
  # Kriging usando o melhor modelo
  coordinates(grid) <- ~LON+LAT
  gridded(grid) <- TRUE
  
  # Realizar a krigagem com o melhor modelo
  predicted <- krige(formula = formula, locations = data, nmax=15, newdata = grid, model = fit_models[[melhor_modelo]])
  
  # Salvar o resultado da krigagem
  png(file.path(folder, paste0("kriging_", characteristic, ".png")), width = 800, height = 600)
  plot(predicted, main = paste("Krigagem para", characteristic))
  dev.off()
  
  # Retornar as predições
  return(predicted)
}

# Criar pasta para salvar os resultados
if (!dir.exists("kriging_results")) {
  dir.create("kriging_results")
}

# Características a serem analisadas
characteristics <- setdiff(names(data), c("LON", "LAT"))

# Inicializar data frame para armazenar as predições
all_predictions <- data.frame(LON = grid$LON, LAT = grid$LAT)

# Loop para todas as características
for (characteristic in characteristics) {
  predicted <- krige_and_save(data, characteristic, grid, folder = "kriging_results")
  if (!is.null(predicted)) {
    all_predictions[[characteristic]] <- predicted$var1.pred
  }
}

# Salvar as predições em um arquivo CSV
write.csv(all_predictions, file = "kriging_results/all_predictions.csv", row.names = FALSE)
