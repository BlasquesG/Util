

data_clean <- data


for (env_code in paste0("E", sprintf("%02d", 1:23))) {
  

  data_subconjunto <- data_clean[data_clean$env == env_code, ]
  

  if (nrow(data_subconjunto) > 0) {
    

    modex <- asreml(yield ~ rep,
                    random = ~ gen,
                    data = data_subconjunto)
    

    residuos <- residuals(modex)
    

    limite_superior <- mean(residuos) + 3.0 * sd(residuos)
    limite_inferior <- mean(residuos) - 3.0 * sd(residuos)
    

    indices_discrepantes <- which(residuos > limite_superior | residuos < limite_inferior)
    

    data_clean$yield[data_clean$env == env_code & rownames(data_subconjunto) %in% rownames(data_subconjunto[indices_discrepantes, ])] <- NA
    

    print(paste("Ambiente:", env_code))
    print(paste("Número de outliers identificados:", length(indices_discrepantes)))
  }
}


print("Dados com outliers substituídos por NA:")
print(data_clean)




write.xlsx(data_clean, file = "data_cleaned3sd.xlsx")
