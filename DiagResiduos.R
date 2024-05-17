

data_clean <- na.omit(data)
data_subconjunto <- data_clean [data_clean $env == "Amb13", ]
modex <- asreml(yield ~ rep,
                random = ~ gen,
                data = data_subconjunto,
                subset = env == "Amb13")

residuos <- residuals(modex)
limite <- mean(residuos) + 3 * sd(residuos)
indices_discrepantes <- which(abs(residuos) > limite)

observacoes_discrepantes <- data_subconjunto[indices_discrepantes, ]
observacoes_discrepantes$residuo <- residuos[indices_discrepantes]
observacoes_discrepantes_ordenadas <- observacoes_discrepantes[order(observacoes_discrepantes$gen), ]

plot(modex)
print(observacoes_discrepantes_ordenadas)
