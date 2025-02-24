library(openxlsx)
library(dplyr)
library(tidyr)
library(asreml)
library(pls)
library(asreml)
library(openxlsx)
library(pls)
library(dplyr)
library(tidyr)
source("https://raw.githubusercontent.com/saulo-chaves/May_b_useful/main/fa.outs.R")


data <- read.xlsx("C:/Users/gabri/OneDrive/Área de Trabalho/3.xlsx")

data <- transform(data,
                  gen = factor(gen),
                  rep = factor(rep),
                  env = factor(env),
                  yield = as.numeric(yield))

num.env <- nlevels(data$env)
num.gen <- nlevels(data$gen)
name.env <- levels(data$env)
name.gen <- levels(data$gen)

asreml.options(maxit = 100)


mod <- asreml(fixed = yield ~ rep:env + env,
              random = ~ gen:fa(env, 4),
              residual = ~ dsum(~ id(units) | env),
              data = data,
              na.action = na.method(x = "include", y = "include"))
mod <- update(mod)

fa.res <- fa.outs(mod, name.env = "env", name.gen = "gen")
all <- fa.res$blups


EBLUPs_matrix <- matrix(NA, nrow = num.gen, ncol = num.env, dimnames = list(name.gen, name.env))


for (excluded_env in name.env) {
  

  train_data <- subset(data, env != excluded_env)
  

  IACEnvExperimentos_with <- read.xlsx("C:/Users/gabri/OneDrive/Área de Trabalho/IACEnvExperimentos.xlsx", rowNames = TRUE)
  train_covamb <- IACEnvExperimentos_with[rownames(IACEnvExperimentos_with) != excluded_env, ]
  

  mod <- asreml(fixed = yield ~ rep:env + env,
                random = ~ gen:fa(env, 4),
                residual = ~ dsum(~ id(units) | env),
                data = train_data,
                na.action = na.method(x = "include", y = "include"))
  mod <- update(mod)
  
  fa.res <- fa.outs(mod, name.env = "env", name.gen = "gen")
  

  data.pls.lamb <- data.frame(
    lambda = I(fa.res$rot.loads),
    CovAmb = I(scale(as.matrix(train_covamb)))
  )
  
  pls_model <- pls::plsr(lambda ~ CovAmb, validation = "none", data = data.pls.lamb, ncomp = 3)
  coef_pls <- coef(pls_model, intercept = TRUE)[,,1]

  
  IACEnvExperimentos_with <- scale(IACEnvExperimentos_with)
  matcovamb_pred <- IACEnvExperimentos_with[rownames(IACEnvExperimentos_with) == excluded_env, , drop = FALSE]
  

  pred_lambda <- matcovamb_pred %*% coef_pls[-1, ]
  pred_lambda <- cbind(
    fa1 = pred_lambda[,"fa1"] + coef_pls["(Intercept)", "fa1"],
    fa2 = pred_lambda[,"fa2"] + coef_pls["(Intercept)", "fa2"],
    fa3 = pred_lambda[,"fa3"] + coef_pls["(Intercept)", "fa3"],
    fa4 = pred_lambda[,"fa4"] + coef_pls["(Intercept)", "fa4"]
  )
  

  EBLUPs_pred <- as.matrix(pred_lambda[,"fa1"]) %*% fa.res$rot.scores[,"fa1"] +
    as.matrix(pred_lambda[,"fa2"]) %*% fa.res$rot.scores[,"fa2"] +
    as.matrix(pred_lambda[,"fa3"]) %*% fa.res$rot.scores[,"fa3"] +
    as.matrix(pred_lambda[,"fa4"]) %*% fa.res$rot.scores[,"fa4"] +
    mean(data$yield, na.rm = TRUE)
  
  colnames(EBLUPs_pred) <- levels(data$gen)

  
  EBLUPs_matrix[, excluded_env] <- EBLUPs_pred
}


EBLUPs_matrix_df <- as.data.frame(EBLUPs_matrix)


correlations <- list()
ambientes <- unique(all$env)

for (env in ambientes) {
  subset_all <- all %>% filter(env == !!env)
  if (env %in% colnames(EBLUPs_matrix_df)) {
    common_gen <- intersect(subset_all$gen, rownames(EBLUPs_matrix_df))
    if (length(common_gen) > 1) {
      valores_all <- subset_all %>% filter(gen %in% common_gen) %>% arrange(gen) %>% pull(conditional)
      valores_eblup <- EBLUPs_matrix_df[common_gen, env, drop = FALSE] %>% arrange(rownames(.)) %>% pull()
      cor_value <- cor(valores_all, valores_eblup, method = "spearman")
      correlations[[env]] <- cor_value
    }
  }
}

correlation_df <- data.frame(
  Ambiente = names(correlations),
  Spearman_Correlation = unlist(correlations)
)

meanCV4 <- mean(correlation_df$Spearman_Correlation, na.rm = TRUE)






rmse_list <- list()

for (env in ambientes) {
  subset_all <- all %>% filter(env == !!env)
  if (env %in% colnames(EBLUPs_matrix_df)) {
    common_gen <- intersect(subset_all$gen, rownames(EBLUPs_matrix_df))
    if (length(common_gen) > 1) {
      valores_all <- subset_all %>% 
        filter(gen %in% common_gen) %>% 
        arrange(gen) %>% 
        pull(conditional)

      valores_eblup <- EBLUPs_matrix_df[common_gen, env, drop = FALSE] %>% 
        arrange(rownames(.)) %>% 
        pull()
      
      rmse <- sqrt(mean((valores_all - valores_eblup)^2, na.rm = TRUE))
      rmse_list[[env]] <- rmse
    }
  }
}

rmse_df <- data.frame(
  Ambiente = names(rmse_list),
  RMSEP = unlist(rmse_list)
)

meanRMSEP <- mean(rmse_df$RMSEP, na.rm = TRUE)