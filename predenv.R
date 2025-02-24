
library(openxlsx)
library(dplyr)
library(tidyr)
library(asreml)
library(pls)
library(broom)
library(ggplot2)


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

mod3 = asreml(fixed = yield ~ rep:env,
              random = ~ gen:fa(env, 3),
              residual = ~dsum(~id(units)|env),
              data = data,
              na.action = na.method(x="include", y = "include"))
mod3 = update(mod3)

# FA4
sum.m3 = summary(mod3)$varcomp
initg3<-sum.m3[grep('fa',rownames(sum.m3)),'component']
initgfa4<-c(initg3, rep(0.2 ,num.env))

mod1 <- asreml(fixed = yield ~ rep:env,
               random = ~ gen:fa(env, 4, init= initgfa4),
               data = data,
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x = "include", y = "include"))
mod1 <- update(mod1)


mod2 <- asreml(fixed = yield ~ rep:env,
               random = ~ gen,
               data = data,
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x = "include", y = "include"))
mod2 <- update(mod2)


p1 <- predict(mod1, classify = "gen:env")
blup_mod1 <- p1$pvals

p2 <- predict(mod2, classify = "gen")
blup_mod2 <- p2$pvals


blup_mod1 <- blup_mod1 %>%
  rename(blup_genEnv = predicted.value) %>%
  select(gen, env, blup_genEnv)

blup_mod2 <- blup_mod2 %>%
  rename(blup_gen = predicted.value) %>%
  select(gen, blup_gen)


df <- inner_join(blup_mod1, blup_mod2, by = "gen")


resultados <- df %>%
  group_by(env) %>%
  do({

    fit <- lm(blup_genEnv ~ blup_gen, data = .)

    tidy_fit <- broom::tidy(fit)
    
 
    slope_est <- tidy_fit$estimate[2]
    se_slope  <- tidy_fit$std.error[2]
    
  
    pval_slope0 <- tidy_fit$p.value[2]
    

    t_stat  <- (slope_est - 1) / se_slope
   
    df_res  <- fit$df.residual
   
    pval_slope1 <- 2 * (1 - pt(abs(t_stat), df = df_res))
    
    
    r2_val <- broom::glance(fit)$r.squared
    
    data.frame(
      env         = unique(.$env),
      intercept   = tidy_fit$estimate[1],
      slope       = slope_est,
      pval_slope0 = pval_slope0,
      pval_slope1 = pval_slope1,
      r2          = r2_val
    )
  }) %>%
  ungroup()


resultados <- resultados %>%
  mutate(EnvT = case_when(
    pval_slope0 > 0.05             ~ "EnvT = 0",   
    pval_slope1 > 0.05             ~ "EnvT = 1",   
    slope > 1                    ~ "EnvT > 1",   
    slope < 1 & slope > 0        ~ "EnvT < 1"    
  ))




ggplot(df, aes(x = blup_gen, y = blup_genEnv, color = env)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  theme_minimal() +
  labs(
    x = "BLUP",
    y = "BLUP GxE",
    color = "",
    title = ""
  )



#############################################################
data <- data %>%
  left_join(resultados %>% select(env, EnvT), by = "env")

data <- transform(data,
                  gen = factor(gen),
                  rep = factor(rep),
                  env = factor(env),
                  EnvT = factor(EnvT),
                  yield = as.numeric(yield))

data <- data[order(data$EnvT), ]


mody <- asreml(
  fixed = yield ~ rep:env,
  random = ~ gen + gen:env,
  data = data,
  residual = ~ dsum(~ id(units) | env),
  na.action = na.method(x = "include", y = "include")
)

mody <- update(mody)

var_compy <- summary(mody)$varcomp

ratioy <- (0.38)

modx <- asreml(
  fixed = yield ~ rep:env,
  random = ~ gen:diag(EnvT) + gen:env:diag(EnvT),
  data = data,
  residual = ~ dsum(~ id(units) | EnvT),
  na.action = na.method(x = "include", y = "include")
)

modx <- update(modx)



var_comp <- summary(modx)$varcomp


var_table <- data.frame(
  Effect = rownames(var_comp),
  Variance = var_comp[, "component"]
)


var_table <- var_table[grep("gen:env|gen", var_table$Effect), ]

var_table$EnvT <- gsub("gen:EnvT!EnvT_", "", var_table$Effect)

var_gen_env <- var_table[grep("gen:env:EnvT", var_table$Effect, ignore.case = TRUE), ]
var_gen <- var_table[grep("gen:EnvT", var_table$Effect, ignore.case = TRUE), ]


Ratio <- var_gen$Variance / var_gen_env$Variance




Ratio <- c(0.4529, 0.0531, 0.2097, 1.0457)

class_names <- c("EnvT < 1", "EnvT = 0", "EnvT = 1", "EnvT > 1")

ratio_table <- data.frame(
  Class = factor(class_names, levels = class_names),  # Definir como fator com ordem correta
  Ratio = Ratio
)

p <- ggplot(ratio_table, aes(x = Ratio, y = Class)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +  # Barras azuis com transparência
  geom_vline(xintercept = 0.38, linetype = "dashed", color = "red", size = 1) +  # Linha no x=0.38
  labs(title = "",
       x = "Ratio (gen / gen:env)",
       y = "") +
  theme_minimal() 

print(p)





###########################################
env <- read.xlsx("IACEnvExperimentos.xlsx")






library(xgboost)
library(dplyr)
library(caret)
library(Matrix)
library(smotefamily)


colnames(resultados)[colnames(resultados) == "env"] <- "X1"
df <- inner_join(resultados, env, by = "X1")


X <- df %>% select(-X1, -EnvT, -slope, -pval_slope0, -pval_slope1, -r2, -intercept)  
y <- df$EnvT  


y <- as.factor(y)
y_num <- as.numeric(y) - 1 


tabela_classes <- table(y_num)
max_obs <- max(tabela_classes)  


dados_balanceados <- list()


set.seed(123)
for (classe in names(tabela_classes)) {
  X_classe <- X[y_num == as.numeric(classe), , drop = FALSE]  
  y_classe <- rep(as.numeric(classe), nrow(X_classe))  
  

  dup_size <- round((max_obs / nrow(X_classe)) - 1)
  
 
  K_value <- min(3, nrow(X_classe) - 1)  
  
  if (dup_size > 0) {
    smote_result <- SMOTE(X_classe, y_classe, K = K_value, dup_size = dup_size)
    
   
    if (is.list(smote_result) && "data" %in% names(smote_result)) {
      smote_data <- smote_result$data
    } else {
      smote_data <- smote_result  
    }
    
    dados_balanceados[[classe]] <- smote_data
  } else {
    
    dados_balanceados[[classe]] <- data.frame(X_classe, class = y_classe)
  }
}

dados_balanceados <- do.call(rbind, dados_balanceados)


dados_balanceados <- as.data.frame(dados_balanceados)


X_bal <- dados_balanceados[, -ncol(dados_balanceados)]  
y_bal <- as.numeric(dados_balanceados[, ncol(dados_balanceados)])  



y_pred_loocv <- numeric(length(y_bal))


for (i in 1:length(y_bal)) {
  
  X_train <- X_bal[-i, ]
  X_test <- X_bal[i, , drop = FALSE]
  y_train <- y_bal[-i]
  y_test <- y_bal[i]
  

  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test)

  dtrain <- xgb.DMatrix(data = X_train_matrix, label = y_train)
  dtest <- xgb.DMatrix(data = X_test_matrix, label = y_test)
  

  params <- list(
    objective = "multi:softmax",  
    num_class = length(unique(y_bal)),  
    eval_metric = "mlogloss",  
    max_depth = 3,  
    eta = 0.3,  
    subsample = 0.9,  
    colsample_bytree = 0.9  
  )
  

  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50,  
    verbose = 0 
  )
  

  y_pred_loocv[i] <- predict(xgb_model, dtest)
}


conf_matrix_loocv <- confusionMatrix(as.factor(y_pred_loocv), as.factor(y_bal))
print(conf_matrix_loocv)







envpred <- read.xlsx("env_mergednovo1.xlsx")




library(xgboost)
library(dplyr)
library(ggplot2)
library(sf)
library(RColorBrewer)  

variaveis_treino <- colnames(X)  
envpred_filtered <- envpred %>% select(all_of(variaveis_treino))


envpred_matrix <- as.matrix(envpred_filtered)


dpred <- xgb.DMatrix(data = envpred_matrix)


envpred$EnvT_Pred <- predict(xgb_model, dpred)

envpred$EnvT_Pred <- as.factor(envpred$EnvT_Pred)
levels(envpred$EnvT_Pred) <- c(" Highly limiting", "Mildly limiting", "Favorable", "Highly favorable")  


set.seed(123)  
envpred_sampled <- envpred %>% sample_frac(0.3)  


cores_personalizadas <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")  


ggplot(envpred_sampled, aes(x = LON, y = LAT, color = EnvT_Pred)) +
  geom_point(alpha = 0.7, size = 0.1) +  
  theme_minimal() +
  scale_color_manual(
    values = cores_personalizadas, 
    guide = guide_legend(override.aes = list(size = 4))  
  ) +
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude",
    color = ""
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 18, face = "bold"),  
    legend.key.size = unit(2, "cm")  
  )






















library(dplyr)


env_medias <- blup_mod1 %>%
  group_by(env) %>%
  summarise(mean_env = mean(blup_genEnv, na.rm = TRUE))


print(env_medias)


resultados2 <- left_join(resultados, env_medias, by = c("env" = "env"))


head(resultados2)


modelo_reg <- lm(slope ~ mean_env, data = resultados2)
summary(modelo_reg)


cor_pearson <- cor.test(resultados2$slope, resultados2$mean_env, method = "pearson")
print(cor_pearson)








