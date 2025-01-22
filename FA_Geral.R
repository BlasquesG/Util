library(asreml)
library(tidyverse)
library(gghighlight)
library(ggrepel)
library(sf)
library(ggpubr)
library(parallel)
library(doParallel)
library(foreach)
library(corrplot)
library(colorRamp2)
library(readxl)
library(metan)
library(ComplexHeatmap)
library(ggplot2)
library(circlize)
library(viridis)
library(openxlsx)


#Leitura dos dados--------------------------------------------------------------
data = read_xlsx("C:/Users/gabri/Desktop/datatotaIAC.xlsx")
head(data)

#Transformação em fatores
data = transform(data, 
                 gen = factor(gen),
                 rep = factor(rep),
                 env = factor(env),
                 yield = as.numeric(yield))

num.env = nlevels(data$env)
num.gen = nlevels(data$gen)
name.env = levels(data$env)
name.gen = levels(data$gen)

#Teste LRT----------------------------------------------------------------------
significancia = NULL

for (i in levels(data$env)) {
  mod0 = asreml(yield ~ rep,
                random = ~ gen,
                data = data, maxit = 100,
                na.action = na.method(x="include", y = "include"),
                subset = env == i)
  
  mod1 = asreml(yield ~ rep,
                data = data, maxit = 100,
                na.action = na.method(x="include", y = "include"),
                subset = env == i)
  
  significancia[i] = lrt(mod0,mod1)[3]}

#Verificar Desbalanceamento-----------------------------------------------------
data_expanded = expand.grid(env = levels(data$env), gen = levels(data$gen))

data_expanded$presence = with(data_expanded, paste(env, gen) %in% paste(data$env, data$gen))

data_present = subset(data_expanded, presence)

ggplot(data_present, aes(x = gen, y = env)) +
  geom_point(color = "black") +
  theme_minimal() +
  labs(x = "Genótipo",
       y = "Ambiente") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Ajuste Modelos FA--------------------------------------------------------------
## First-order FA
m1.ph = asreml(fixed = yield ~ env + rep:env ,
               random = ~ gen:fa(env,1),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 100)
m1.ph = update(m1.ph)

sum.m1.ph = summary(m1.ph)$varcomp
acc.m1.ph.mean = summary(m1.ph)$aic
bic.m1.ph = summary(m1.ph)$bic
loglik.m1.ph = summary(m1.ph)$loglik

lambda = sum.m1.ph[grep('fa1',rownames(sum.m1.ph)),'component']
psi = sum.m1.ph[grep('var',rownames(sum.m1.ph)),'component']

Gvcov = lambda %*% t(lambda) + psi

expvar1.ph = (sum(diag(lambda %*% t(lambda)))/
                sum(diag(Gvcov))) * 100


acc.m1.ph = NULL
her.m1.ph = NULL
for(i in 1:num.env){
  predm1.ph_vcov = predict(m1.ph, classify = "gen:env",
                           level=list(env = i), vcov = T)
  predm1.ph_sed = predict(m1.ph, classify = "gen:env",
                          level=list(env = i), sed = T)
  
  PEV = mean(diag(predm1.ph_vcov$vcov))
  MVdelta = mean((predm1.ph_sed$sed^2)[upper.tri(predm1.ph_sed$sed^2,diag = F)])
  acc.m1.ph[i] = sqrt(1-(PEV/Gvcov[i,i]))
  her.m1.ph[i] = 1-(MVdelta/(2*Gvcov[i,i]))
}

acc.m1.ph.mean = mean(acc.m1.ph)
her.m1.ph.mean = mean(her.m1.ph)

aic.m1.ph = summary(m1.ph)$aic

## Second-order FA
m2.ph = asreml(fixed = yield ~ env +  rep:env ,
               random = ~ gen:fa(env,2),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 100)
m2.ph = update(m2.ph)

sum.m2.ph = summary(m2.ph)$varcomp
aic.m2.ph = summary(m2.ph)$aic
bic.m2.ph = summary(m2.ph)$bic
loglik.m2.ph = summary(m2.ph)$loglik

fa1.loadings = sum.m2.ph[grep('fa1',rownames(sum.m2.ph)),1]
fa2.loadings = sum.m2.ph[grep('fa2',rownames(sum.m2.ph)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
colnames(mat.loadings.star) = c("fa1",'fa2')
psi = diag(sum.m2.ph[grep('var',rownames(sum.m2.ph)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi
expvar2.ph = (sum(diag(lamblamb.star))/
                sum(diag(Gvcov))) * 100

acc.m2.ph = NULL
her.m2.ph = NULL
for(i in 1:num.env){
  predm2.ph_vcov = predict(m2.ph, classify = "gen:env",
                           level=list(env = i), vcov = T)
  predm2.ph_sed = predict(m2.ph, classify = "gen:env",
                          level=list(env = i), sed = T)
  
  PEV = mean(diag(predm2.ph_vcov$vcov))
  MVdelta = mean((predm2.ph_sed$sed^2)[upper.tri(predm2.ph_sed$sed^2,diag = F)])
  acc.m2.ph[i] = sqrt(1-(PEV/Gvcov[i,i]))
  her.m2.ph[i] = 1-(MVdelta/(2*Gvcov[i,i]))
}

acc.m2.ph.mean = mean(acc.m2.ph)
her.m2.ph.mean = mean(her.m2.ph)

## Third-order FA
m3.ph = asreml(fixed = yield ~ env +  rep:env ,
               random = ~ gen:fa(env,3),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 100)
m3.ph = update(m3.ph)

sum.m3.ph = summary(m3.ph)$varcomp
aic.m3.ph = summary(m3.ph)$aic
bic.m3.ph = summary(m3.ph)$bic
loglik.m3.ph = summary(m3.ph)$loglik

fa1.loadings = sum.m3.ph[grep('fa1',rownames(sum.m3.ph)),1]
fa2.loadings = sum.m3.ph[grep('fa2',rownames(sum.m3.ph)),1]
fa3.loadings = sum.m3.ph[grep('fa3',rownames(sum.m3.ph)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
colnames(mat.loadings.star) = c("fa1",'fa2','fa3')
psi = diag(sum.m3.ph[grep('var',rownames(sum.m3.ph)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi
expvar3.ph = (sum(diag(lamblamb.star))/
                sum(diag(Gvcov))) * 100

acc.m3.ph = NULL
her.m3.ph = NULL
for(i in 1:num.env){
  predm3.ph_vcov = predict(m3.ph, classify = "gen:env",
                           level=list(env = i), vcov = T)
  predm3.ph_sed = predict(m3.ph, classify = "gen:env",
                          level=list(env = i), sed = T)
  
  PEV = mean(diag(predm3.ph_vcov$vcov))
  MVdelta = mean((predm3.ph_sed$sed^2)[upper.tri(predm3.ph_sed$sed^2,diag = F)])
  acc.m3.ph[i] = sqrt(1-(PEV/Gvcov[i,i]))
  her.m3.ph[i] = 1-(MVdelta/(2*Gvcov[i,i]))
}

acc.m3.ph.mean = mean(acc.m3.ph)
her.m3.ph.mean = mean(her.m3.ph)

## Fourth-order FA
m4.ph = asreml(fixed = yield ~ env +  rep:env ,
               random = ~ gen:fa(env,4),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 1000)
m4.ph = update(m4.ph)

sum.m4.ph = summary(m4.ph)$varcomp
aic.m4.ph = summary(m4.ph)$aic
bic.m4.ph = summary(m4.ph)$bic
loglik.m4.ph = summary(m4.ph)$loglik

fa1.loadings = sum.m4.ph[grep('fa1',rownames(sum.m4.ph)),1]
fa2.loadings = sum.m4.ph[grep('fa2',rownames(sum.m4.ph)),1]
fa3.loadings = sum.m4.ph[grep('fa3',rownames(sum.m4.ph)),1]
fa4.loadings = sum.m4.ph[grep('fa4',rownames(sum.m4.ph)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings,
                               fa4.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
colnames(mat.loadings.star) = c("fa1",'fa2','fa3','fa4')
psi = diag(sum.m4.ph[grep('var',rownames(sum.m4.ph)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi
expvar4.ph = (sum(diag(lamblamb.star))/
                sum(diag(Gvcov))) * 100

acc.m4.ph = NULL
her.m4.ph = NULL
for(i in 1:num.env){
  predm4.ph_vcov = predict(m4.ph, classify = "gen:env",
                           level=list(env = i), vcov = T)
  predm4.ph_sed = predict(m4.ph, classify = "gen:env",
                          level=list(env = i), sed = T)
  
  PEV = mean(diag(predm4.ph_vcov$vcov))
  MVdelta = mean((predm4.ph_sed$sed^2)[upper.tri(predm4.ph_sed$sed^2,diag = F)])
  acc.m4.ph[i] = sqrt(1-(PEV/Gvcov[i,i]))
  her.m4.ph[i] = 1-(MVdelta/(2*Gvcov[i,i]))
}

acc.m4.ph.mean = mean(acc.m4.ph)
her.m4.ph.mean = mean(her.m4.ph)

## Fifth-order FA
initg<-sum.m4.ph[grep('fa',rownames(sum.m4.ph)),'component']
initgfa5<-c(initg, rep(2.0,num.env))
m5.ph = asreml(fixed = yield ~ env +  rep:env,
               random = ~ gen:fa(env,5),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 1000)
m5.ph = update(m5.ph)

sum.m5.ph = summary(m5.ph)$varcomp
aic.m5.ph = summary(m5.ph)$aic
bic.m5.ph = summary(m5.ph)$bic
loglik.m5.ph = summary(m5.ph)$loglik

fa1.loadings = sum.m5.ph[grep('fa1',rownames(sum.m5.ph)),1]
fa2.loadings = sum.m5.ph[grep('fa2',rownames(sum.m5.ph)),1]
fa3.loadings = sum.m5.ph[grep('fa3',rownames(sum.m5.ph)),1]
fa4.loadings = sum.m5.ph[grep('fa4',rownames(sum.m5.ph)),1]
fa5.loadings = sum.m5.ph[grep('fa5',rownames(sum.m5.ph)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings,
                               fa4.loadings, fa5.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
colnames(mat.loadings.star) = c("fa1",'fa2','fa3','fa4','fa5')
psi = diag(sum.m5.ph[grep('var',rownames(sum.m5.ph)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi
expvar5.ph = (sum(diag(lamblamb.star))/
                sum(diag(Gvcov))) * 100

acc.m5.ph = NULL
her.m5.ph = NULL
for(i in 1:num.env){
  predm5.ph_vcov = predict(m5.ph, classify = "gen:env",
                           level=list(env = i), vcov = T)
  predm5.ph_sed = predict(m5.ph, classify = "gen:env",
                          level=list(env = i), sed = T)
  
  PEV = mean(diag(predm5.ph_vcov$vcov))
  MVdelta = mean((predm5.ph_sed$sed^2)[upper.tri(predm5.ph_sed$sed^2,diag = F)])
  acc.m5.ph[i] = sqrt(1-(PEV/Gvcov[i,i]))
  her.m5.ph[i] = 1-(MVdelta/(2*Gvcov[i,i]))
}

acc.m5.ph.mean = mean(acc.m5.ph)
her.m5.ph.mean = mean(her.m5.ph)

## Sixth-order FA
initg<-sum.m5.ph[grep('fa',rownames(sum.m5.ph)),'component']
initgfa6<-c(initg, rep(2.0,num.env))
m6.ph = asreml(fixed = yield ~ env + rep:env ,
               random = ~ gen:fa(env,6),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 100)
m6.ph = update(m6.ph)

sum.m6.ph = summary(m6.ph)$varcomp
aic.m6.ph = summary(m6.ph)$aic
bic.m6.ph = summary(m6.ph)$bic
loglik.m6.ph = summary(m6.ph)$loglik

fa1.loadings = sum.m6.ph[grep('fa1',rownames(sum.m6.ph)),1]
fa2.loadings = sum.m6.ph[grep('fa2',rownames(sum.m6.ph)),1]
fa3.loadings = sum.m6.ph[grep('fa3',rownames(sum.m6.ph)),1]
fa4.loadings = sum.m6.ph[grep('fa4',rownames(sum.m6.ph)),1]
fa5.loadings = sum.m6.ph[grep('fa5',rownames(sum.m6.ph)),1]
fa6.loadings = sum.m6.ph[grep('fa6',rownames(sum.m6.ph)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings,
                               fa4.loadings, fa5.loadings,fa6.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
colnames(mat.loadings.star) = c("fa1",'fa2','fa3','fa4','fa5','fa6')
psi = diag(sum.m6.ph[grep('var',rownames(sum.m6.ph)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi
expvar6.ph = (sum(diag(lamblamb.star))/
                sum(diag(Gvcov))) * 100

acc.m6.ph = NULL
her.m6.ph = NULL
for(i in 1:num.env){
  predm6.ph_vcov = predict(m6.ph, classify = "gen:env",
                           level=list(env = i), vcov = T)
  predm6.ph_sed = predict(m6.ph, classify = "gen:env",
                          level=list(env = i), sed = T)
  
  PEV = mean(diag(predm6.ph_vcov$vcov))
  MVdelta = mean((predm6.ph_sed$sed^2)[upper.tri(predm6.ph_sed$sed^2,diag = F)])
  acc.m6.ph[i] = sqrt(1-(PEV/Gvcov[i,i]))
  her.m6.ph[i] = 1-(MVdelta/(2*Gvcov[i,i]))}

acc.m6.ph.mean = mean(acc.m6.ph)
her.m6.ph.mean = mean(her.m6.ph)

##Teste Valores Iniciais
initg<-sum.m5.ph[grep('fa',rownames(sum.m5.ph)),'component']

for (init_val in seq(0.1, 5, by = 0.1)) {
  initgfa6 <- c(initg, rep(init_val, num.env))
  
  m6.ph = asreml(fixed = yield ~ env + rep:env,
                 random = ~ gen:fa(env, 6, init= initgfa6),
                 residual = ~dsum(~id(units)|env),
                 na.action = na.method(x="include", y = "include"),
                 data = data, maxit = 1000)
  
  m6.ph = update(m6.ph)
  
  if (m6.ph$converge == TRUE) {
    cat("Convergência alcançada com init_val =", init_val, "\n")
    break
  }
}

##Avaliacao Modelos
source("https://raw.githubusercontent.com/saulo-chaves/May_b_useful/main/fa.outs.R")

result <- data.frame(
  "Model" = c(paste0("FA",1:6)),
  "AIC" = c(aic.m1.ph, aic.m2.ph, aic.m3.ph, aic.m4.ph, aic.m5.ph, aic.m6.ph),
  "BIC" = c(bic.m1.ph, bic.m2.ph, bic.m3.ph, bic.m4.ph, bic.m5.ph, bic.m6.ph),
  "ExpVar" = c(expvar1.ph, expvar2.ph, expvar3.ph, expvar4.ph, expvar5.ph, expvar6.ph),
  "Accuracy" = c(acc.m1.ph.mean, acc.m2.ph.mean, acc.m3.ph.mean, 
                 acc.m4.ph.mean, acc.m5.ph.mean, acc.m6.ph.mean),
  "Heritability" = c(her.m1.ph.mean, her.m2.ph.mean, her.m3.ph.mean, 
                     her.m4.ph.mean, her.m5.ph.mean, her.m6.ph.mean)
)

for (i in 1:6) {
  model_name <- paste0("m", i, ".ph")
  SV <- fa.outs(model = get(model_name), name.env = "env", name.gen = "gen")
  result[i, "ASVR"] <- SV$diagnostics["ASVR"]
  result[i, "logLik"] <- SV$diagnostics["logl"]
}

print(result)

#Modelo Para Predicoes----------------------------------------------------------
## FAST - FA3 ------------------------------------------------------------------
m3.ph = asreml(fixed = yield ~ env + rep:env,
               random = ~ gen:fa(env,3),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 1000)
m3.ph = update(m3.ph)


sum.m3 = summary(m3.ph)$varcomp
aic.m3 = summary(m3.ph)$aic
bic.m3 = summary(m3.ph)$bic
loglik.m3 = summary(m3.ph)$loglik

fa1.loadings = sum.m3[grep('fa1',rownames(sum.m3)),1]
fa2.loadings = sum.m3[grep('fa2',rownames(sum.m3)),1]
fa3.loadings = sum.m3[grep('fa3',rownames(sum.m3)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
rownames(mat.loadings.star) = name.env
colnames(mat.loadings.star) = c("fa1",'fa2','fa3')

write.csv(mat.loadings.star, 'cargas.csv')

psi = diag(sum.m3[grep('var',rownames(sum.m3)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi

## Explained variance per factor
psi=psi[row(psi)==col(psi)]
i <- 1
FA1 <- NULL
FA2 <- NULL
FA3 = NULL

while(i <= num.env){
  FA1[i] = 100*(mat.loadings.star[i,1]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2
                  )+psi[i]))
  FA2[i] = 100*(mat.loadings.star[i,2]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2
                  )+psi[i]))
  FA3[i] = 100*(mat.loadings.star[i,3]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2
                  )+psi[i]))
  
  i = i+1}

var.exp = data.frame(
  "Env" = name.env,
  "var" = c(FA1,FA2,FA3),
  "FA" = c(rep("FA1",num.env), rep("FA2",num.env),
           rep("FA3",num.env)))

a1 = ggplot(data = var.exp, aes(x = factor(Env), y = var, fill = FA)) +
  geom_bar(stat = "identity") +
  xlab("Ambientes") + ylab("% Variância") +
  theme(legend.position = "right", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(fill = "FAk") +
  scale_fill_hue(labels = c(paste0('FA1 (', round((sum(mat.loadings.star[,1] * t(mat.loadings.star[,1]))/sum(diag(Gvcov)))*100, 2), '%)'),
                            paste0('FA2 (', round((sum(mat.loadings.star[,2] * t(mat.loadings.star[,2]))/sum(diag(Gvcov)))*100, 2), '%)'),
                            paste0('FA3 (', round((sum(mat.loadings.star[,3] * t(mat.loadings.star[,3]))/sum(diag(Gvcov)))*100, 2), '%)')))

a1

## Obtaining and rotating the scores
coef3 = coef(m3.ph)$random

fa1.scores = coef3[grep("Comp1",row.names(coef3)),1];
names(fa1.scores) = sub("fa(env, 3)_Comp1:gen_","",names(fa1.scores),fixed=T)
fa2.scores = coef3[grep("Comp2",row.names(coef3)),1];
names(fa2.scores) = sub("fa(env, 3)_Comp2:gen_","",names(fa2.scores),fixed=T)
fa3.scores = coef3[grep("Comp3",row.names(coef3)),1];
names(fa3.scores) = sub("fa(env, 3)_Comp3:gen_","",names(fa3.scores),fixed=T)
fa.scores = rbind(as.matrix(fa1.scores),as.matrix(fa2.scores),as.matrix(fa3.scores))
fa.scores.star = -kronecker(t(svd.mat.loadings$v), diag(num.gen))%*%fa.scores 
rownames(fa.scores.star) = rep(name.gen, 3)

fa1.scores.star = fa.scores.star[1:num.gen,1]
fa2.scores.star = fa.scores.star[(num.gen+1):(num.gen*2),1]
fa3.scores.star = fa.scores.star[(num.gen*2+1):(num.gen*3),1]

scor = data.frame("Gen" = name.gen,
                  "Factor scores 1" = fa1.scores.star,
                  "Factor scores 2" = fa2.scores.star,
                  "Factor scores 3" = fa3.scores.star
); rownames(scor) = NULL

## Marginal EBLUPs
EBLUPs_marg = (kronecker(mat.loadings.star,diag(num.gen))) %*% fa.scores.star 
EBLUPs_marg = data.frame("Environment" = rep(name.env,each = num.gen),
                         "Gen" = rep(name.gen,num.env),
                         "EBLUP_marg" = EBLUPs_marg)
EBLUPs_marg$FL_1 = rep(mat.loadings.star[,1], each = num.gen)

## Reliability
pred_vcov = predict(m3.ph, classify = "gen:env", vcov = T)
PEVij = diag(pred_vcov$vcov)
PEVij = cbind(pred_vcov$pvals[,1:2],PEVij)
PEVi = PEVij %>% group_by(gen) %>% summarise(PEVi = mean(PEVij)) %>% 
  mutate(
    reli = 1-(PEVi/mean(diag(Gvcov))))

## Heritability and CV 
CV = sqrt(sum.m3[grep('!R', rownames(sum.m3)),1])/
  (data %>% group_by(env) %>% summarise(Value = mean(yield, na.rm=T)))[,2]

H2 = NULL
for(i in 1:num.env){
  pred_sed = predict(m3.ph, classify = "gen:env", levels = list(env = i), sed = T)
  MVDB = mean((pred_sed$sed^2)[upper.tri((pred_sed$sed^2),diag=F)])
  H2[i] = 1-(MVDB/(2*Gvcov[i,i]))
}

h2cv = data.frame("Environment" = name.env,
                  "Heritability" = H2,
                  "coefvar" = CV)

h2cv %>% ggplot(aes(x = Heritability, y= Value, color = Environment)) +
  xlim(0,1)+xlab("Herdabilidade")+ylab("CV") + ylim(0.0, 0.5) +
  theme(legend.position = 'none')+ xlim(0.4, 1) +
  geom_text(aes(label = Environment), size = 4) 

data.frame(
  Environment = name.env,
  Value = c(H2, CV$Value),
  param = rep(c("Her","CV"),each = num.env)
) %>% ggplot()+
  geom_density(aes(x = Value, fill = param), alpha = .6)+
  scale_fill_manual(values = c("CV" = 'red1',
                               'Her' = 'darkolivegreen2'),
                    labels = c("CV" = "Coeficiente de Variação",
                               "Her" = "Herdabilidade"))+
  theme(legend.position = "bottom")+
  labs(fill = "", x = "", y="Frequência")

# FAST - FA4 -------------------------------------------------------------------
initg<-sum.m3.ph[grep('fa',rownames(sum.m3.ph)),'component']
initgfa4<-c(initg, rep(2.0 ,num.env))
m4.ph = asreml(fixed =yield ~ env+ rep:env ,
               random = ~ gen:fa(env,4),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 1000)
m4.ph = update(m4.ph)

sum.m4 = summary(m4.ph)$varcomp
aic.m4 = summary(m4.ph)$aic
bic.m4 = summary(m4.ph)$bic
loglik.m4 = summary(m4.ph)$loglik

fa1.loadings = sum.m4[grep('fa1',rownames(sum.m4)),1]
fa2.loadings = sum.m4[grep('fa2',rownames(sum.m4)),1]
fa3.loadings = sum.m4[grep('fa3',rownames(sum.m4)),1]
fa4.loadings = sum.m4[grep('fa4',rownames(sum.m4)),1]

mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings, fa4.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
rownames(mat.loadings.star) = name.env
colnames(mat.loadings.star) = c("fa1",'fa2','fa3','fa4')

write.csv(mat.loadings.star, 'cargas.csv')


psi = diag(sum.m4[grep('var',rownames(sum.m4)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi

## Explained variance per factor
psi=psi[row(psi)==col(psi)]
i <- 1
FA1 <- NULL
FA2 <- NULL
FA3 <- NULL
FA4 = NULL

while(i <= num.env){
  FA1[i] = 100*(mat.loadings.star[i,1]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2)+psi[i]))
  FA2[i] = 100*(mat.loadings.star[i,2]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2)+psi[i]))
  FA3[i] = 100*(mat.loadings.star[i,3]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2)+psi[i]))
  FA4[i] = 100*(mat.loadings.star[i,4]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2)+psi[i]))
  
  i = i+1}

var.exp = data.frame(
  "Env" = name.env,
  "var" = c(FA1,FA2,FA3,FA4),
  "FA" = c(rep("FA1",num.env), rep("FA2",num.env),
           rep("FA3",num.env),rep("FA4",num.env)))

a1 = ggplot(data = var.exp, aes(x = factor(Env), y = var, fill = FA)) +
  geom_bar(stat = "identity") +
  xlab("Ambientes") + ylab("% Variância") +
  theme(legend.position = "right", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(fill = "FAk") +
  scale_fill_hue(labels = c(paste0('FA1 (', round((sum(mat.loadings.star[,1] * t(mat.loadings.star[,1]))/sum(diag(Gvcov)))*100, 2), '%)'),
                            paste0('FA2 (', round((sum(mat.loadings.star[,2] * t(mat.loadings.star[,2]))/sum(diag(Gvcov)))*100, 2), '%)'),
                            paste0('FA3 (', round((sum(mat.loadings.star[,3] * t(mat.loadings.star[,3]))/sum(diag(Gvcov)))*100, 2), '%)'),
                            paste0('FA4 (', round((sum(mat.loadings.star[,4] * t(mat.loadings.star[,4]))/sum(diag(Gvcov)))*100, 2), '%)')))
a1

## Obtaining and rotating the scores
coef4 = coef(m4.ph)$random

fa1.scores = coef4[grep("Comp1",row.names(coef4)),1];
names(fa1.scores) = sub("fa(env, 4)_Comp1:gen_","",names(fa1.scores),fixed=T)
fa2.scores = coef4[grep("Comp2",row.names(coef4)),1];
names(fa2.scores) = sub("fa(env, 4)_Comp2:gen_","",names(fa2.scores),fixed=T)
fa3.scores = coef4[grep("Comp3",row.names(coef4)),1];
names(fa3.scores) = sub("fa(env, 4)_Comp3:gen_","",names(fa3.scores),fixed=T)
fa4.scores = coef4[grep("Comp4",row.names(coef4)),1];
names(fa4.scores) = sub("fa(env, 4)_Comp4:gen_","",names(fa4.scores),fixed=T)
fa.scores = rbind(as.matrix(fa1.scores),as.matrix(fa2.scores),as.matrix(fa3.scores),as.matrix(fa4.scores))
fa.scores.star = -kronecker(t(svd.mat.loadings$v), diag(num.gen))%*%fa.scores 
rownames(fa.scores.star) = rep(name.gen, 4)

fa1.scores.star = fa.scores.star[1:num.gen,1]
fa2.scores.star = fa.scores.star[(num.gen+1):(num.gen*2),1]
fa3.scores.star = fa.scores.star[(num.gen*2+1):(num.gen*3),1]
fa4.scores.star = fa.scores.star[(num.gen*3+1):(num.gen*4),1]


scor = data.frame("Gen" = name.gen,
                  "Factor scores 1" = fa1.scores.star,
                  "Factor scores 2" = fa2.scores.star,
                  "Factor scores 3" = fa3.scores.star,
                  "Factor scores 4" = fa4.scores.star
); rownames(scor) = NULL

## Marginal EBLUPs
EBLUPs_marg = (kronecker(mat.loadings.star,diag(num.gen))) %*% fa.scores.star 
EBLUPs_marg = data.frame("Environment" = rep(name.env,each = num.gen),
                         "Gen" = rep(name.gen,num.env),
                         "EBLUP_marg" = EBLUPs_marg)
EBLUPs_marg$FL_1 = rep(mat.loadings.star[,1], each = num.gen)

## Reliability
pred_vcov = predict(m4.ph, classify = "gen:env", vcov = T)
PEVij = diag(pred_vcov$vcov)
PEVij = cbind(pred_vcov$pvals[,1:2],PEVij)
PEVi = PEVij %>% group_by(gen) %>% summarise(PEVi = mean(PEVij)) %>% 
  mutate(
    reli = 1-(PEVi/mean(diag(Gvcov))))

## Heritability and CV 
CV = sqrt(sum.m4[grep('!R', rownames(sum.m4)),1])/
  (data %>% group_by(env) %>% summarise(Value = mean(yield, na.rm=T)))[,2]

H2 = NULL
for(i in 1:num.env){
  pred_sed = predict(m4.ph, classify = "gen:env", levels = list(env = i), sed = T)
  MVDB = mean((pred_sed$sed^2)[upper.tri((pred_sed$sed^2),diag=F)])
  H2[i] = 1-(MVDB/(2*Gvcov[i,i]))
}

h2cv = data.frame("Environment" = name.env,
                  "Heritability" = H2,
                  "coefvar" = CV)

h2cv %>% ggplot(aes(x = Heritability, y= Value, color = Environment)) +
  xlim(0,1)+xlab("Herdabilidade")+ylab("CV") + ylim(0.0, 0.5) +
  theme(legend.position = 'none')+ xlim(0.4, 1.0) +
  geom_text(aes(label = Environment), size = 4) 



# FAST - FA5 -------------------------------------------------------------------
m5.ph = asreml(fixed = yield ~ env + rep,
               random = ~ gen:fa(env,5),
               residual = ~dsum(~id(units)|env),
               na.action = na.method(x="include", y = "include"),
               data = data, maxit = 100)
m5.ph = update(m5.ph)

sum.m5 = summary(m5.ph)$varcomp
aic.m5 = summary(m5.ph)$aic
bic.m5 = summary(m5.ph)$bic
loglik.m5 = summary(m5.ph)$loglik

fa1.loadings = sum.m5[grep('fa1',rownames(sum.m5)),1]
fa2.loadings = sum.m5[grep('fa2',rownames(sum.m5)),1]
fa3.loadings = sum.m5[grep('fa3',rownames(sum.m5)),1]
fa4.loadings = sum.m5[grep('fa4',rownames(sum.m5)),1]
fa5.loadings = sum.m5[grep('fa5',rownames(sum.m5)),1]
mat.loadings = as.matrix(cbind(fa1.loadings, fa2.loadings,fa3.loadings, fa4.loadings, fa5.loadings))
svd.mat.loadings = svd(mat.loadings)
mat.loadings.star = mat.loadings %*% svd.mat.loadings$v * -1
rownames(mat.loadings.star) = name.env
colnames(mat.loadings.star) = c("fa1",'fa2','fa3','fa4','fa5')

write.csv(mat.loadings.star, 'cargas_rot.csv')

psi = diag(sum.m5[grep('var',rownames(sum.m5)),1])
lamblamb.star = mat.loadings.star %*% t(mat.loadings.star)
Gvcov = lamblamb.star + psi

## Explained variance per factor
psi=psi[row(psi)==col(psi)]
i <- 1
FA1 <- NULL
FA2 <- NULL
FA3 <- NULL
FA4 <- NULL
FA5 = NULL
while(i <= num.env){
  FA1[i] = 100*(mat.loadings.star[i,1]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2,
                       mat.loadings.star[i,5]^2)+psi[i]))
  FA2[i] = 100*(mat.loadings.star[i,2]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2,
                       mat.loadings.star[i,5]^2)+psi[i]))
  FA3[i] = 100*(mat.loadings.star[i,3]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2,
                       mat.loadings.star[i,5]^2)+psi[i]))
  FA4[i] = 100*(mat.loadings.star[i,4]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2,
                       mat.loadings.star[i,5]^2)+psi[i]))
  FA5[i] = 100*(mat.loadings.star[i,5]^2/
                  (sum(mat.loadings.star[i,1]^2,
                       mat.loadings.star[i,2]^2,
                       mat.loadings.star[i,3]^2,
                       mat.loadings.star[i,4]^2,
                       mat.loadings.star[i,5]^2)+psi[i]))
  i = i+1}

var.exp = data.frame(
  "Env" = name.env,
  "var" = c(FA1,FA2,FA3,FA4,FA5),
  "FA" = c(rep("FA1",num.env), rep("FA2",num.env),
           rep("FA3",num.env),rep("FA4",num.env),
           rep("FA5",num.env)))

a1 = ggplot(data = var.exp, aes(x = Env, y = var, fill = FA)) +
  geom_bar(stat = "identity") +
  xlab("Ambientes") + ylab("% Variância") +
  theme(legend.position = "right",
        legend.title = element_blank(),  # Remove o título da legenda
        legend.text = element_text(size = 8),  # Tamanho da fonte da legenda
        axis.title.x = element_text(size = 10), # Tamanho da fonte do título do eixo X
        axis.title.y = element_text(size = 10), # Tamanho da fonte do título do eixo Y
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_hue(labels = c(paste0('FA1 (', round(mean(subset(var.exp, FA %in% "FA1")$var), 2), '%)'),
                            paste0('FA2 (', round(mean(subset(var.exp, FA %in% "FA2")$var), 2), '%)'),
                            paste0('FA3 (', round(mean(subset(var.exp, FA %in% "FA3")$var), 2), '%)'),
                            paste0('FA4 (', round(mean(subset(var.exp, FA %in% "FA4")$var), 2), '%)'),
                            paste0('FA5 (', round(mean(subset(var.exp, FA %in% "FA5")$var), 2), '%)')
  ))
a1



## Obtaining and rotating the scores
coef5 = coef(m5.ph)$random

fa1.scores = coef5[grep("Comp1",row.names(coef5)),1];
names(fa1.scores) = sub("fa(env, 5)_Comp1:gen_","",names(fa1.scores),fixed=T)
fa2.scores = coef5[grep("Comp2",row.names(coef5)),1];
names(fa2.scores) = sub("fa(env, 5)_Comp2:gen_","",names(fa2.scores),fixed=T)
fa3.scores = coef5[grep("Comp3",row.names(coef5)),1];
names(fa3.scores) = sub("fa(env, 5)_Comp3:gen_","",names(fa3.scores),fixed=T)
fa4.scores = coef5[grep("Comp4",row.names(coef5)),1];
names(fa4.scores) = sub("fa(env, 5)_Comp4:gen_","",names(fa4.scores),fixed=T)
fa5.scores = coef5[grep("Comp4",row.names(coef5)),1];
names(fa4.scores) = sub("fa(env, 5)_Comp4:gen_","",names(fa5.scores),fixed=T)
fa.scores = rbind(as.matrix(fa1.scores),as.matrix(fa2.scores),as.matrix(fa3.scores),as.matrix(fa4.scores),as.matrix(fa5.scores))
fa.scores.star = -kronecker(t(svd.mat.loadings$v), diag(num.gen))%*%fa.scores 
rownames(fa.scores.star) = rep(name.gen, 5)

fa1.scores.star = fa.scores.star[1:num.gen,1]
fa2.scores.star = fa.scores.star[(num.gen+1):(num.gen*2),1]
fa3.scores.star = fa.scores.star[(num.gen*2+1):(num.gen*3),1]
fa4.scores.star = fa.scores.star[(num.gen*3+1):(num.gen*4),1]
fa5.scores.star = fa.scores.star[(num.gen*4+1):(num.gen*5),1]

scor = data.frame("Gen" = name.gen,
                  "Factor scores 1" = fa1.scores.star,
                  "Factor scores 2" = fa2.scores.star,
                  "Factor scores 3" = fa3.scores.star,
                  "Factor scores 4" = fa4.scores.star,
                  "Factor scores 5" = fa5.scores.star
); rownames(scor) = NULL

## Marginal EBLUPs
EBLUPs_marg = (kronecker(mat.loadings.star,diag(num.gen))) %*% fa.scores.star 
EBLUPs_marg = data.frame("Environment" = rep(name.env,each = num.gen),
                         "Gen" = rep(name.gen,num.env),
                         "EBLUP_marg" = EBLUPs_marg)
EBLUPs_marg$FL_1 = rep(mat.loadings.star[,1], each = num.gen)

## Reliability
pred_vcov = predict(m5.ph, classify = "gen:env", vcov = T)
PEVij = diag(pred_vcov$vcov)
PEVij = cbind(pred_vcov$pvals[,1:2],PEVij)
PEVi = PEVij %>% group_by(gen) %>% summarise(PEVi = mean(PEVij)) %>% 
  mutate(
    reli = 1-(PEVi/mean(diag(Gvcov))))

## Heritability and CV 
CV = sqrt(sum.m5[grep('!R', rownames(sum.m5)),1])/
  (data %>% group_by(env) %>% summarise(Value = mean(yield, na.rm=T)))[,2]

H2 = NULL
for(i in 1:num.env){
  pred_sed = predict(m5.ph, classify = "gen:env", levels = list(env = i), sed = T)
  MVDB = mean((pred_sed$sed^2)[upper.tri((pred_sed$sed^2),diag=F)])
  H2[i] = 1-(MVDB/(2*Gvcov[i,i]))
}

h2cv = data.frame("Environment" = name.env,
                  "Heritability" = H2,
                  "coefvar" = CV)

h2cv %>% ggplot(aes(x = Heritability, y= Value, color = Environment)) +
  xlim(0,1)+xlab("Herdabilidade")+ylab("CV") + ylim(0.05, 0.2) +
  theme(legend.position = 'none')+ xlim(0.85, 1.0) +
  geom_text(aes(label = Environment), size = 4) 

data.frame(
  Environment = name.env,
  Value = c(H2, CV$Value),
  param = rep(c("Her","CV"),each = num.env)
) %>% ggplot()+
  geom_density(aes(x = Value, fill = param), alpha = .6)+
  scale_fill_manual(values = c("CV" = 'red1',
                               'Her' = 'darkolivegreen2'),
                    labels = c("CV" = "Coeficiente de Variação",
                               "Her" = "Herdabilidade"))+
  theme(legend.position = "bottom")+
  labs(fill = "", x = "", y="Frequência")

# OP and RMSD-------------------------------------------------------------------
OP = mean(mat.loadings.star[,1]) * as.matrix(fa1.scores.star)
OP = rownames_to_column(as.data.frame(OP), var = 'gen')
colnames(OP)[2] = "OP"

RMSD = data.frame(
  'gen' = rep(name.gen, num.env),
  'ST' = (EBLUPs_marg$EBLUP_marg - 
            (kronecker(mat.loadings.star[,1],diag(num.gen))) %*% fa1.scores.star)^2
) %>% group_by(gen) %>% summarise(RMSD = sqrt(mean(ST)))


# indice 2XProd 1x estabilidade
index = (2*(OP$OP-mean(OP$OP))/sd(OP$OP))-
  ((RMSD$RMSD - mean(RMSD$RMSD))/sd(RMSD$RMSD)) 

selection = data.frame('gen' = name.gen,
                       'OP' = OP$OP,
                       'RMSD' = RMSD$RMSD,
                       'Index' = index,
                       'reli' = PEVi$reli)

b <- selection %>% ggplot(aes(x = RMSD, y = OP))+
  geom_point(aes(color=reli),size = 2) +
  geom_vline(xintercept = 0,linetype="dashed", colour = "black")+
  geom_hline(yintercept = 0,linetype="dashed")+
  scale_x_continuous(name = "DPQM", 
                     breaks = seq(0, 500, by = 100), 
                     limits = c(0, 500)) +  
  scale_y_continuous(name = "DG", 
                     breaks = seq(-500, 700, by = 100), 
                     limits = c(-500, 700)) +
  gghighlight(gen %in% 
                selection[order(selection$Index, decreasing = T),][1:6,]$gen,
              use_direct_label = F)+
  geom_label_repel(aes(x = RMSD, y = OP, label = gen), size = 5, 
                   box.padding = .5, max.overlaps = 30)+
  scale_color_viridis_c(option = "viridis", begin = 0, end = 1, limits = c(0, 1)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 11))+
  labs(color = "Confiabilidade") 

plot(b)

#Ganho--------------------------------------------------------------------------
ai = predict(m5.ph, classify = 'gen')$pvals

Gan = (mean((ai[ai$gen %in% c('L33', 'L32', 'L22', 'L31', 'L28','L17'),])$predicted.value) -
         (mean(ai$predicted.value)))/(mean(ai$predicted.value)) * 100

#Correlações genéticas entre os ambientes---------------------------------------
corr = cov2cor(Gvcov)
diag(corr) = 0
col_fun = colorRamp2(c(-1,-.5,0,.5,1), viridis(5))
Heatmap(corr, col=col_fun,
        heatmap_legend_param = list(title = "Correlação"),
        row_names_side = "left",
        cluster_rows = TRUE, 
        cluster_columns = TRUE)