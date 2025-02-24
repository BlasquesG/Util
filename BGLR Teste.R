
library(openxlsx)
library(dplyr)
library(sommer)
library(stringr)
library(BGLR)
library(tibble)


data <- read.xlsx("3.xlsx")

data <- transform(
  data,
  gen   = factor(gen),  
  rep   = factor(rep),  
  env   = factor(env),  
  yield = as.numeric(yield) 
)


data.split <- split(x= data, f= data$env)

indiv.results = lapply(dat.split, function(x){
  
  message("====> Environment: ", unique(x$env))
  
  x = transform(x, genotype = as.factor(gen), block = as.factor(rep))
  
  mfix = mmer(fixed = yield ~ block + genotype,
              rcov = ~ units,
              data = x)
  blue = coef(mfix)
  mu = blue[1, 3]
  blue = x |> reframe(yield = mean(yield), .by = c("genotype")) |>
    left_join(
      blue[grepl("genotype", blue$Effect), ] |>
        mutate_at("Effect", str_replace, "genotype", '') |>
        rename(genotype = Effect),
      by = 'genotype'
    ) |>
    mutate(Estimate = ifelse(is.na(Estimate), 0, Estimate) + mu,
           env = unique(x$year_loc)) |> 
    dplyr::select(-Trait, -yield) |> 
    rename(blue = Estimate) |> relocate(env, .before = genotype)
  
  
  mrand = mmer(fixed = yield ~ block, 
               random = ~ genotype, 
               rcov = ~units,
               data = x)
  lrtest = anova(mrand, mmer(fixed = yield ~ block, data = x))
  vc = summary(mrand)$varcomp
  
  list(
    params = data.frame(
      s2g = vc[1,1],
      s2e = vc[2,1],
      H2 = vc[1,1]/(vc[1,1] + (vc[2,1]/nlevels(x$block))),
      CV = sqrt(vc[2,1])/mean(x$yield, na.rm = TRUE),
      mu = mean(x$yield, na.rm = TRUE),
      lrt = as.numeric(strsplit(lrtest$PrChisq[2], split = ' ')[[1]][1])
    ),
    blues = blue
  )
})
params = do.call(rbind, lapply(indiv.results, function(x) x$params)) |> 
  rownames_to_column("env")
blues = do.call(rbind, lapply(indiv.results, function(x) x$blues)) 




envcov <- read.xlsx("IACEnvExperimentos.xlsx")
colnames(envcov)[1] <- "env"

envcov <- envcov[ envcov$env %in% unique(blues$env), ]

rownames(envcov) <- envcov$env
envcov <- envcov[ , -1]

envcov_numeric <- envcov[, sapply(envcov, is.numeric)]

W <- scale(envcov_numeric, center = TRUE, scale = TRUE)

q <- ncol(W)
Omega <- tcrossprod(W) / q





Zenv <- model.matrix(~ -1 + env, data = blues)


Zgen <- model.matrix(~ -1 + genotype, data = blues)


ZZ_env <- tcrossprod(Zenv)


ZOZ_env <- tcrossprod(tcrossprod(Zenv, Omega), Zenv)


G_gen <- tcrossprod(Zgen)


GEI_env <- G_gen * ZZ_env

GEI_w <- G_gen * ZOZ_env






ETA1 <- list(
  env      = list(V = eigen(ZZ_env)$vectors, d = eigen(ZZ_env)$values, model = "RKHS"),
  w        = list(V = eigen(ZOZ_env)$vectors, d = eigen(ZOZ_env)$values, model = "RKHS"),
  gen      = list(V = eigen(G_gen)$vectors, d = eigen(G_gen)$values, model = "RKHS"),
  genxenv  = list(V = eigen(GEI_env)$vectors, d = eigen(GEI_env)$values, model = "RKHS"),
  genxw    = list(V = eigen(GEI_w)$vectors, d = eigen(GEI_w)$values, model = "RKHS")
)


ETA2 <- list(
  env      = list(V = eigen(ZZ_env)$vectors, d = eigen(ZZ_env)$values, model = "RKHS"),
  gen      = list(V = eigen(G_gen)$vectors, d = eigen(G_gen)$values, model = "RKHS"),
  genxenv  = list(V = eigen(GEI_env)$vectors, d = eigen(GEI_env)$values, model = "RKHS")
)


ETA3 <- list(
  w        = list(V = eigen(ZOZ_env)$vectors, d = eigen(ZOZ_env)$values, model = "RKHS"),
  gen      = list(V = eigen(G_gen)$vectors, d = eigen(G_gen)$values, model = "RKHS"),
  genxw    = list(V = eigen(GEI_w)$vectors, d = eigen(GEI_w)$values, model = "RKHS")
)


set.seed(123)


mod1 <- BGLR(y = blues$blue, ETA = ETA1, nIter = 12000, burnIn = 2000, verbose = T)


mod2 <- BGLR(y = blues$blue, ETA = ETA2, nIter = 12000, burnIn = 2000, verbose = T)


mod3 <- BGLR(y = blues$blue, ETA = ETA3, nIter = 12000, burnIn = 2000, verbose = T)

