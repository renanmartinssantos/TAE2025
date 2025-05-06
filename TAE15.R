# Carregar pacotes
if (!require("glmnet")) install.packages("glmnet")
if (!require("mgcv")) install.packages("mgcv")
library(glmnet)
library(mgcv)

# Transformações
esforco3$altura <-  esforco3$altura/10
esforco3$IMC <-  esforco3$peso/(esforco3$altura/100)^2
esforco3$SC <- sqrt(esforco3$altura*esforco3$peso/ 3600)
esforco3$SC2 <-  0.007184 * (esforco3$peso^0.425) * (esforco3$altura^0.725)

# Remover linhas com NA
esforco3 <- na.omit(esforco3)

# Criar matriz de preditoras
X <- data.matrix(esforco3[, c("altura", "peso", "IMC", "SC")])

# Função para ajustar modelos e armazenar resultados
ajusta_modelos_vo2 <- function(y, nome) {
  
  # Regressão Linear
  modelo_lm <- lm(y ~ altura + peso + IMC + SC, data = esforco3)
  pred_lm <- predict(modelo_lm)
  rmse_lm <- sqrt(mean((y - pred_lm)^2))
  r2_lm <- 1 - sum((y - pred_lm)^2) / sum((y - mean(y))^2)
  SMLM <-summary(modelo_lm)
  
  # Ridge
  ridge <- cv.glmnet(X, y, alpha = 0)
  pred_ridge <- predict(ridge, s = ridge$lambda.min, newx = X)
  rmse_ridge <- sqrt(mean((y - pred_ridge)^2))
  r2_ridge <- 1 - sum((y - pred_ridge)^2) / sum((y - mean(y))^2)
  SMR <- sqrt(ridge$cvm[ridge$lambda == ridge$lambda.min])
  
  # Lasso
  lasso <- cv.glmnet(X, y, alpha = 1)
  pred_lasso <- predict(lasso, s = lasso$lambda.min, newx = X)
  rmse_lasso <- sqrt(mean((y - pred_lasso)^2))
  r2_lasso <- 1 - sum((y - pred_lasso)^2) / sum((y - mean(y))^2)
  SML <- sqrt(lasso$cvm[lasso$lambda == lasso$lambda.min])
  
  # Elastic Net
  elastic <- cv.glmnet(X, y, alpha = 0.5)
  pred_elastic <- predict(elastic, s = elastic$lambda.min, newx = X)
  rmse_elastic <- sqrt(mean((y - pred_elastic)^2))
  r2_elastic <- 1 - sum((y - pred_elastic)^2) / sum((y - mean(y))^2)
  SME <- sqrt(elastic$cvm[elastic$lambda == elastic$lambda.min])
  
  # GAM
  gam_model <- gam(y ~ s(altura) + s(peso) + s(IMC) + s(SC), data = esforco3)
  pred_gam <- predict(gam_model)
  rmse_gam <- sqrt(mean((y - pred_gam)^2))
  r2_gam <- 1 - sum((y - pred_gam)^2) / sum((y - mean(y))^2)
  SMG <- summary(gam_model)$r.sq
  
  # Retornar resultados como uma lista
  return(list(
    Modelo = c("Regressão Linear", "Ridge", "Lasso", "Elastic Net", "GAM"),
    RMSE = c(rmse_lm, rmse_ridge, rmse_lasso, rmse_elastic, rmse_gam),
    R2 = c(r2_lm, r2_ridge, r2_lasso, r2_elastic, r2_gam),
    SMLM = SMLM$r.squared,
    SMR = SMR,
    SML = SML,
    SME = SME,
    SMG = SMG
  ))
}

# Rodar para cada variável de VO2 e armazenar resultados
resultados_vo2rep <- ajusta_modelos_vo2(esforco3$vo2rep, "VO2REP")
resultados_vo2lan <- ajusta_modelos_vo2(esforco3$vo2lan, "VO2LAN")
resultados_vo2pcr <- ajusta_modelos_vo2(esforco3$vo2pcr, "VO2PCR")
resultados_vo2fcpico <- ajusta_modelos_vo2(esforco3$vo2fcpico, "VO2FCPICO")

# Criar uma tabela de comparação
tabela_comparacao <- data.frame(
  Modelo = rep(c("Regressão Linear", "Ridge", "Lasso", "Elastic Net", "GAM"), 4),
  RMSE = c(resultados_vo2rep$RMSE, resultados_vo2lan$RMSE, resultados_vo2pcr$RMSE, resultados_vo2fcpico$RMSE),
  R2 = c(resultados_vo2rep$R2, resultados_vo2lan$R2, resultados_vo2pcr$R2, resultados_vo2fcpico$R2),
  Variavel = rep(c("VO2REP", "VO2LAN", "VO2PCR", "VO2FCPICO"), each = 5),
  SMLM = c(resultados_vo2rep$SMLM, resultados_vo2lan$SMLM, resultados_vo2pcr$SMLM, resultados_vo2fcpico$SMLM),
  SMR = c(resultados_vo2rep$SMR, resultados_vo2lan$SMR, resultados_vo2pcr$SMR, resultados_vo2fcpico$SMR),
  SML = c(resultados_vo2rep$SML, resultados_vo2lan$SML, resultados_vo2pcr$SML, resultados_vo2fcpico$SML),
  SME = c(resultados_vo2rep$SME, resultados_vo2lan$SME, resultados_vo2pcr$SME, resultados_vo2fcpico$SME),
  SMG = c(resultados_vo2rep$SMG, resultados_vo2lan$SMG, resultados_vo2pcr$SMG, resultados_vo2fcpico$SMG)
)

# Exibir a tabela de comparação
print(tabela_comparacao)      

