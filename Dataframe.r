library(tidyverse)


################################
#                              #
#          FUNCIONES           #
#                              #
################################

# Función para calcular la desviación típica:

desvtip <- function(x){
  sqrt(mean(x^2) - mean(x)^2)
}

# Función para calcular modelos lineales simples.

AjusteLineal <- function(dtf, y, x) {
  list(x=x, y=y, mod=lm(str_c(y, "~", x), dtf))
}

# Funciones para entrenar el modelo con unos datos y testearlo con otros.
# Devolvemos en una lista todos los datos útiles para el analisis del modelo como el coeficiente de determinación ajustado o el MSE.

AjusteLinealV2 <- function(dfTrain, dfTest, y, x) {
  mod <- lm(str_c(y, "~", x), dfTrain)
  calcR2(dfTest, mod, "IMC")
}

calcR2 <- function(df, mod, y) {
  MSE  <- mean((df[[y]] - predict.lm(mod, df)) ^ 2)
  varY <- mean(df[[y]] ^ 2) - mean(df[[y]]) ^ 2
  R2   <- 1 - MSE / varY
  aR2  <- 1 - (1- R2) * (nrow(df) - 1) / (nrow(df) - mod$rank)
  
  tibble(MSE=MSE, varY=varY, R2=R2, aR2=aR2)
}

# Funcion de Ajuste Lineal para multiples factores

AjusteMultiple <- function(df, y, x) {
  lm(str_c(y, "~", str_c(x, collapse="+")), df)
}

# Función para dibujar rectas de regresión 

dibujarModelos <- function(mod) {
  jpeg(str_c("c:/data/Plots/IMC~", mod$x, ".jpeg"))
  plot(df[[mod$x]], df[[mod$y]], xlab=mod$x, ylab=mod$y)
  abline(mod$mod, col="red")
  dev.off()
}

# Función para separar un conjunto de datos en 3 subconjuntos definiendo las proporciones en tanto por uno.

separarConjuntos <- function(dtf, p1, p2){
  rDf <- 1:nrow(dtf)
  rTrain <- sample(rDf, p1 * length(rDf))
  rTmp <- setdiff(rDf, rTrain)
  rTest <- sample(rTmp, p2 * length(rTmp))
  rVal <- setdiff(rTmp,rTest)
  
  list(train = dtf[rTrain,], test = dtf[rTest,], valid = dtf[rVal,])
}

# Funcion que iterativamente añade variables comprobando el coeficiente de detrminación ajustado hasta calcular un modelo lineal óptimo.

encontrarMejorAjuste <- function(dfTrain, dfTest, varPos) {
  bestVars <- character(0)
  aR2      <- 0
  
  repeat {
    aR2v <- map_dbl(varPos, ~calcModR2(dfTrain, dfTest, "IMC", c(bestVars, .)))
    i    <- which.max(aR2v)
    aR2M <- aR2v[i]
    if (aR2M <= aR2) break
    
    cat(sprintf("%1.4f %s\n", aR2M, varPos[i]))
    aR2 <- aR2M
    bestVars <- c(bestVars, varPos[i])
    varPos   <- varPos[-i]
  }
  
  mod <- AjusteMultiple(dfTrain, "IMC", bestVars)
  
  list(vars=bestVars, mod=mod)
}

calcModR2 <- function(dfTrain, dfTest, y, x) {
  mod <- AjusteMultiple(dfTrain, y, x)
  calcR2(dfTest, mod, y)$aR2
}



################################
#                              #
#          CODIGO              #
#                              #
################################

# Cargar Fichero en Memoria como Tibble definiendo como factores las columnas de variables cualitativas.

df <- read_csv("c:/data/Data.csv", 
                  col_types = cols(
                    .default = col_double(),
                    sexo = col_factor(),
                    dietaEsp = col_factor(),
                    nivEstPad = col_factor(),
                    nivEstudios = col_factor(),
                    nivIngresos = col_factor()))

# Construir una nueva columna IMC a partir de las columnas de peso y altura.

df <- df %>%  mutate(IMC = peso / (altura) ^2) 
df$IMC <- round(df$IMC,digits = 2)

# Eliminar filas con NA en alguna de sus columnas.

df <- df %>% drop_na()

# Calcula las medias y desviaciones tipicas de las variables numericas.

ListaMedias <- df %>% summarise_if(is.numeric, mean)

ListaDesviaciones <- df %>% summarise_if(is.numeric, desvtip)

#  Calcula los coeficientes de regresión y determinacion para las regresiones unidimensionales.

modelos <- names(df[4:15]) %>% map(AjusteLineal, dtf=df, y= "IMC")

coeficientes <- modelos %>% map(3)
R2 <- coeficientes %>% map(summary) %>% map("r.squared")

# Representar graficos de dispersion o boxplots 

modelos %>% walk(dibujarModelos)

# 7) Separarar el conjunto original en 3: Entrenamiento(60%), Validacion(20%) y Test(20%)

dfs <- separarConjuntos(df, .6, .5)

# 8) Seleccionar la mejor variable usando los conjuntos de entremiento y test.

CoeficientesV2 <- names(df[4:15]) %>% map(AjusteLinealV2, dfTrain=dfs$train, dfTest=dfs$test, y="IMC") %>% map(3)

MejorVariable <- names(df)[3+which.max(CoeficientesV2)]

MejorVariable
max(as_vector(CoeficientesV2))

# 9) Seleccionar el modelo optimo lineal  y evaluar con el conjunto de validación.

# Generamos un vector con todas las variables predictoras

varPos1 <- names(df[4:15])
varPos2 <- crossing(var1=varPos1, var2=varPos1) %>% pmap_chr(str_c, sep=":")

# Mejor modelo con variables simples.

bestMod1 <- encontrarMejorAjuste(dfs$train, dfs$test, varPos1)
calcR2(dfs$valid, bestMod1$mod, "IMC")

# Mejor modelo con variables compuestas.

bestMod2 <- encontrarMejorAjuste(dfs$train, dfs$test, varPos2)
calcR2(dfs$valid, bestMod2$mod, "IMC")

