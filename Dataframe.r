library(tidyverse)

# 1) Cargar Fichero en Memoria como Tibble definiendo como factores las columnas de variables cualitativas.

# Cargo como factor aquellas columnas del dataframe que representen posibles alternativas, Sexo (V/M), dieta(S/N) y las columnas que representan los diferentes niveles socioeconomicos.

df <- read_csv("'c:/data/Data.csv'", 
                  col_types = cols(
                    .default = col_double(),
                    sexo = col_factor(),
                    dietaEsp = col_factor(),
                    nivEstPad = col_factor(),
                    nivEstudios = col_factor(),
                    nivIngresos = col_factor()))

# 2) Construir una nueva columna IMC a partir de las columnas de peso y altura.

# La columna IMC por error ya estaba incluida en el dataframe, asi que la recalculo con su formula y la redondeo para que esté en el mismo formato que aparecia originalmente.

df <- df %>%  mutate(IMC = peso / (altura) ^2) 
df$IMC <- round(df$IMC,digits = 2)

# 3) Eliminar filas con NA en alguna de sus columnas.

# Hago uso de la función drop_na incluida en la librería tidyverse para elimianr aquellas filas que tengan valores no definidos.

df <- df %>% drop_na()

# 4) Calcula las medias y desviaciones tipicas de las variables numericas.

# Uso una funcion para calcular la desviación típica y filtramos aquellas columnas del dataframe que sean numericas con la función "summarise_if" generando una lista de medias y de desviaciones.

desvtip <- function(x){
  sqrt(mean(x^2) - mean(x)^2)
}

ListaMedias <- df %>% summarise_if(is.numeric, mean)

ListaDesviaciones <- df %>% summarise_if(is.numeric, desvtip)

# 5) Calcula los coeficientes de regresión y determinacion para las regresiones unidimensionales.

# Hago una funcion para calcular los 12 ajustes lineales, y lo guardo en un objeto utilizando la función map y filtrando las 3 primeras columnas del dataframe que son el propio IMC y peso y altura que usamos para construir el IMC
# Finalmente mediante las funciones map saco los 2 parametros que me interesan, los coeficientes de regresion y el R^2 o coeficiente de correlación.

AjusteLineal <- function(dtf, y, x) {
  list(x=x, y=y, mod=lm(str_c(y, "~", x), dtf))
}

modelos <- names(df[4:15]) %>% map(AjusteLineal, dtf=df, y= "IMC")

coeficientes <- modelos %>% map(3)
R2 <- coeficientes %>% map(summary) %>% map("r.squared")

# 6) Representar graficos de dispersion o boxplots 

# Utilizo una función para dibujar los 12 modelos, esta funcion nos dibuja la nube de puntos con el comando plot y la recta de regresión con el comando abline y nos lo guarda en disco
# Con estos comandos en caso de que la variable sea cualitativa nos dibujara un boxplot. Para realizar estas operaciones de I/O uso la función walk 


dibujarModelos <- function(mod) {
  jpeg(str_c("'c:/data/Plots/IMC~", mod$x, ".jpeg"))
  plot(df[[mod$x]], df[[mod$y]], xlab=mod$x, ylab=mod$y)
  abline(mod$mod, col="red")
  dev.off()
}

modelos %>% walk(dibujarModelos)