library(tidyverse)

# 1) Cargar Fichero en Memoria como Tibble definiendo como factores las columnas de variables cualitativas.

df <- read_csv('c:/data/Data.csv', 
                  col_types = cols(
                    .default = col_double(),
                    sexo = col_factor(),
                    dietaEsp = col_factor(),
                    nivEstPad = col_factor(),
                    nivEstudios = col_factor(),
                    nivIngresos = col_factor()))

# 2) IMC = peso / altura ^2 

df <- df %>%  mutate(IMC = peso / (altura) ^2) 
df$IMC <- round(df$IMC,digits = 2)

# 3) Eliminar filas con NA en alguna de sus columnas

df <- df %>% drop_na()

# 4) Calcula las medias y desviaciones tipicas de las variables numericas.