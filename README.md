# Métodos Estadisticos para la Computación 2019/20

## Trabajo de Modelización Estadistica

1. Cargar en memoria el fichero CSV como tibble, las variables cualitativas seran leídas como factores.
2. Construir una nueva columna llamada IMC = Peso / Altura ^2.
3. Eliminar las filas que contengan NA en alguna de sus columnas.
4. Calcular las medias y desviaciones típicas de las variables numericas.
5. Calcular los coeficientes de regresión y determinación para las 12 regresiones lineales unidimensionales.
6. Representar graficamente las rectas de regresion de los modelos anteriores.
7. Separar el conjunto de datos original en 3 subconjuntos, entrenamiento(60%), test(20%) y validación(20%).
8. Seleccionar cual de las 12 variables es mejor variable predictora de IMC, entrenando con el conjunto de entrenamiento y testeando con el de test.
9. Seleccionar un modelo óptimo de regresión lineal, entrenando con el conjunto de entrenamiento y testeando en el conjunto de test el coeficiente de determinación ajustado, utilizando la técnica de ir añadiendo progresivamente la mejor variable.
10. Evaluar el resultado con el conjunto de validación.