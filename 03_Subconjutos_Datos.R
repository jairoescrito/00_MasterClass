library(tidyverse)
# No en todos los casos los outliers son sujetos a eliminación, es posible que los
# mismos requieran un "análisis" adicional fuera del conjunto global de datos
# Creación de un subconjunto de datos con filter
df_amount <- filter(dataset,dataset$amount>4000)
df_duration <- filter(dataset,dataset$duration>36)
# Creación de un subconjunto de datos con subset
df_age <- subset(dataset, dataset$age>55)
# Creación de grupos mediante funciones de agregación
# Monto promedio de solicitudes de préstamo para personas mayores de 55 años
av_age_mayores <- df_age |> 
                    group_by(age) |>
                      summarise(prom = mean(amount))
# Monto promedio de solicitudes detalladas por plazo
av_plazo <- df_duration %>%
              group_by(duration) %>%
                summarise(prom = mean(amount))
# Construcción de una tabla de frecuencia con group_by
tf_1<- dataset |>
        group_by(purpose,personal_status_sex) |>
          summarise(Frec = length(purpose), .groups = "drop")


# 2. Data Tyding - Preparación, limpieza y organización de datos
# Se ejecutan una serie de tareas a fin de limpiar, transformar o reorganizar el conjunto de datos
# a fin de hacer más fácil su análisis:

# 1) El dataset debe tener una estructura de variables (columnas) y registros (filas)
# A veces, los valores de una variable aparecen como variables, se debe eliminar esa dinamización
# 2) Desagregar los valores de variables fundidos (cuando dos o mas variables están en una sola)
# 3) Ajustar el tipo o clase de dato según corresponda a la información que contiene cada variable
# 4) Tratar los NA's
# 5) Tratamiento de outliers


# Modificación de los tipos de variables

# Las variables tipo "caracter" requieren ser ajustadas a tipo "factor" esto es, convertirlas a 
# variables categóricas. En las variables tipo factor se definen los niveles (o categorías) lo cual 
# facilita el análisis de los datos. Consultar los valores únicos que toma cada una de las variables
# tipo caracter ayuda a identificar aquellas que pueden transformarse en categóricas. 
# También, es importante tener en cuenta que no todas las variables numéricas lo son en sí,
# Algunas son variable dummie (son variables discretas que toman un número limitado de valores)
# Usar la función summary (que entrega un resumen de los datos) también puede ser útil
summary(dataset)
# Adicional a las variables tipo caracter, se tiene las variables 
# installment_rate, present_residence, number_credits, people_liable, credit_risk
# Pese a que son númericas, no es lógico un análisis numérico, su naturaleza es más
# categórica, son variables tipo dummie
# Las variables tipo caracter se van a ajustar todas a categóricas (factor)
# Algunas tipo entero deben ser ajustadas, también, a tipo factor, no obstante, hay que
# tener claramente identificadas cuales. Los resultados de summary proporcionan información
# si se requiere ver solo las variables numéricas
# Observado el resumen de variables, solo las variables "duration", "amount" y "age" 
# tienen caracter numérico (integer)
index_num <- which(names(dataset) %in% c("duration","amount","age")) # variable que contiene los índices de las columnas
# que no voy a modificar. Con el ciclo modifico una a una las variables del dataset
i = 1
for (i in (1:ncol(dataset))){
  if (!(i %in% index_num)){
    dataset[,i] <- as.factor(dataset[,i]) 
  }
}
rm(i) # Elimino las variables que no necesito más
summary(dataset) # Resumen de las variables con el ajuste en el dataset

# Identificación y tratamiento de los Missing Values (NA's)

# Es común que en los dataset tengan NA's, esto se debe a debilidades en el proceso de captura de datos
# Un NA's hace referencia a un dato que no fue recolectado u observado pese a que el mismo existe
# Un NA (en términos de bases de datos) es diferente a un valor nulo, para este último se asume que el mismo
# no existe

sum(is.na(dataset)) #Cantidad total de missing values 
library(VIM)
nas <- aggr(dataset) # Gráfica de identificación de los NA's
summary(nas) # Resumen numérico de la identificación de NA's
var_nas<-nas$missings$Variable[nas$missings[2]>0]
rm(nas)

# Una vez identificados los NA's es necesario generarles un tratamiento
# En el tratamiento se pueden contemplar varias opciones: eliminación de la observación
# Imputar por media o mediana (para variables numéricas), moda o proporciones (para categóricas)
# o usar el método de vecinos cercanos (kNN): media de los k vecinos para variables numéricas y
# mayor proporción para las variables categóricas

# Variables con NA's: purpose, amount y housing
# Imputación con kNN para k = 5 
dataset<-kNN(dataset,k=5,variable=var_nas, imp_var = FALSE)
rm(var_nas)
# Identificación de outliers
summary(dataset[,index_num]) # Revisión de las variables numéricas
caja <- boxplot(dataset[,index_num]) # Gráfico de caja 
caja <- boxplot(scale(dataset[,index_num])) # Datos centrados en la media
# LS = Q3 + 1.5 * (Q1 - Q3) 
# LI = Q1 - 1.5 * (Q1 - Q3)
caja <- boxplot(scale(dataset[,index_num]), outcol="red") # Detalle de los "outliers"
# Para tener el valor real de las observaciones, en cada variable, de los "outliers"
# se hace el cálculo sin scale
caja <- boxplot(dataset[,index_num], outcol="red")
# Valores que la gráfica identifica como valores atípicos
# Group contiene el número de la variable y caja el dato observado atípico
outliers <- data.frame(caja$group,caja$out)
library(tidyverse)
outliers <- outliers %>% # Reemplazar los números por los nombres de las variables
  mutate(caja.group = case_when(
    caja.group==1 ~ names(dataset[index_num[1]]),
    caja.group==2 ~ names(dataset[index_num[2]]),
    caja.group==3 ~ names(dataset[index_num[3]]),
    )
  )
# Separar en dataframes distintos los outliers por variables
list_outliers <- split(outliers, f = outliers$caja.group)
rm(outliers,caja,index_num)
names(list_outliers)
# De acuerdo al conocimiento del negocio (y de los datos) se decide si los puntos marcados
# como outliers se eliminan del dataset
# Si se decidiesen eliminar se debe identificar en qué filas están los valores atípicos y
# luego se excluyen del dataset
# Para el dataset en cuestión las variables age y duration no contienen datos atípicos
# los valores marcados como tal se consideran dentro de los valores típicos que una variable
# de este tipo puede tomar.
# Bajo el supuesto que para la variable "amount" si existen outliers, se procederá entonces a 
# eliminar estos puntos, es de resaltar que se debe eliminar la observación completa.
# Para esto, se requiere conocer el índice de fila (en el dataset) donde están los outliers 
f_out <- which(dataset$amount %in% list_outliers$amount$caja.out) # identifico las filas de 
# los registros con observaciones atípicas
df<- dataset[-f_out,] # dataset sin los registros que contienen las observaciones atípicas
rm(df,f_out,list_outliers)