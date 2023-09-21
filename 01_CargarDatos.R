# Cargar el conjunto de datos
# En algunos casos es más práctico cargar los datos desde su publicación en la web
url <- "https://raw.githubusercontent.com/jairoescrito/datasets/master/GermanCredit.csv"
# https://archive.ics.uci.edu/dataset/144/statlog+german+credit+data

# Si los datos se tienen en un repositorio local, es necesario copiar el archivo en el
# directorio de trabajo, o bien, en una variable tipo url, la ruta completa de acceso al
# archivo 
# Consultar mi directorio de trabajo
getwd()
# Modificar el directorio de trabajo
setwd("C:\\Directorio\de\Trabajo")

# Descargar el conjunto de datos y guardarlo en un arreglo de datos tipo dataframe
dataset <- read.csv(url)
Index <- sample(1:nrow(dataset),size=round(nrow(dataset)*0.05,0),replace=FALSE) 
dataset$purpose[Index] <- NA
Index <- sample(1:nrow(dataset),size=round(nrow(dataset)*0.05,0),replace=FALSE)
dataset$amount[Index] <- NA
Index <- sample(1:nrow(dataset),size=round(nrow(dataset)*0.03,0),replace=FALSE)
dataset$housing[Index] <- NA

write.csv("dataset.csv",dataset)


rm(url) #eliminar la variable del entorno es útil para optimizar el uso de recursos

## 1. Entendimiento del dataset
# Conocer cada una de las variables que compponen el dataset, el tipo de datos y las pposibles
# variables de baja relevancia en el análisis a realizar

# Tamaño del dataset
cat(ncol(dataset), "filas y", ncol(dataset),"columnas")
head(dataset)
'
status: estado de ingresos en cuentas (movimientos)
duration: duración, en meses, del crédito solicitado
credit_history: historial crediticio
purpose: propósito del prestamo
amount: monto del prestamo
savings: ahorros
employment_duration: antigüedad en el empleo actual
installment_rate: proporción de recursos qque se destinan a pago de deudas
personal_status_sex: estado civil y género
other_debtors: otras deudas
present_residence: antigüedad en la residencia actual 
property: Propiedades o activos
age: edad
other_installment_plans: otros planes de pago
housing: tipo de vivienda
number_credits: número de créditos en el banco
job: empleo
people_liable: personas a cargo
telephone: teléfono a nombre del solicitante
foreign_worker: extranjero
credit_risk: riesgo de crédito
'
# Análisis inicial del tipo de datos por variable (columna): se verificar el tipo de dato de cada
# variable corresponde al tipo de dato que contiene la columna, por ejemplo, que las columnas que
# tienen números estén guardados como números.
sapply(dataset,class)
unique(sapply(dataset,class)) # Obtenemos los tipos de datos que tenemos en el dataset
sum(sapply(dataset,class) == "character") # Cantidad de variables tipo caracter
sum(sapply(dataset,class) == "integer") # cantidad de variables tipo entero
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
index <- array(which(sapply(dataset,class) == "integer"))
summary(dataset[,index])




