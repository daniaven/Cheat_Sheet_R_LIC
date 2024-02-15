# 1) Crear Data.Frame de ejemplo ----

# Este dataframe contendrá columnas de distinta estructura para poder sacarle
# el mayor jugo posible. Se llamará df_ejemplo (df = dataframe)

df_ejemplo <- data.frame(
  Columna1 = factor(rep(c("Si", "No"), each = 5)[1:10]),
  Columna2 = factor(rep(c("standard", "intermediate", "high"), times = 3)[1:10]),
  Columna3 = sample(0:1, 10, replace = TRUE),
  Columna4 = runif(10, 0, 1),
  Columna5 = sample(1:10, 10, replace = TRUE),
  Columna6 = factor(rep(c("hipodiploidía", "ETV6-RUNX1", "TCF3-HLF", "P2RY8-CRLF2", "Ph-like", "DUX4"), times = 2)[1:10]),
  Columna7 = sample(1:100, 10),
  Columna8 = paste("Muestra", 1:10)
)

# para ver qué clase de objeto creé uso:
class(df_ejemplo)

# para ver qué formato tiene cada columna uso:
str(df_ejemplo)

# 2) Modificar Rownames y Colnames ----

# RowNames y ColNames es el nombre de las filas y columnas respectivamente (duh)
# No forman parte de los valores dentro del dataframe pero facilita la 
# interpretación de éstos valores (por ejemplo, la Columna1 toma valores de "Si"
# o "No" pero ¿qué significa eso? ¿Sí/No votó? ¿Sí/No recayó? ¿Sí/No aprobó?)

# Cambiar ColNames:
colnames(df_ejemplo) <- c("Muerte","Grupo_Riesgo","Toxicidad_Aguda",
                          "Probabilidad_recaída", "Edad","Mutación",
                          "Sobrevida_libre_de_Evento", "Muestra_ID") 
library(tidyverse)
View(df_ejemplo)

#Cambiar RowNames:

# Se podría hacer del mismo modo en que lo hice para ColNames solo que usando
# rownames() en lugar de colnames(). Sin embargo como ya tengo una columna con 
# el ID de las muestras voy a hacer que sea igual a esa columna:

rownames(df_ejemplo) <- df_ejemplo$Muestra_ID

# Ahora se podría eliminar la columna llamada Muestra_ID ya que esa información
# ya está en el RowNames, yo no lo voy a hacer porque no es necesario eliminarlo
# todavía.... 

# 3) Eliminar columna/fila ----

# Siempre que hago una operación está bueno crear un nuevo objeto y mantener el 
# original sin alterar.

# Eliminar una columna:
df_ejemplo_edit <- df_ejemplo[,-2] # con esto elimino la columna 2
df_ejemplo_edit <- df_ejemplo[,-c(2:5)] # con esto elimino las columnas de la 
                                        # 2 a la 5 (inclusive)

# Eliminar una fila:
df_ejemplo_edit <- df_ejemplo[-2,] # con esto elimino la fila 2
df_ejemplo_edit <- df_ejemplo[-c(2:5),] # con esto elimino las filas de la 
                                        # 2 a la 5 (inclusive)

# Eliminar filas que tengan cierto valor en una columna
df_ejemplo_edit <- subset(df_ejemplo, Muerte != "Si") # Elimino los pacientes que murieron

df_ejemplo_edit <- subset(df_ejemplo, !(Muerte == "Si" & Toxicidad_Aguda == 1)) # Elimino los
                          # pacientes que murieron Y QUE ADEMÁS sufrieron Toxicidad Aguda

df_ejemplo_edit <- subset(df_ejemplo, Probabilidad_recaída >= 0.5) # Elimino filas con valores
                          # de probabilidad de recaída menor a 0,5

# 4) Filtrar filas con cierto valor ----
library(tidyverse)
# Filtrar valores mayores o menores
df_ejemplo_edit <- df_ejemplo%>% filter(Probabilidad_recaída>0.5)
view(df_ejemplo_edit)

# Filtrar valor exacto, puede ser numérico o character
df_ejemplo_edit <- df_ejemplo%>% filter(Muerte=="Si")
view(df_ejemplo_edit)

# Filtrar más de un valor
df_ejemplo_edit <- df_ejemplo%>%filter(Mutación%in% c("ETV6-RUNX1", "P2RY8-CRLF2"))
view(df_ejemplo_edit)


# 5) Crear Matriz ----
m_ejemplo <- matrix(1:9, nrow = 3, ncol = 3)
# se puede nombrar los rownames y los colnames durante la creacion
m_ejemplo <- matrix(1:9, nrow = 3, dimnames = list(c("X","Y","Z"), c("A","B","C")))

# 6) Convertir dataframe en matriz ----
df_ejemplo.2 <- data.frame(
  Neutrofilos = round(runif(10), 2),
  Monocitos = round(runif(10), 2),
  Macro.M1 = round(runif(10), 2),
  Macro.M2 = round(runif(10), 2),
  Dendriticas = round(runif(10), 2),
  B.cells = round(runif(10), 2),
  T.cells.CD4 = round(runif(10), 2),
  T.cells.CD8 = round(runif(10), 2),
  T.cells.helper = round(runif(10), 2),
  NK.cells = round(runif(10), 2)
)

m_ejemplo <- as.matrix(df_ejemplo.2)

# 7) Eliminar un objeto del Global enviroment -----
remove(df_ejemplo.2) # este objeto lo vamos a usar más adelante, así que si 
                     # quieren pueden volver a crearlo

# 8) Cambiar rownames de matriz (igual que para data.frames)----
# Crear un vector con los nuevos nombres de fila
rownames <- c("Muestra_1", "Muestra_2", "Muestra_3", "Muestra_4", "Muestra_5",
                     "Muestra_6","Muestra_7","Muestra_8","Muestra_9","Muestra_10")

# Asignar los nuevos nombres de fila a la matriz
rownames(m_ejemplo) <- rownames

# 9) Transponer matriz / dataframe ----

# Transponer (que las columnas pasen a ser filas y las filas columnas)
m_ejemplo_edit <- t(m_ejemplo) 
df_ejemplo_edit <- t(df_ejemplo) 
# notar que ahora el dataframe es una matriz
class(df_ejemplo_edit)

# 10) Pivotear data frame ----

# Vamos a usar el objeto que creamos en el paso 6, pero primero vamos a prepararla:
rownames(df_ejemplo.2) <- df_ejemplo$Muestra_ID # le ponemos rownames
df_ejemplo.2 <- t(df_ejemplo.2) # transponemos
df_ejemplo.2 <- as.data.frame(df_ejemplo.2) # convertir la matriz transpuesta a data.frame
df_ejemplo.2$tipo_celular <- rownames(df_ejemplo.2) # hacer una columna con el dato de rownames

library(tidyverse)
view(df_ejemplo.2)

df_ejemplo.2.pivot <- pivot_longer(df_ejemplo.2, # dataframe to be pivoted
                                       cols = "Muestra 1":"Muestra 10", # column names to be stored as a SINGLE variable
                                       names_to = "Muestras", # name of that new variable (column)
                                       values_to = "Expresion") # name of new variable (column) storing all the values (data)

view(df_ejemplo.2.pivot) # esto es muy útil para hacer gráficos o análisis estadístico
