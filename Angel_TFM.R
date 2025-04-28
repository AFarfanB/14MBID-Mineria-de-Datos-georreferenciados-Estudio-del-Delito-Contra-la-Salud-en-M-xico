## Universidad Internacional de Valencia
## Directora: Nery Sofia Huerta, PhD
## Estudiante: Angel Farfán Bernal

## ******************************************************************************

data<-Incidencia_del_fuero_federal_2012_2024_nov2024
table(data$CONCEPTO)
# Filtrar el concepto unicamente para CONTRA LA SALUD
data0<-subset(data,CONCEPTO=="CONTRA LA SALUD")
   

# Da el numero de entidades unicas que hay en data1
#length(table(data1$ENTIDAD))
#data1$SUMA=rowSums(data1[7:18]) # Suma los delitos de todos los meses para cada fila

#T_A=aggregate(data1$SUMA, list(data1$AÑO), sum,na.rm=TRUE)
# Ajustar los límites del eje Y basados en los datos
#ylim_max = max(T_A$x) * 1.1  # Aumenta un 10% para mejor visualización
#barplot(T_A$x, names.arg=T_A$Group.1, horiz=FALSE, las=2, cex.names=1, ylim=c(0, ylim_max))
#T_A

################### REESTRUCTURACION DE LOS DATOS #######################
# Instalar paquetes si no están instalados
#if (!require(tidyr)) install.packages("tidyr", dependencies=TRUE) #instalar

# Cargar librerías necesarias
library(tidyr)
library(dplyr)
library(ggplot2)


# Cargar los datos (suponiendo que están en un data frame llamado 'data1')
data1 <- data0 %>%
  pivot_longer(cols = ENERO:DICIEMBRE,  # Selecciona las columnas de los meses
               names_to = "MES",        # Nueva columna con los nombres de los meses
               values_to = "VALOR")     # Nueva columna con los valores correspondientes
# Mostrar la nueva estructura
print(data1)

## *****************************************************************************

############################## AÑOs 2012 - 2024  #########################################

T_AÑO_2012=aggregate(data1$VALOR, list(data1$AÑO), sum,na.rm=TRUE)
# Renombrar columnas 
colnames(T_AÑO_2012) <- c("AÑO", "VALOR_TOTAL")
# Ajustar los límites del eje Y basados en los datos
#ylim_max = max(T_AÑO$VALOR_TOTAL, na.rm = TRUE) * 1.3  # Aumenta un 10% para mejor visualización

#Convertir la columna año en factor para que aparezcan todos en la grafica
T_AÑO_2012$AÑO <- factor(T_AÑO_2012$AÑO, levels = sort(unique(T_AÑO_2012$AÑO)))

ggplot(T_AÑO_2012, aes(x = AÑO, y = VALOR_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  theme_minimal() +
  labs(title = "Número de registros por Año 2012-2024", x = "Año", y = "Registros") +
  scale_y_continuous(breaks = seq(0, max(T_AÑO_2012$VALOR_TOTAL + 500), by = 2000)) +  # Aumentar frecuencia
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),      # Tamaño de texto eje X
    axis.text.y = element_text(size = 12),                             # Tamaño de texto eje Y
    axis.title.x = element_text(size = 13),                            # Etiqueta eje X
    axis.title.y = element_text(size = 13),
    
    axis.line.x = element_line(color = "gray", linewidth = 0.8),
    axis.line.y = element_line(color = "gray", linewidth = 0.8),
    panel.grid.major = element_line(color = "gray75", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )

T_AÑO_2012


## *******************  Filtrar por el año 2012 ********************************
# Eliminar el año 2012 para continuar con el análisis
data1_long<-subset(data1,AÑO != 2012)
head(data1_long)

############################## AÑO #############################################

T_AÑO=aggregate(data1_long$VALOR, list(data1_long$AÑO), sum,na.rm=TRUE)
# Renombrar columnas 
colnames(T_AÑO) <- c("AÑO", "VALOR_TOTAL")
# Ajustar los límites del eje Y basados en los datos
#ylim_max = max(T_AÑO$VALOR_TOTAL, na.rm = TRUE) * 1.3  # Aumenta un 10% para mejor visualización

#Convertir la columna año en factor para que aparezcan todos en la grafica
T_AÑO$AÑO <- factor(T_AÑO$AÑO, levels = sort(unique(T_AÑO$AÑO)))

ggplot(T_AÑO, aes(x = AÑO, y = VALOR_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  theme_minimal() +
  labs(title = "Número de registros por Año", x = "Año", y = "Registros") +
  scale_y_continuous(breaks = seq(0, max(T_AÑO$VALOR_TOTAL + 500), by = 1000)) +  # Aumentar frecuencia
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),      # Tamaño de texto eje X
    axis.text.y = element_text(size = 12),                             # Tamaño de texto eje Y
    axis.title.x = element_text(size = 13),                            # Etiqueta eje X
    axis.title.y = element_text(size = 13),
 
    axis.line.x = element_line(color = "gray", linewidth = 0.8),
    axis.line.y = element_line(color = "gray", linewidth = 0.8),
    panel.grid.major = element_line(color = "gray75", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )

T_AÑO

# Filtrar el año con mas delitos
data1_AÑO<-subset(data1_long,AÑO==2013)
head(data1_AÑO)


########################### MES #############################################

T_MES=aggregate(data1_long$VALOR, list(data1_long$MES), sum,na.rm=TRUE)
# Renombrar columnas 
colnames(T_MES) <- c("MES", "VALOR_TOTAL")

Meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", 
           "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
# Convertir 'MES' a factor ordenado 
T_MES$MES <- factor(T_MES$MES, levels = Meses, ordered = TRUE)
# Reordenar T_MES
T_MES <- T_MES[order(T_MES$MES), ]

# Ajustar los límites del eje Y basados en los datos
#ylim_max = max(T_MES$VALOR_TOTAL, na.rm = TRUE) * 1.21  # Aumenta un 10% para mejor visualización
# Calcular el porcentaje de cada mes
T_MES_Porcentage <- (T_MES[,2] / sum(T_MES[,2])) * 100
T_MES_Porcentage

# Graficar con ggplot2

#Convertir la columna año en factor para que aparezcan todos en la grafica
#T_AÑO$AÑO <- factor(T_AÑO$AÑO, levels = sort(unique(T_AÑO$AÑO)))

ggplot(T_MES, aes(x = MES, y = VALOR_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  theme_minimal() +
  labs(title = "Número de registros por Mes", x = "Mes", y = "Registros") +
  scale_y_continuous(breaks = seq(0, max(T_MES$VALOR_TOTAL), by = 1000)) +  # Aumentar frecuencia
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),      # Tamaño de texto eje X
    axis.text.y = element_text(size = 12),                             # Tamaño de texto eje Y
    axis.title.x = element_text(size = 13),                            # Etiqueta eje X
    axis.title.y = element_text(size = 13),
    
    axis.line.x = element_line(color = "gray", linewidth = 0.8),
    axis.line.y = element_line(color = "gray", linewidth = 0.8),
    panel.grid.major = element_line(color = "gray75", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )

# Filtrar el año con mas delitos
data1_MES<-subset(data1_long,MES=="MARZO")
head(data1_MES)



########################### MES Y AÑO #############################################

T_AÑO_MES=aggregate(data1_long$VALOR, list(data1_long$AÑO,data1_long$MES), sum,na.rm=TRUE)
T_AÑO_MES

# Renombrar columnas para mejor manejo
colnames(T_AÑO_MES) <- c("AÑO", "MES", "VALOR")

Meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", 
           "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
# Convertir 'MES' a factor ordenado 
T_AÑO_MES$MES <- factor(T_AÑO_MES$MES, levels = Meses, ordered = TRUE)
# Reordenar T_MES
T_AÑO_MES <- T_AÑO_MES[order(T_AÑO_MES$MES), ]

#Convertir la columna año en factor para que aparezcan todos en la grafica
T_AÑO_MES$AÑO <- factor(T_AÑO_MES$AÑO, levels = sort(unique(T_AÑO_MES$AÑO)))

# Crear gráfico con escalas ajustadas
ggplot(data=T_AÑO_MES, aes(x=AÑO, y=VALOR, group=MES, color=MES)) +
  geom_line() +
  geom_point() +
  facet_wrap(~MES, scales="free_y") +  # Escala independiente para cada gráfico
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust=1),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
  labs(title="Evolución mensual de registros por Año",
       x="Año",
       y="Total de registros") 
  
# Renombrar columnas para mejor manejo
#colnames(T_AÑO_MES) <- c("AÑO", "MES", "VALOR_TOTAL")
T_AÑO_MES
# Encontrar la fila con el valor máximo
max_AÑO_MES = T_AÑO_MES[which.max(T_AÑO_MES$VALOR_TOTAL), ]
max_AÑO_MES

# Mostrar el resultado
print(max_AÑO_MES)
# Filtrar el MES y el AÑO con mas delitos
data1_MES_AÑO<-subset(data1_long,MES=="MARZO" & AÑO==2012)
head(data1_MES_AÑO)




# ************************* heatmap ******************************************

#install.packages("pheatmap")
#install.packages("RColorBrewer")

# Cargar librerías
library(pheatmap)
library(RColorBrewer)

# Convertir a formato matriz
matriz_heatmap <- reshape(T_AÑO_MES, idvar = "MES", timevar = "AÑO", direction = "wide")
rownames(matriz_heatmap) <- matriz_heatmap$MES
matriz_heatmap$MES <- NULL  # Eliminar la columna  

# Reemplazar valores NA con 0
matriz_heatmap[is.na(matriz_heatmap)] <- 0

# Definir unicamente los años para el label de x (Limpiar los nombres de las columnas)
colnames(matriz_heatmap) <- gsub(".*\\.", "", colnames(matriz_heatmap))

# Definir una mejor paleta de colores
color_palette <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100)

# Crear el clustermap con mejoras de apariencia
pheatmap(matriz_heatmap, 
         scale = "row",  # Normaliza los valores por fila
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D2",  # Mejor agrupamiento jerárquico
         color = color_palette,  # Colores más atractivos
         fontsize_row = 10,  # Tamaño de texto para entidades
         fontsize_col = 10,  # Tamaño de texto para años
         angle_col = 45,  # Rotar etiquetas de año
         border_color = "grey70",  # Agregar bordes a las celdas
         main = "Clustermap de Delitos por Año y Mes",
         cellwidth = 40, cellheight = 10)  # Ajustar tamaño de celdas

########################### ESTADO CON MAS REGISTROS #############################################

T_ESTADO=aggregate(data1_long$VALOR, list(data1_long$ENTIDAD), sum,na.rm=TRUE)
# Renombrar columnas 
colnames(T_ESTADO) <- c("ENTIDAD", "VALOR_TOTAL")

# Ajustar la escala del eje Y dinámicamente
ylim_max = max(T_ESTADO$VALOR_TOTAL, na.rm = TRUE) * 1.2  # Aumenta un 10% para mejor visualización

# Graficar el barplot con etiquetas ajustadas
barplot(T_ESTADO$VALOR_TOTAL, names.arg = T_ESTADO$ENTIDAD, horiz = FALSE, 
        las = 2, cex.names = 0.8, ylim = c(0, ylim_max), 
        main = "Suma de Valores por Entidad", 
        col = "skyblue", border = "white")


ggplot(T_ESTADO, aes(x = ENTIDAD, y = VALOR_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  theme_minimal() +
  labs(title = "Número de registros por Entidad", x = "Entidad", y = "Registros") +
  scale_y_continuous(breaks = seq(0, max(T_ESTADO$VALOR_TOTAL), by = 1000)) +  # Aumentar frecuencia
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),      # Tamaño de texto eje X
    axis.text.y = element_text(size = 10),                             # Tamaño de texto eje Y
    axis.title.x = element_text(size = 13),                            # Etiqueta eje X
    axis.title.y = element_text(size = 13),
    
    axis.line.x = element_line(color = "gray", linewidth = 0.8),
    axis.line.y = element_line(color = "gray", linewidth = 0.8),
    panel.grid.major = element_line(color = "gray75", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )



T_ESTADO

# Filtrar la ENTIDAD con mas delitos
data1_ESTADO<-subset(data1_long,ENTIDAD=="BAJA CALIFORNIA")
head(data1_ESTADO)

########################### TIPO CON MAS REGISTROS #############################################

T_TIPO=aggregate(data1_long$VALOR, list(data1_long$TIPO), sum,na.rm=TRUE)
# Renombrar columnas 
colnames(T_TIPO) <- c("TIPO", "VALOR_TOTAL")

# Ajustar la escala del eje Y dinámicamente
ylim_max = max(T_TIPO$VALOR_TOTAL, na.rm = TRUE) * 1.2  # Aumenta un 10% para mejor visualización

ggplot(T_TIPO, aes(x = TIPO, y = VALOR_TOTAL)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  theme_minimal() +
  labs(title = "Número de registros por Tipo", x = "Tipo", y = "Registros") +
  scale_y_continuous(breaks = seq(0, max(T_TIPO$VALOR_TOTAL), by = 5000)) +  # Aumentar frecuencia
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),      # Tamaño de texto eje X
    axis.text.y = element_text(size = 10),                             # Tamaño de texto eje Y
    axis.title.x = element_text(size = 13),                            # Etiqueta eje X
    axis.title.y = element_text(size = 13),
    
    axis.line.x = element_line(color = "gray", linewidth = 0.8),
    axis.line.y = element_line(color = "gray", linewidth = 0.8),
    panel.grid.major = element_line(color = "gray75", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
  )



T_TIPO

# Filtrar la ENTIDAD con mas delitos
data1_TIPO<-subset(data1_long,TIPO=="POSESION")
head(data1_TIPO)


########################### AÑO Y ENTIDAD CON UN VALOR MAYOR #############################################

T_AÑO_ENTIDAD=aggregate(data1_long$VALOR, list(data1_long$AÑO,data1_long$ENTIDAD), sum,na.rm=TRUE)

# Renombrar columnas para mejor manejo
colnames(T_AÑO_ENTIDAD) <- c("AÑO", "ENTIDAD", "VALOR_TOTAL")
T_AÑO_ENTIDAD
# Encontrar la fila con el valor máximo
max_AÑO_ENTIDAD = T_AÑO_ENTIDAD[which.max(T_AÑO_ENTIDAD$VALOR_TOTAL), ]

# Mostrar el resultado
print(max_AÑO_ENTIDAD)


# Filtrar la ENTIDAD y el AÑO con mas delitos
data1_ENTIDAD_AÑO<-subset(data1_long,ENTIDAD=="BAJA CALIFORNIA" & AÑO==2013)
head(data1_ENTIDAD_AÑO)


# ******************************************************************************
# Filtrar el año 2013 para todas las ENTIDAD  
# Se filtra el año 2013 para graficar en el mapa cada una de las entidades
T_ENTIDAD_AÑO_2013<-subset(T_AÑO_ENTIDAD, AÑO==2013)
head(T_ENTIDAD_AÑO_2013)

# Filtrar el año 2020 para todas las ENTIDAD
T_ENTIDAD_AÑO_2020<-subset(T_AÑO_ENTIDAD, AÑO==2020)
head(T_ENTIDAD_AÑO_2020)

# Filtrar el año 2024 para todas las ENTIDAD
T_ENTIDAD_AÑO_2024<-subset(T_AÑO_ENTIDAD, AÑO==2024)
head(T_ENTIDAD_AÑO_2024)



# ******************************************************************************

# Entidad con mayor registro para cada año

Max_por_Año <- T_AÑO_ENTIDAD %>%
  group_by(AÑO) %>%
  slice_max(order_by = VALOR_TOTAL, n = 1, with_ties = FALSE) %>%
  ungroup()

# Mostrar resultado
print(Max_por_Año)


# ************************ heatmap ***********************************


# Agrupar datos por AÑO y ENTIDAD
#T_AÑO_ENTIDAD <- aggregate(VALOR ~ AÑO + ENTIDAD, data = data1_long, sum, na.rm = TRUE)

# Convertir a formato matriz
matriz_heatmap_AE <- reshape(T_AÑO_ENTIDAD, idvar = "ENTIDAD", timevar = "AÑO", direction = "wide")
rownames(matriz_heatmap_AE) <- matriz_heatmap_AE$ENTIDAD
matriz_heatmap_AE$ENTIDAD <- NULL  # Eliminar la columna de nombres

# Reemplazar valores NA con 0
matriz_heatmap_AE[is.na(matriz_heatmap_AE)] <- 0

# Definir unicamente los años para el label de x (Limpiar los nombres de las columnas)
colnames(matriz_heatmap_AE) <- gsub(".*\\.", "", colnames(matriz_heatmap_AE))

# Definir una mejor paleta de colores
color_palette <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100)

# Crear el clustermap con mejoras de apariencia
pheatmap(matriz_heatmap_AE, 
         scale = "row",  # Normaliza los valores por fila
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D2",  # Mejor agrupamiento jerárquico
         color = color_palette,  # Colores más atractivos
         fontsize_row = 10,  # Tamaño de texto para entidades
         fontsize_col = 10,  # Tamaño de texto para años
         angle_col = 45,  # Rotar etiquetas de año
         border_color = "grey70",  # Agregar bordes a las celdas
         main = "Clustermap de Delitos por Año y Entidad",
         cellwidth = 40, cellheight = 10)  # Ajustar tamaño de celdas



########################### ENTIDAD Y TIPO CON UN VALOR MAYOR #############################################

T_ENTIDAD_TIPO=aggregate(data1_long$VALOR, list(data1_long$ENTIDAD,data1_long$TIPO), sum,na.rm=TRUE)

# Renombrar columnas para mejor manejo
colnames(T_ENTIDAD_TIPO) <- c("ENTIDAD", "TIPO", "VALOR_TOTAL")
T_ENTIDAD_TIPO
# Encontrar la fila con el valor máximo
max_ENTIDAD_TIPO = T_ENTIDAD_TIPO[which.max(T_ENTIDAD_TIPO$VALOR_TOTAL), ]

# Mostrar el resultado
print(max_ENTIDAD_TIPO)


# Filtrar la ENTIDAD y el TIPO con mas delitos
data1_ENTIDAD_TIPO<-subset(data1_long,ENTIDAD == "BAJA CALIFORNIA" & TIPO == "POSESION")
head(data1_ENTIDAD_TIPO)


########################### ENTIDAD, AÑO Y MES CON UN VALOR MAYOR #############################################

T_ENTIDAD_AÑO_MES=aggregate(data1_long$VALOR, list(data1_long$ENTIDAD,data1_long$AÑO, data1_long$MES), sum,na.rm=TRUE)

# Renombrar columnas para mejor manejo
colnames(T_ENTIDAD_AÑO_MES) <- c("ENTIDAD", "AÑO", "MES", "VALOR_TOTAL")
T_ENTIDAD_AÑO_MES
# Encontrar la fila con el valor máximo
max_ENTIDAD_AÑO_MES = T_ENTIDAD_AÑO_MES[which.max(T_ENTIDAD_AÑO_MES$VALOR_TOTAL), ]

# Mostrar el resultado
print(max_ENTIDAD_AÑO_MES)


# Filtrar la ENTIDAD y el TIPO con mas delitos
data1_ENTIDAD_AÑO_MES<-subset(data1_long,ENTIDAD == "BAJA CALIFORNIA" & AÑO == 2012 & MES == "MARZO")
head(data1_ENTIDAD_AÑO_MES)


########################### ENTIDAD, AÑO, MES  TIPO  CON UN VALOR MAYOR #############################################

T_ENTIDAD_AÑO_MES_TIPO=aggregate(data1_long$VALOR, list(data1_long$ENTIDAD,data1_long$AÑO, data1_long$MES, data1_long$TIPO), sum,na.rm=TRUE)

# Renombrar columnas para mejor manejo
colnames(T_ENTIDAD_AÑO_MES_TIPO) <- c("ENTIDAD", "AÑO", "MES", "TIPO", "VALOR_TOTAL")
T_ENTIDAD_AÑO_MES_TIPO
# Encontrar la fila con el valor máximo
max_ENTIDAD_AÑO_MES_TIPO = T_ENTIDAD_AÑO_MES_TIPO[which.max(T_ENTIDAD_AÑO_MES_TIPO$VALOR_TOTAL), ]

# Mostrar el resultado
print(max_ENTIDAD_AÑO_MES_TIPO)


# Filtrar la ENTIDAD y el TIPO con mas delitos
data1_ENTIDAD_AÑO_MES<-subset(data1_long,ENTIDAD == "BAJA CALIFORNIA" & AÑO == 2012 & MES == "MARZO")
head(data1_ENTIDAD_AÑO_MES)



############################## ENTIDAD Y MES CON EL VALOR MAYOR ##########################
T_ENTIDAD_MES=aggregate(data1_long$VALOR, list(data1_long$ENTIDAD, data1_long$MES), sum,na.rm=TRUE)

# Renombrar columnas para mejor manejo
colnames(T_ENTIDAD_MES) <- c("ENTIDAD", "MES", "VALOR_TOTAL")
T_ENTIDAD_MES
# Encontrar la fila con el valor máximo
max_ENTIDAD_MES = T_ENTIDAD_MES[which.max(T_ENTIDAD_MES$VALOR_TOTAL), ]

# Mostrar el resultado
print(max_ENTIDAD_MES)

# Filtrar la ENTIDAD y el MES con mas delitos
data1_ENTIDAD_MES<-subset(data1_long,ENTIDAD == "BAJA CALIFORNIA" & MES == "MAYO")
head(data1_ENTIDAD_MES)

# ****************************  heatmap ****************************************

# Convertir a formato matriz
matriz_heatmap <- reshape(T_ENTIDAD_MES, idvar = "ENTIDAD", timevar = "MES", direction = "wide")
rownames(matriz_heatmap) <- matriz_heatmap$ENTIDAD
matriz_heatmap$ENTIDAD <- NULL  # Eliminar la columna  

# Reemplazar valores NA con 0
matriz_heatmap[is.na(matriz_heatmap)] <- 0

# Definir unicamente los años para el label de x (Limpiar los nombres de las columnas)
colnames(matriz_heatmap) <- gsub(".*\\.", "", colnames(matriz_heatmap))

# Definir una mejor paleta de colores
color_palette <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100)

# Crear el clustermap con mejoras de apariencia
pheatmap(matriz_heatmap, 
         scale = "row",  # Normaliza los valores por fila
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D2",  # Mejor agrupamiento jerárquico
         color = color_palette,  # Colores más atractivos
         fontsize_row = 10,  # Tamaño de texto para entidades
         fontsize_col = 10,  # Tamaño de texto para años
         angle_col = 45,  # Rotar etiquetas de año
         border_color = "grey70",  # Agregar bordes a las celdas
         main = "Clustermap de Delitos por Mes y Entidad",
         cellwidth = 40, cellheight = 10)  # Ajustar tamaño de celdas


#################################### MAPAS ############################################
#######################################################################################

#### INSTALACION DE LOS PAQUETES 

#if (!require("devtools")) {
#  install.packages("devtools")
#}
#devtools::install_github("diegovalle/mxmaps")

library("mxmaps")

# CLave INEGI  para las 32 entidades
clave_INEGI = 1:32
# Rellenar las unidades con un cero a la izquierda 
clave_INEGI <- sprintf("%02d", clave_INEGI)

codigo_INEGI <- data.frame(
  ENTIDAD = c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE", "COAHUILA",
              "COLIMA", "CHIAPAS", "CHIHUAHUA", "CIUDAD DE MEXICO", "DURANGO", "GUANAJUATO",
              "GUERRERO", "HIDALGO", "JALISCO", "MEXICO", "MICHOACAN", "MORELOS", "NAYARIT",
              "NUEVO LEON", "OAXACA", "PUEBLA", "QUERETARO", "QUINTANA ROO", "SAN LUIS POTOSI",
              "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA", "VERACRUZ", "YUCATAN", "ZACATECAS"),
  clave_INEGI )

codigo_INEGI

################################################################################
# ***************** Mapa - Delitos por Entidad *********************************

T_ESTADO_MAP <- T_ESTADO
T_ESTADO_MAP <- T_ESTADO_MAP[-11, ] # Eliminar la fila de Extranjero
rownames(T_ESTADO_MAP) <- NULL

# Unir el codigo INEGI con el dataset de las entidades
df_mxstate_2024 <- merge(T_ESTADO_MAP, codigo_INEGI, by = "ENTIDAD", all.x = TRUE)

# Mostrar los resultado
head(df_mxstate_2024)
df_mxstate_2024

# DEfinir el titulo adecuado para la clave y el valor
colnames(df_mxstate_2024)[colnames(df_mxstate_2024) == "clave_INEGI"] <- "region"
colnames(df_mxstate_2024)[colnames(df_mxstate_2024) == "VALOR_TOTAL"] <- "value"

df_mxstate_2024

# Se define el mapa con su titulo y su legend
mapa <- mxstate_choropleth(df_mxstate_2024, 
                   num_colors = 1,
                   title = "Número de delitos para cada Entidad",
                   legend = "Num")

# Longitud y latitud de cada entidad  
centros_estado <- mxstate.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  left_join(data.frame(region = as.character(df_mxstate_2024$region),
                       state_name = df_mxstate_2024$ENTIDAD),
            by = "region")

#install.packages("ggrepel")

# Unir el mapa con las etiquetas de las entidades
library(ggrepel)
mapa_entidades <- mapa +
  geom_text_repel(data = centros_estado,
                  aes(x = long, y = lat, label = state_name),
                  size = 2.25,
                  color = "black",
                  inherit.aes = FALSE,
                  max.overlaps = 100) +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar el titulo

mapa_entidades
# Guardar el mapa en la carpeta específica
#ggsave("C:/Users/Milton/OneDrive/Documentos/MASTER/TFM/SCRIPT/mapas/1-mapa_entidades.png", plot = mapa_entidades, width = 10, height = 6, dpi = 300)



################################################################################
# ***************** Mapa - Delitos por Año 2013 *********************************

T_ENTIDAD_AÑO_2013_MAP <- T_ENTIDAD_AÑO_2013
T_ENTIDAD_AÑO_2013_MAP <- T_ENTIDAD_AÑO_2013_MAP[-11, ] # Eliminar la fila de Extranjero
rownames(T_ENTIDAD_AÑO_2013_MAP) <- NULL

# Unir el codigo INEGI con el dataset de las entidades
df_mxstate_ENT_AÑO_2013 <- merge(T_ENTIDAD_AÑO_2013_MAP, codigo_INEGI, by = "ENTIDAD", all.x = TRUE)

# Mostrar los resultado
head(df_mxstate_ENT_AÑO_2013)
#df_mxstate_ENT_AÑO_2013

# DEfinir el titulo adecuado para la clave y el valor
colnames(df_mxstate_ENT_AÑO_2013)[colnames(df_mxstate_ENT_AÑO_2013) == "clave_INEGI"] <- "region"
colnames(df_mxstate_ENT_AÑO_2013)[colnames(df_mxstate_ENT_AÑO_2013) == "VALOR_TOTAL"] <- "value"

df_mxstate_ENT_AÑO_2013

# Se define el mapa con su titulo y su legend
mapa_2013 <- mxstate_choropleth(df_mxstate_ENT_AÑO_2013, 
                           num_colors = 1,
                           title = "Número de delitos para cada Entidad - Año 2013",
                           legend = "Num")

# Longitud y latitud de cada entidad  
centros_estado_2013 <- mxstate.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  left_join(data.frame(region = as.character(df_mxstate_ENT_AÑO_2013$region),
                       state_name = df_mxstate_ENT_AÑO_2013$ENTIDAD),
            by = "region")

#install.packages("ggrepel")

# Unir el mapa con las etiquetas de las entidades
library(ggrepel)
mapa_entidades_2013 <- mapa_2013 +
  geom_text_repel(data = centros_estado_2013,
                  aes(x = long, y = lat, label = state_name),
                  size = 2.25,
                  color = "black",
                  inherit.aes = FALSE,
                  max.overlaps = 100) +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar el titulo

mapa_entidades_2013
# Guardar el mapa en la carpeta específica
# ggsave("C:/Users/Milton/OneDrive/Documentos/MASTER/TFM/SCRIPT/mapas/1-mapa_entidades_2013.png", plot = mapa_entidades_2013, width = 10, height = 6, dpi = 300)


################################################################################
# ***************** Mapa - Delitos por Año 2020 *********************************

T_ENTIDAD_AÑO_2020_MAP <- T_ENTIDAD_AÑO_2020
T_ENTIDAD_AÑO_2020_MAP <- T_ENTIDAD_AÑO_2020_MAP[-11, ] # Eliminar la fila de Extranjero
rownames(T_ENTIDAD_AÑO_2020_MAP) <- NULL

# Unir el codigo INEGI con el dataset de las entidades
df_mxstate_ENT_AÑO_2020 <- merge(T_ENTIDAD_AÑO_2020_MAP, codigo_INEGI, by = "ENTIDAD", all.x = TRUE)

# Mostrar los resultado
head(df_mxstate_ENT_AÑO_2020)
#df_mxstate_ENT_AÑO_2013

# DEfinir el titulo adecuado para la clave y el valor
colnames(df_mxstate_ENT_AÑO_2020)[colnames(df_mxstate_ENT_AÑO_2020) == "clave_INEGI"] <- "region"
colnames(df_mxstate_ENT_AÑO_2020)[colnames(df_mxstate_ENT_AÑO_2020) == "VALOR_TOTAL"] <- "value"

df_mxstate_ENT_AÑO_2020

# Se define el mapa con su titulo y su legend
mapa_2020 <- mxstate_choropleth(df_mxstate_ENT_AÑO_2020, 
                                num_colors = 1,
                                title = "Número de delitos para cada Entidad - Año 2020",
                                legend = "Num")

# Longitud y latitud de cada entidad  
centros_estado_2020 <- mxstate.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  left_join(data.frame(region = as.character(df_mxstate_ENT_AÑO_2020$region),
                       state_name = df_mxstate_ENT_AÑO_2020$ENTIDAD),
            by = "region")

#install.packages("ggrepel")

# Unir el mapa con las etiquetas de las entidades
library(ggrepel)
mapa_entidades_2020 <- mapa_2020 +
  geom_text_repel(data = centros_estado_2020,
                  aes(x = long, y = lat, label = state_name),
                  size = 2.25,
                  color = "black",
                  inherit.aes = FALSE,
                  max.overlaps = 100) +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar el titulo

mapa_entidades_2020
# Guardar el mapa en la carpeta específica
#ggsave("C:/Users/Milton/OneDrive/Documentos/MASTER/TFM/SCRIPT/mapas/1-mapa_entidades_2020.png", plot = mapa_entidades_2020, width = 10, height = 6, dpi = 300)



# ***************** Mapa - Delitos por Año 2024 *********************************

T_ENTIDAD_AÑO_2024_MAP <- T_ENTIDAD_AÑO_2024
T_ENTIDAD_AÑO_2024_MAP <- T_ENTIDAD_AÑO_2024_MAP[-11, ] # Eliminar la fila de Extranjero
rownames(T_ENTIDAD_AÑO_2024_MAP) <- NULL

# Unir el codigo INEGI con el dataset de las entidades
df_mxstate_ENT_AÑO_2024 <- merge(T_ENTIDAD_AÑO_2024_MAP, codigo_INEGI, by = "ENTIDAD", all.x = TRUE)

# Mostrar los resultado
head(df_mxstate_ENT_AÑO_2024)
#df_mxstate_ENT_AÑO_2013

# DEfinir el titulo adecuado para la clave y el valor
colnames(df_mxstate_ENT_AÑO_2024)[colnames(df_mxstate_ENT_AÑO_2024) == "clave_INEGI"] <- "region"
colnames(df_mxstate_ENT_AÑO_2024)[colnames(df_mxstate_ENT_AÑO_2024) == "VALOR_TOTAL"] <- "value"

df_mxstate_ENT_AÑO_2024

# Se define el mapa con su titulo y su legend
mapa_2024 <- mxstate_choropleth(df_mxstate_ENT_AÑO_2024,
                                num_colors = 1,
                                title = "Número de delitos para cada Entidad - Año 2024",
                                legend = "Num")

# Longitud y latitud de cada entidad  
centros_estado_2024 <- mxstate.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  left_join(data.frame(region = as.character(df_mxstate_ENT_AÑO_2024$region),
                       state_name = df_mxstate_ENT_AÑO_2024$ENTIDAD),
            by = "region")

#install.packages("ggrepel")

# Unir el mapa con las etiquetas de las entidades
library(ggrepel)
mapa_entidades_2024 <- mapa_2024 +
  geom_text_repel(data = centros_estado_2024,
                  aes(x = long, y = lat, label = state_name),
                  size = 2.25,
                  color = "black",
                  inherit.aes = FALSE,
                  max.overlaps = 100) +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar el titulo

mapa_entidades_2024
# Guardar el mapa en la carpeta específica
#ggsave("C:/Users/Milton/OneDrive/Documentos/MASTER/TFM/SCRIPT/mapas/1-mapa_entidades_2024.png", plot = mapa_entidades_2024, width = 10, height = 6, dpi = 300)



# Crear el clustermap con mejoras de apariencia
pheatmap(matriz_heatmap, 
         scale = "row",  # Normaliza los valores por fila
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "ward.D2",  # Mejor agrupamiento jerárquico
         color = color_palette,  # Colores más atractivos
         fontsize_row = 10,  # Tamaño de texto para entidades
         fontsize_col = 10,  # Tamaño de texto para años
         angle_col = 45,  # Rotar etiquetas de año
         border_color = "grey70",  # Agregar bordes a las celdas
         main = "Clustermap de Delitos por Año y Entidad",
         cellwidth = 40, cellheight = 10)  # Ajustar tamaño de celdas



# ***************** Mapa - Presencia o ausencia de delitos (Entidades por año) *********************************

# Se toman los datos del clustering usado para graficar el Heatmap para el Año y la ENtidad (matriz_heatmap_AE)
pheatmap_result <- pheatmap(matriz_heatmap_AE, 
                            scale = "row",
                            clustering_distance_rows = "euclidean",
                            clustering_distance_cols = "euclidean",
                            clustering_method = "ward.D2",
                            color = colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100),
                            fontsize_row = 10,
                            fontsize_col = 10,
                            angle_col = 45,
                            border_color = "grey70",
                            main = "Clustermap de Delitos por Año y Entidad",
                            cellwidth = 40, cellheight = 10)

# Obtener unicamente las entidades del primer cluster 
cluster_assignments <- cutree(pheatmap_result$tree_row, k = 2)
entidades_cluster2 <- names(cluster_assignments[cluster_assignments == 1])

# Preparar el shape para graficar
mxstate_mapa_datos <- mxstate.map %>%
  left_join(df_mxstate_2024, by = "region")

# Asignar el color para cada cluster
mxstate_mapa_datos$color_cluster <- "azul"  # todos azul por defecto
mxstate_mapa_datos$color_cluster[mxstate_mapa_datos$ENTIDAD %in% entidades_cluster2] <- "gris"  # solo clúster 2 en gris

# Definir colores invertidos
colores_mapa <- c(
  "gris" = "lightgray",  #
  "azul" = "steelblue"  # primer cluster
)

# Crear el mapa
mapa_cluster_1 <- ggplot() +
  geom_polygon(data = mxstate_mapa_datos,
               aes(x = long, y = lat, group = group, fill = color_cluster),
               color = "black", size = 0.3) +
  scale_fill_manual(values = colores_mapa,                     
                    labels = c("Presencia", "Ausencia"),  # Cambiar etiquetas de la leyenda
                    name = "Delitos") +
  theme_void() +
  labs(title = "Mapa de presencia y ausencia de delitos") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.5),  # Mover la leyenda ligeramente hacia la izquierda
        legend.justification = c(0, 0.5))  # Ajustar la justificación de la leyenda


# Añadir las etiquetas
# Longitud y latitud promedio para las etiquetas
centros_estado_2024 <- mxstate.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  left_join(data.frame(region = as.character(df_mxstate_2024$region),
                       state_name = df_mxstate_2024$ENTIDAD),
            by = "region")

# Añadir etiquetas sobre el mapa
mapa_entidades_con_sin_del <- mapa_cluster_1 +
  geom_text_repel(data = centros_estado_2024,
                  aes(x = long, y = lat, label = state_name),
                  size = 2.25,
                  color = "black",
                  inherit.aes = FALSE,
                  max.overlaps = 100) +
  theme(plot.title = element_text(hjust = 0.5))

mapa_entidades_con_sin_del # Mapa con y sin delitos



 