##este es un proyecto donde se da a conocer la cantidad de divorcios en Mexico 
## y en el estado de aguascalientes, llevando a cabo un procesos de importacion
## de datos  filtracion y lipieza de datos y asi poder generar visalizaciones 
## donde facilite el entendimiento  de los mismos datos.

##paquetes
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")

library(tidyr)
library(ggplot2)
library(tidyverse)
library(dplyr)

##importar datos 
divorcios_estatal <-read.csv("C:/PROYECTO_DIVORCIOS_RSTUDIO/DIVORCIOS_ESTATAL.csv")
divorcios_mex <-read.csv("C:/PROYECTO_DIVORCIOS_RSTUDIO/DIVORCIOS_MEX.csv")

## observar datos 
head(divorcios_estatal)
head(divorcios_mex)

colnames(divorcios_estatal)
colnames(divorcios_mex)

str(divorcios_estatal)
str(divorcios_mex)

##limpieza u  organizacion de datos 

##filtrar las columans requeridas y guardar en una variable del archivo divorcios_estatal 
divorcios_ags <- divorcios_estatal %>% 
  select(desc_municipio, X2021, X2022)
print(divorcios_ags) 

##filtrar las columans requeridas del archivo divorcios_mex
##se filtra que la columna cve_municipio que contenga 0 traega los datos de esa fila 
##pero solo lo que contine las columnas desc_entidad y el año x2022
##trae el total de divorcios y el estado 

total_div_municipios <- divorcios_mex %>%
  filter(cve_municipio == 0) %>%
  slice(4:n()) %>%
  select(X2022, desc_entidad)

print(total_div_municipios)

##verificando el contenido del df filtrado
names(total_div_municipios)
str(total_div_municipios)

# Crear el gráfico de barras de mexico 
divorcios_mex_barras <- ggplot(total_div_municipios, aes(x = desc_entidad, y = X2022, fill = X2022)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs( x= "Estados", y = "Divorcios", title = "Divorcios por Estado en Mexico en  2022",caption="Datos recopilados del INEGI ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje x
        axis.text = element_text(face = "bold")) +  # Texto en negrita
  scale_fill_gradient(low = "#FFA07A", high = "#87CEEB", name = "Divorcios") +  # Gradiente de color para los valores de los divorcios
  guides(fill = guide_colorbar(title = "Divorcios"))  # Título de la leyenda de colores


print(divorcios_mex_barras)


# Crear el gráfico de barras con ggplot2

# Reorganizar los datos en formato largo
divorcios_ags_long <- pivot_longer(divorcios_ags, cols = c(X2021, X2022), names_to = "Año", values_to = "Divorcios")


divorcios_barras <- ggplot(divorcios_ags_long, aes(x = desc_municipio, y = Divorcios, fill = Año)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Municipio", y = "Divorcios", title = "Divorcios por municipio en Aguascalientes en los años 2021 y 2022"
       ,caption="Datos recopilados del INEGI "   ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje x
        axis.text = element_text(face = "bold")) +  # Texto en negrita
  scale_fill_manual(values = c("#FFA07A", "#87CEEB")) +  # Colores personalizados para los años
  guides(fill = guide_legend(title = "Año"))  # Título de la leyenda de colores

print(divorcios_barras)








