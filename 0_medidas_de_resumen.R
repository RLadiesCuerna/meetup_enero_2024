#FUENTE:
#https://rpkgs.datanovia.com/rstatix/

library(dplyr)

#UN BREVE REPASO DE DPLYR

#Con dplyr podemos usar el símbolo %>% (del paquete magrittr).
#Este símbolo se llama "pipe" en inglés y que se traduce como "tubería" en español
#Podemos leer al pipe como un "Y luego". Por ejemplo:

#Lo podemos re-escribir como

#Toma a iris y luego visualizalo
iris %>% summary
summary(iris)

#Toma 100 y luego obten la raiz cuadrada y luego obten el logaritmo
100 %>% sqrt %>% log
log(sqrt(100))

df<-as_tibble(iris)

#Además dplyr incluye funciones (o verbos) muy útiles para explorar nuestros
#Conjuntos de datos: select, filter, group_by, summarize

#select: para seleccionar columnas

## Por número de columna
df %>% select(1,2)

## Por nombre de columna
df %>% select(Species, Sepal.Width)

## Por tipo de variables, por ejemplo numérico
df %>% select(where(is.numeric))

## Según cómo inicia el nombre de columna
df %>% select(starts_with("Sepal"))

#filter: para seleccionar filas

df %>% filter(Species=="versicolor")
df %>% filter(Sepal.Length < 5.5 & Species=="virginica")
df %>% filter(Species %in% c("virginica","versicolor"))
df %>% filter(grepl("v",Species))

#summarize: para obtener resumenes de nuestros datos (y group_by para hacerlo grupo por grupo)

df %>% summarise( mean = mean(Sepal.Length), n = n())

df %>%
  group_by(Species) %>%
  summarise( mean = mean(Sepal.Length),
             sd= sd(Sepal.Length),
             n = n() )



#Estadísticas fáciles con RSTATIX

library(rstatix)
#Obtención de estadísicas de resumen para datos cuantitativos

#rstatix automáticamente identifica a las columnas con datos numéricos y obtiene
#estadísticas de resumen

df %>% get_summary_stats

#Podemos obtener distintos grupos de estadísticas

df %>% get_summary_stats(type="common")

df %>% get_summary_stats(type="mean_sd")

#Y podemos obtener estadísticas por grupo

iris %>%
  group_by(Species) %>%
  get_summary_stats(type = "mean_sd")

#Y hacer selecciones de filas o columnas

iris %>%
  group_by(Species) %>%
  get_summary_stats(type = "mean_sd") %>%
  filter(Species %in% c("setosa","virginica")) %>%
  filter(grepl("Sepal",variable))

iris %>%
  select(Species, starts_with("Sepal")) %>%
  filter(Species %in% c("virginica","versicolor")) %>%
  group_by(Species) %>%
  get_summary_stats()

