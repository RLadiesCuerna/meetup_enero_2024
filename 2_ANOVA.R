library(dplyr)
library(rxtatix)
library(ggppubr)

# PRUEBA ESTADÍSTICA ANOVA PARA COMPARAR TRES O MÁS GRUPOS

#.  Set de datos Toothgrowth: Crecimiento de dientes
#    7oo¨D ____                                 ^ ^
#   / ´  |     ``\      +  Vitamina C    ...   | U |
#   \ T  /      ´ |`                          /  `  \ Crecimiento???
#     ¨U-U¨¨---d_/                            \_.´\_/
#.  Metodo de administración (supp): Jugo de naranja (OJ) o Acido Ascórbico (VC)
#   Dosis (dose): 0.5, 1 y 2 mg

#   Haremos unas breves ediciones de nuestros datos

tg <- ToothGrowth

View(tg)

tg$dose <- paste("D",tg$dose)
tg$dose <- as.factor(tg$dose)

View(tg)


##########################
# ANOVA DE UNA VIA
##########################

#   Queremos estudiar las diferencias entre distintas dosis

#   Comenzaremos por graficar los datos

#   Un pánel por suplemento

box_facet <- tg %>% ggboxplot(x = "dose", y = "len", facet.by = "supp", add="jitter")

box_all <- tg %>% ggboxplot( x = "dose", y = "len", color = "supp", add="jitter")


#   Al igual que el test t el test ANOVA asume que los datos
#   Tienen una distribución normal y que las varianzas entre los
#   grupos de datos son homogéneas (ver la comprobación de
#   estas suposiciones en la sección anterior)

#   Puuesto que ya sabemos que estos supuestos se cumplen procederemos
#   con el test ANOVA

# rstatix
tg %>% anova_test(len~dose)

# Comparemos con el formato de salida usando rbase
oneway.test(len~dose, tg)

# Ahora utilizamos a rstatix para hacer el test para cada suplemento por separado
tg %>% group_by(supp) %>% anova_test(len~dose)

# En el resultado anterior "ges" es el "generalized eta squared", que muestra
# el tamaño del efecto para el ANOVA. Indica qué parte de la la varianza
# es explicada por el efecto de la dosis

# También tenemos una opción si las varianzas entre grupos de datos no son homogeneas

tg %>% group_by(supp) %>% welch_anova_test(len~dose)

# ggpubr nos da una función muy sencilla para añadir los resultados del ANOVA
# a nuestro boxplot. stat_compare_means utiliza el ANOVA con corrección de Welch

box_facet + stat_compare_means(method="anova")

# Si quisiéramos un ANOVA sin corrección
# tg %>% ggboxplot(., x = "dose", y = "len", facet.by = "supp") + stat_compare_means(method="anova", method.args = list(var.equal = TRUE))

##########################
# ANOVA: TESTS POST-HOC
##########################

# El test de Tukey se usa una vez que se encuentra una diferencia significativa
# en el ANOVA para buscar el par o pares de grupos entre los que existe
# la diferencia significativa

tg %>% tukey_hsd(len~dose)
test_tu<-tg %>% group_by(supp) %>% tukey_hsd(len~dose)

# Creamos un boxplot con los resultados del test post-hoc de Tukey

# Primero separando en diferentes páneles los resultados para OJ y VC

test_tu<-test_tu %>% add_xy_position(x="dose")

box_facet + scale_y_continuous(limits=c(0,50)) +
  stat_pvalue_manual(test_tu, hide.ns = TRUE) + stat_compare_means(method="anova")

# Ahora usando un sólo panel para todos los grupos

tg %>% group_by(supp) %>% anova_test(len~dose)

box_all +
  stat_pvalue_manual(test_tu, color="supp", step.increase = 0.05,
                     step.group.by = "supp", hide.ns = TRUE) +
  annotate(geom="text",x=2, y=52, label="ANOVA  OJ p=8.89e-8  VC p=3.36e-11")


##########################
# ALTERNATIVAS NO PARAMÉTRICAS AL ANOVA
##########################

#   Cuando la nuestros datos no tienen una distribución normal necesitamos
#   utilizar un test no paramétrico. El test de de Kurskal Wallis es la alternativa
#   no paramétrica al ANOVA

tg %>% group_by(supp) %>% kruskal_test(len~dose)

# Para evaluar el tamaño del efecto usamos la "eta squared" (estadístico H).
# Es la fracción de la varianza que es explicada por el efecto de la dosis

tg %>% group_by(supp) %>% kruskal_effsize(len~dose)

#   Como test post-hoc para el test de Kruskal Wallis usamos el test de Dunn

test_du <- tg %>% group_by(supp) %>% dunn_test(len~dose)

test_du<-test_du %>% add_xy_position(x="dose")

box_facet + scale_y_continuous(limits=c(0,50)) +
  stat_pvalue_manual(test_du, hide.ns = TRUE) + stat_compare_means(method="kruskal")

box_all +
  stat_pvalue_manual(test_du, color="supp", step.increase = 0.05,
                     step.group.by = "supp", hide.ns = TRUE) +
  annotate(geom="text",x=2, y=52, label="Kurskal-Wallis  OJ p=0.00009  VC p=0.000003")


##########################
# ANOVA DE DOS VÍAS
##########################
#      El ANOVA de dos vias evalua simultaneamente
#      el efecto de dos variables categoricas (ej. tratamientos, grupos)
#      sobre el comportamiento de los datos numéricos

#      Al haber distintas variables se evaluan varios pares de
#      hipótesis nula y alternativa.

#  En el siguiente cálculo evaluamos la influencia de supp y dose
#  en la longitud (len) con un modelo aditivo. Estamos asumiendo
#  que las variables supp y dose son independientes entre si:

tg %>% anova_test(len~supp + dose)

tg %>% tukey_hsd(len~supp + dose)

#  El p value para la interacción es significativo. Esto es por que:
#  el suplemento de vitamina C que se usa (supp) cambia el tamaño
#  promedio de los odontoblastos, no obstante, la diferencia entre
#  los dos suplementos es dependiente de la dosis. Cuando la dosis
#  es D2 (alta) se vuelve indetectable.

tg %>% anova_test(len~supp * dose)

tg %>% anova_test(len~supp + dose + supp:dose)

tg %>% tukey_hsd(len~supp * dose)
