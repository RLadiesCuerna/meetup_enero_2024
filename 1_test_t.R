#FUENTES:
#https://rpkgs.datanovia.com/rstatix/
#https://rpkgs.datanovia.com/ggpubr/
#https://www.datanovia.com/en/lessons/anova-in-r
#https://www.datanovia.com/en/blog/how-to-add-p-values-onto-a-grouped-ggplot-using-the-ggpubr-r-package/
#https://www.researchgate.net/post/What-is-the-difference-between-Tukeys-Post-Hoc-Test-and-Students-t-test


library(dplyr)
library(rxtatix)
library(ggpubr)

# PRUEBA ESTADÍSTICA T PARA COMPARAR DOS GRUPOS

#.  Set de datos Toothgrowth: Crecimiento de dientes
#    7oo¨D ____                                 ^ ^
#   / ´  |     ``\      +  Vitamina C    ...   | U |
#   \ T  /      ´ |`                          /  `  \ Crecimiento???
#     ¨U-U¨¨---d_/                            \_.´\_/
#.  Metodo de administración (supp): Jugo de naranja (OJ) o Acido Ascórbico (VC)
#   Dosis (dose): 0.5, 1 y 2 mg

##########################
# OBSERVACIÓN DE LOS DATOS
##########################

#   Haremos unas breves ediciones de nuestros datos
tg <- ToothGrowth

View(tg)

tg$dose <- paste("D",tg$dose)
tg$dose <- as.factor(tg$dose)

View(tg)

#Histogramas con ggpubr
tg %>% ggdensity(., x = "len", color = "supp", fill="supp",
                 add="mean", facet.by = "dose")

#Boxplots con ggpubr
tg %>% ggboxplot(., x = "dose", y = "len", color="supp",
                 add="jitter", facet.by = "dose", palette="Set2", scales="free_x")

#Estadísticas de resumen con rstatix
tg %>% group_by(supp,dose) %>% get_summary_stats(type="mean_sd")


##########################
# SUPUESTOS DE LA PRUEBA T
##########################

#   La prueba t asume que la distribución de los la los datos es normal y que
#   La varianza entre los grupos es homogénea. Haremos algunas
#   Pruebas para estudiar si los supuestos de la prueba se cumplen

#   Correlación entre los cuantiles de nuestros datos y los cuantiles
#   de una distribución normal mediante Quantile-Quantile (Q-Q) plots

ggqqplot(tg, x="len")
ggqqplot(tg, x="len", facet.by = c("supp","dose"))

#   Cómo se verían datos con distribución Poisson
usage<-data.frame(users=as.numeric(WWWusage))
ggqqplot(usage,x="users")

#   También evaluaremos normalidad con el test de Shapiro-Wilk
#   Este test es sensible a outliers. Buscaremos outliers utilizando
#   a rstatix

tg %>% group_by(supp,dose) %>% identify_outliers(len)

#   Si queremos eliminar los outliers
# tg_outliers<-tg %>% group_by(supp,dose) %>% identify_outliers(len)
# anti_join(tg, tg_outliers, by=c("supp","dose","len"))

#   Ahora realizamos el test the Shapiro Wilk. En este test la hipótesis
#   nula es que la distribución de los datos es normal. Esperamos observar
#   una p no significativa.

tg %>% group_by(supp,dose) %>% shapiro_test(len)

#   Así se verían los resultados para datos no normales
usage %>% shapiro_test(users)

#   Para evaluar la homogeneidad de varianzas usaremos el test de Levene.
#   La hipotesis nula (H0) de este test es que las varianzas son homogeneas.
#   Esperamos un valor p no significativo

#¿Son homogeneas las varianzas para diferentes dosis (del mismo suplemento)?
tg %>%
  group_by(supp) %>%
  levene_test(len ~ dose)

#¿Son homogeneas las varianzas para suplementos (con la misma dosis)?
tg %>%
  group_by(dose) %>%
  levene_test(len ~ supp)

################
# TEST T
################

#   Compararemos la longitud de los odontoblastos para cada suplemento (OJ o VC)
#   Veamos cómo se verían los resultados utilizando a R base

tg %>% t.test(len~supp, data=.)

#   Y ahora con rstatix

tg %>% t_test(len ~ supp)

#   Y repetimos la prueba para cada dosis por separado

tg %>% group_by(dose) %>% t_test(len ~ supp)


#   Boxplot de los resultados con valores p (ggpubr)

#   Si queremos observar la diferencia entre suplementos dosis por dosis
box1<-ggboxplot(tg, x = "dose", y = "len", color = "supp")

#   Para añadir valores p al boxplot añadimos realizamos el t_test
#   Y luego utilizamos a add_xy_position de rstatix
#   que añadirá las columnas y.position, x, xmin y xmax a nuestro tibble

test_t<-tg %>% group_by(dose) %>% t_test(len ~ supp) %>% add_xy_position(x="dose")

#   Ahora hacemos nuestro boxplot añadiendo a stat_pvalue_manual (ggpubr)
#   que leerá las columnas y.position, x, xmin y xmax para colocar las
#   barras de los valores p en nuestra gráfica

box1 + stat_pvalue_manual(test_t)

#   También podemos utilizar add_significance (rstatix) para añadir etiquetas
#   que indiquen si la prueba resultó significativa o no en cada caso

test_t<-tg %>% group_by(dose) %>% t_test(len ~ supp) %>%
  add_xy_position(x="dose") %>% add_significance()

box1 + stat_pvalue_manual(test_t, hide.ns = TRUE)


#   Una alternativa más para añadir los valores p es utilizar distintos
#   páneles para cada dosis

test_t<-tg %>% group_by(dose) %>% t_test(len ~ supp)

ggboxplot( tg, x = "supp", y = "len", color = "supp",
           facet.by = "dose", ylim=c(0,40)) +
  stat_pvalue_manual(test_t, label = "p", y.position = 35)


################################
# TAMAÑO DEL EFECTO PARA EL TEST T
################################

#   El test de Cohen se utiliiza para definir si el tamaño del efecto para el test t
#   es pequeño (>0.2) moderado (>0.5) o alto (>0.8).

tg %>%  group_by(dose) %>% cohens_d(len~supp)

################################################
# ALTERNATIVAS NO PARAMÉTRICAS AL TEST T
################################################

#   Cuando la nuestros datos no tienen una distribución normal necesitamos
#   utilizar un test no paramétrico. El test de Wilcoxon es una alternativa
#   no paramétrica al test t

tg %>% wilcox_test(len ~ supp)

test_w<-tg %>% group_by(dose) %>% wilcox_test(len ~ supp)

ggboxplot( tg, x = "supp", y = "len", color = "supp",
           facet.by = "dose", ylim=c(0,40)) +
  stat_pvalue_manual(test_w, label = "p", y.position = 35)

#  El tamaño del efecto (r) para el test de Wilcoxon se interpreta como
#  0.10 - < 0.3 (pequeño), 0.30 - < 0.5 (moderado) and >= 0.5 (grande).
tg %>%  group_by(dose) %>% wilcox_effsize(len ~supp)


