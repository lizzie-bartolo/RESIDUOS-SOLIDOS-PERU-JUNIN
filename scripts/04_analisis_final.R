####################################################################
# TRABAJO FINAL - R - PARTE 2
# ESTUDIANTE: BARTOLO ARTICA LIZZIE GIMENA
# La brecha de Junin: es por ser mas rural, o hay algo mas?
####################################################################
setwd("C:/Users/LIZZIE/Documents/RESIDUOS_SOLIDOS")
getwd()

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
#install.packages("tidyr") #Instalar si no se tiene la paquetería
library(tidyr)

df <- read_delim(
  "1. Dataset Generación anual de residuos sólidos domiciliarios y municipales.csv",
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = "."),
  trim_ws = TRUE
) %>%
  rename(
    anio = ANIO, departamento = DEPARTAMENTO, gpc_mun = GENERACION_PER_CAPITA_MUNICIPAL,
    clasificacion_mef = CLASIFICACION_MUNICIPAL_MEF
  ) %>%
  mutate(departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(trimws(departamento)))) %>%
  filter(anio == max(anio))


####################################################################
# 1. LA PREGUNTA
####################################################################
#
# En la Parte 1 vimos 2 cosas por separado:
#   - Junin genera menos basura per capita que el promedio del pais
#   - los distritos mas "rurales" (clasificacion MEF de la F a la G)
#     generan menos basura per capita que los mas "de ciudad"
#
# y como Junin tiene bastantes distritos rurales, cabe la duda: la
# brecha de Junin es NOMAS porque tiene mas distritos rurales? o
# incluso comparando peras con peras (un distrito rural de Junin
# contra un distrito rural de cualquier otra parte del pais) Junin
# sigue quedando mas abajo?


####################################################################
# 2. COMPARAMOS JUNIN VS EL RESTO, PERO DENTRO DE CADA CATEGORIA MEF
####################################################################
# (esto es la clave: en vez de comparar Junin contra todo el Peru de
# frente no mas, lo comparamos categoria por categoria, para que la
# comparacion sea justa)

comparacion_mef <- df %>%
  filter(!is.na(clasificacion_mef)) %>%
  mutate(zona = if_else(departamento == "JUNIN", "Junín", "Resto del Perú")) %>%
  group_by(clasificacion_mef, zona) %>%
  summarise(
    gpc_promedio = mean(gpc_mun, na.rm = TRUE),
    n_distritos  = n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = zona, values_from = c(gpc_promedio, n_distritos)) %>%
  mutate(diferencia = `gpc_promedio_Junín` - `gpc_promedio_Resto del Perú`)

comparacion_mef %>% arrange(clasificacion_mef) %>% print(n = 10)


####################################################################
# 3. UN GRAFICO PARA VERLO MEJOR
####################################################################

plot_final <- df %>%
  filter(!is.na(clasificacion_mef)) %>%
  mutate(
    zona = if_else(departamento == "JUNIN", "Junín", "Resto del Perú"),
    clasificacion_mef = factor(clasificacion_mef, levels = c("A","B","D","E","F","G"))
  ) %>%
  filter(!is.na(clasificacion_mef)) %>%
  group_by(clasificacion_mef, zona) %>%
  summarise(gpc_promedio = mean(gpc_mun, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = clasificacion_mef, y = gpc_promedio, fill = zona)) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(aes(label = number(gpc_promedio, accuracy = 0.01)),
            position = position_dodge(width = 0.65), vjust = -0.5, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = c("Junín" = "#d95f0e", "Resto del Perú" = "#2c7fb8")) +
  labs(
    title    = "Aunque comparemos distritos del mismo tipo,\nJunín casi siempre bota menos basura por persona",
    subtitle = "GPC municipal promedio, Junín vs. resto del país, por clasificación MEF (2024)",
    x        = "Clasificación MEF (A = ciudad principal ... G = más rural)",
    y        = "kg por habitante al día",
    fill     = "",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10)
  )

plot_final

ggsave("figures/10_analisis_final_brecha_junin.png", plot = plot_final,
       width = 9, height = 7, dpi = 300, bg = "white")


####################################################################
# 4. LO QUE SACAMOS EN LIMPIO
####################################################################
#
# 1) No, la brecha de Junin NO se explica solo porque tiene mas
#    distritos rurales. Incluso comparando categoria por categoria
#    (un distrito tipo "G" de Junin contra un distrito tipo "G" de
#    cualquier otro departamento), Junin genera menos basura per
#    capita en casi todas las categorias (D, E, F, G).
#
# 2) La unica excepcion es la categoria A (las ciudades principales,
#    ahi esta Huancayo): en ese grupo Junin esta LIGERAMENTE por
#    encima del resto del pais, no por debajo.
#
# 3) Esto sugiere que el problema de Junin no es (solamente) que
#    tiene mas zona rural, sino que incluso sus distritos rurales
#    generan/reportan menos basura que otros distritos rurales del
#    Peru. Puede ser porque de verdad se genera menos basura (menos
#    consumo, mas reuso), o porque el servicio de recojo no llega a
#    todos lados y esa basura no se esta contabilizando bien, algo
#    que valdria la pena revisar con las municipalidades.
#
# Respuesta a la pregunta: la brecha de Junin frente al pais no es
# nomas un tema de "ser mas rural", el patron se repite incluso
# comparando distritos del mismo tipo, EXCEPTO en las ciudades
# principales (Huancayo), donde Junin esta a la par o mejor que el
# resto del pais.
