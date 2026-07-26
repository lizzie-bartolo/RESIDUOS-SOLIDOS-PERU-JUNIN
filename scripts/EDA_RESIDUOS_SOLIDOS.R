####################################################################
# TRABAJO FINAL - R
# Residuos solidos en el Peru y en Junin (2019-2024)
# ESTUDIANTE: BARTOLO ARTICA LIZZIE GIMENA
# Data: MINAM (Plataforma Nacional de Datos Abiertos)
####################################################################

# 1. DE QUE TRATA ESTA DATA -----------------------------------------
#
# La saco el MINAM (Ministerio del Ambiente). Basicamente cada
# municipalidad del Peru reporta cuanta basura genera su distrito
# al anio, y con eso arman esta base a nivel distrital, del 2019 al
# 2024.
#
# Lo que mas nos importa para el trabajo:
#   - departamento, provincia, distrito, region_natural, anio
#   - pob_total (poblacion INEI del distrito)
#   - gpc_mun: cuantos kg de basura bota cada persona AL DIA
#   - gen_mun_tanio: total de toneladas de basura que boto ese
#     distrito en TODO el anio (esto no es per capita, es el total)
#
# La idea es ver como estamos en el pais y luego meternos mas a
# fondo en Junin, que es nuestra region.


# 0. DIRECTORIO DE TRABAJO Y PAQUETES QUE VAMOS A USAR -------------------------

setwd("C:/Users/LIZZIE/Documents/RESIDUOS_SOLIDOS")
getwd()

paquetes <- c("dplyr", "ggplot2", "readr", "scales", "patchwork",
              "sf", "geodata", "stringr")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(sf)

dir.create("figures", showWarnings = FALSE) # aqui se van a guardar los graficos



# 2. IMPORTAR LA DATA -------------------------------------------------

df <- read_delim(
  "1. Dataset Generación anual de residuos sólidos domiciliarios y municipales.csv",
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = "."),
  trim_ws = TRUE
)

df %>% glimpse()


# 3. LIMPIEZA -----------------------------------------------------------

# le puse nombres mas cortos a las columnas, para no escribir tanto
df <- df %>%
  rename(
    anio             = ANIO,
    departamento     = DEPARTAMENTO,
    provincia        = PROVINCIA,
    distrito         = DISTRITO,
    region_natural   = REGION_NATURAL,
    tipo_muni        = TIPO_MUNICIPALIDAD,
    pob_total        = POB_TOTAL_INEI,
    pob_urbana       = POB_URBANA_INEI,
    pob_rural        = POB_RURAL_INEI,
    clasificacion_mef = CLASIFICACION_MUNICIPAL_MEF,
    gpc_dom          = GENERACION_PER_CAPITA_DOM,
    gen_dom_tdia     = GENERACION_DOM_URBANA_TDIA,
    gen_dom_tanio    = `GENERACION_DOM URBANA_TANIO`,
    gen_mun_tanio    = GENERACION_MUN_TANIO,
    gen_mun_tdia     = GENERACION_MUN_TDIA,
    gpc_mun          = GENERACION_PER_CAPITA_MUNICIPAL
  )

# ojo con esto: "LIMA" aparece en algunas filas con un espacio de mas
# ("LIMA "), entonces R lo cuenta como si fuera OTRO departamento
# distinto. Tambien le quito las tildes y lo paso a mayuscula, para
# que despues calce bien con los nombres del mapa.
df <- df %>%
  mutate(
    departamento = trimws(departamento),
    departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(departamento)),
    provincia    = chartr("ÁÉÍÓÚ", "AEIOU", toupper(trimws(provincia)))
  )

# nos quedamos con el ultimo anio (2024) para los mapas y rankings,
# pero guardamos toda la serie por si despues queremos ver tendencia
df_2024 <- df %>% filter(anio == max(anio))

df %>% dim()
df_2024 %>% dim()


# 4. ESTADISTICAS DESCRIPTIVAS ------------------------------------------

# cuantos NA hay por columna (para saber si hay que preocuparse)
df %>% is.na() %>% colSums() %>% head(10)

# cuantos distritos hay por region natural (costa/sierra/selva)
df_2024 %>% count(region_natural, sort = TRUE)

# como se mueve la GPC (basura por persona al dia) a nivel distrital
df_2024 %>%
  summarise(
    promedio = mean(gpc_mun, na.rm = TRUE),
    mediana  = median(gpc_mun, na.rm = TRUE),
    sd       = sd(gpc_mun, na.rm = TRUE),
    min      = min(gpc_mun, na.rm = TRUE),
    max      = max(gpc_mun, na.rm = TRUE)
  )

# ahora agrupamos por departamento: sumamos toda la basura del
# departamento y toda su poblacion, y con eso calculamos su propia
# GPC (asi es mas correcto que promediar los GPC de cada distrito
# sin ponderar por poblacion)
resumen_departamento <- df_2024 %>%
  group_by(departamento) %>%
  summarise(
    gen_total_ton = sum(gen_mun_tanio, na.rm = TRUE),
    poblacion     = sum(pob_total, na.rm = TRUE)
  ) %>%
  mutate(gpc = (gen_total_ton * 1000) / (poblacion * 365)) %>%
  arrange(desc(gpc))

resumen_departamento

# lo mismo pero solo dentro de Junin, por provincia
resumen_junin_provincia <- df_2024 %>%
  filter(departamento == "JUNIN") %>%
  group_by(provincia) %>%
  summarise(
    gen_total_ton = sum(gen_mun_tanio, na.rm = TRUE),
    poblacion     = sum(pob_total, na.rm = TRUE)
  ) %>%
  mutate(gpc = (gen_total_ton * 1000) / (poblacion * 365)) %>%
  arrange(desc(gpc))

resumen_junin_provincia

# y el ranking de distritos de Junin que MAS basura generan en total
# (aqui ya no es per capita, es el total de toneladas al anio, para
# ver donde esta el "grueso" del problema en la region)
top_distritos_junin <- df_2024 %>%
  filter(departamento == "JUNIN") %>%
  select(distrito, provincia, gen_mun_tanio, pob_total) %>%
  arrange(desc(gen_mun_tanio)) %>%
  slice_head(n = 10)

top_distritos_junin


# 5. GRAFICOS ------------------------------------------------------------

# --- grafico 1: ranking de departamentos por GPC, Junin resaltado ---

plot1 <- resumen_departamento %>%
  mutate(destacado = if_else(departamento == "JUNIN", "Junín", "Otro departamento")) %>%
  ggplot(aes(x = reorder(departamento, gpc), y = gpc, fill = destacado)) +
  geom_col(show.legend = FALSE, width = 0.75) +
  geom_text(aes(label = number(gpc, accuracy = 0.01)),
            hjust = -0.15, size = 3.6, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.15), expand = c(0, 0)) +
  scale_fill_manual(values = c("Junín" = "#d95f0e", "Otro departamento" = "#2c7fb8")) +
  labs(
    title    = "Junín está cerca del promedio nacional,\nmuy lejos de Lima",
    subtitle = "Generación per cápita de residuos municipales (kg/hab/día), 2024",
    x        = "",
    y        = "kg por habitante al día",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10.5),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot1

ggsave("figures/01_ranking_departamentos.png", plot = plot1,
       width = 8, height = 8, dpi = 300, bg = "white")


# --- grafico 2 (mapa): Peru por departamento, GPC ---

# esto baja el mapa del Peru la primera vez que lo corres (tarda un
# poco, es normal, es la misma logica que vimos en la sesion de mapas)
mp_peru <- geodata::gadm(country = "PER", level = 1, path = tempdir()) %>%
  sf::st_as_sf() %>%
  filter(TYPE_1 == "Región") # el Callao sale aparte en el catalogo, lo sacamos

mp_peru$departamento <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(mp_peru$NAME_1))

plot2 <- mp_peru %>%
  left_join(resumen_departamento, by = "departamento") %>%
  ggplot() +
  geom_sf(aes(fill = gpc), color = "white", linewidth = 0.2) +
  geom_sf(data = . %>% filter(departamento == "JUNIN"),
          fill = NA, color = "#2c3e50", linewidth = 1.1) + # asi resaltamos donde queda Junin
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey90") +
  labs(
    title    = "¿Dónde se genera más basura por\npersona en el Perú?",
    subtitle = "GPC de residuos municipales (kg/hab/día), 2024",
    fill     = "kg/hab/día",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot2

ggsave("figures/02_mapa_peru_gpc.png", plot = plot2,
       width = 8, height = 9, dpi = 300, bg = "white")


# --- grafico 3 (mapa): Junin por provincia, GPC ---

mp_junin <- geodata::gadm(country = "PER", level = 2, path = tempdir()) %>%
  sf::st_as_sf()

mp_junin$departamento <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(mp_junin$NAME_1))
mp_junin$provincia    <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(mp_junin$NAME_2))
mp_junin <- mp_junin %>% filter(departamento == "JUNIN")

plot3 <- mp_junin %>%
  left_join(resumen_junin_provincia, by = "provincia") %>%
  ggplot() +
  geom_sf(aes(fill = gpc), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = paste0(stringr::str_to_title(provincia), "\n", number(gpc, accuracy = 0.01))),
               size = 2.9, fontface = "bold", color = "grey15") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey90") +
  labs(
    title    = "Huancayo genera casi el triple de basura\nper cápita que el resto de Junín",
    subtitle = "GPC de residuos municipales (kg/hab/día), 2024",
    fill     = "kg/hab/día",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot3

ggsave("figures/03_mapa_junin_provincias.png", plot = plot3,
       width = 8, height = 8, dpi = 300, bg = "white")


# --- grafico 4: top 10 distritos de Junin que MAS basura botan (total) ---
# (este es distinto a los de arriba: ya no es "por persona", es el
# monton total de toneladas al anio, para ver donde se concentra
# fisicamente el problema)

plot4 <- top_distritos_junin %>%
  mutate(etiqueta = paste0(stringr::str_to_title(distrito), " (", stringr::str_to_title(provincia), ")")) %>%
  ggplot(aes(x = reorder(etiqueta, gen_mun_tanio), y = gen_mun_tanio)) +
  geom_col(fill = "#31a354", width = 0.72) +
  geom_text(aes(label = comma(round(gen_mun_tanio))),
            hjust = -0.15, size = 3.6, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80000), expand = c(0, 0),
                      labels = comma) +
  labs(
    title    = "El Tambo, Huancayo y Chilca concentran\nla basura de toda la región",
    subtitle = "Top 10 distritos de Junín con más residuos municipales (toneladas/año), 2024",
    x        = "",
    y        = "Toneladas de basura al año",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot4

ggsave("figures/04_top_distritos_junin.png", plot = plot4,
       width = 8, height = 8, dpi = 300, bg = "white")


# --- grafico 5: caja y bigotes, GPC segun costa/sierra/selva ---
# (esto es a nivel de TODO el pais, no solo Junin, para ver si el
# tipo de region natural influye en cuanta basura se bota por
# persona. Lo hacemos igual que en clase: boxplot + jitter encima)

plot5 <- df_2024 %>%
  filter(!is.na(region_natural)) %>%
  ggplot(aes(x = region_natural, y = gpc_mun, fill = region_natural)) +
  geom_boxplot(outliers = FALSE, alpha = 0.8, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.08, size = 1.4, show.legend = FALSE) +
  scale_fill_manual(values = c("COSTA" = "#2c7fb8", "SIERRA" = "#d95f0e", "SELVA" = "#31a354")) +
  labs(
    title    = "La sierra genera menos basura por persona\nque la costa y la selva",
    subtitle = "Distribución de la GPC municipal por región natural, distritos del Perú, 2024",
    x        = "",
    y        = "kg por habitante al día",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot5

ggsave("figures/05_boxplot_region_natural.png", plot = plot5,
       width = 8, height = 8, dpi = 300, bg = "white")


# --- grafico 6: histograma + densidad de la GPC, con el promedio ---
# (igual que en la EDA_CLASE 4, para ver donde se concentra la
# mayoria de distritos y donde cae Junin respecto al resto)

promedio_nacional <- df_2024 %>% summarise(prom = mean(gpc_mun, na.rm = TRUE))
promedio_junin    <- df_2024 %>% filter(departamento == "JUNIN") %>%
  summarise(prom = mean(gpc_mun, na.rm = TRUE))

plot6 <- df_2024 %>%
  ggplot(aes(x = gpc_mun)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                  fill = "#9ecae1", color = "white") +
  geom_density(color = "#08519c", linewidth = 1) +
  geom_vline(data = promedio_nacional, aes(xintercept = prom),
             color = "grey30", linetype = "dashed", linewidth = 1) +
  geom_vline(data = promedio_junin, aes(xintercept = prom),
             color = "#d95f0e", linetype = "dashed", linewidth = 1) +
  annotate("text", x = promedio_nacional$prom, y = 3.6, label = "Promedio Perú",
           angle = 90, vjust = -0.6, size = 3.3, color = "grey30") +
  annotate("text", x = promedio_junin$prom, y = 3.6, label = "Promedio Junín",
           angle = 90, vjust = -0.6, size = 3.3, color = "#d95f0e") +
  labs(
    title    = "La mayoría de distritos del país bota entre\n0.5 y 0.8 kg de basura por persona al día",
    subtitle = "Distribución de la GPC municipal a nivel distrital, Perú, 2024",
    x        = "kg por habitante al día",
    y        = "Densidad",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot6

ggsave("figures/06_histograma_gpc.png", plot = plot6,
       width = 8, height = 8, dpi = 300, bg = "white")


# un dato mas antes de pasar a graficos: la GPC segun el tipo de
# municipalidad que dice el MEF (A = ciudad principal grande, hasta
# G = bien rural). Esto nos sirve para el grafico 7.
df_2024 %>%
  filter(!is.na(clasificacion_mef)) %>%
  group_by(clasificacion_mef) %>%
  summarise(gpc_promedio = mean(gpc_mun, na.rm = TRUE), n = n()) %>%
  arrange(desc(gpc_promedio))

# y la tendencia Junin vs Peru en toda la serie 2019-2024 (no solo
# el 2024), ponderando por poblacion en cada anio, para el grafico 9
tendencia_peru_junin <- df %>%
  mutate(es_junin = if_else(departamento == "JUNIN", "Junín", "Perú (todo)")) %>%
  group_by(anio, es_junin) %>%
  summarise(
    gen_total_ton = sum(gen_mun_tanio, na.rm = TRUE),
    poblacion     = sum(pob_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(gpc = (gen_total_ton * 1000) / (poblacion * 365))

tendencia_peru_junin


# --- grafico 7: la basura per capita segun que tan "urbano" es el
# distrito (clasificacion del MEF, de A a G) ---
# aqui se nota clarito la relacion: mientras mas "de ciudad" es la
# municipalidad, mas basura bota cada persona

plot7 <- df_2024 %>%
  filter(!is.na(clasificacion_mef)) %>%
  mutate(clasificacion_mef = factor(clasificacion_mef,
                                     levels = c("C","A","D","B","E","F","G"))) %>%
  ggplot(aes(x = clasificacion_mef, y = gpc_mun, fill = clasificacion_mef)) +
  geom_boxplot(outliers = FALSE, show.legend = FALSE, alpha = 0.85) +
  geom_jitter(width = 0.15, alpha = 0.06, size = 1.2, show.legend = FALSE) +
  scale_fill_viridis_d(option = "D") +
  labs(
    title    = "Mientras más 'de ciudad' es la\nmunicipalidad, más basura per cápita bota",
    subtitle = "GPC municipal según clasificación MEF (C = Lima Metrop., G = la más rural), 2024",
    x        = "Clasificación MEF",
    y        = "kg por habitante al día",
    caption  = "Fuente: MINAM - SIGERSOL / clasificación: MEF"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 9.5),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot7

ggsave("figures/07_boxplot_clasificacion_mef.png", plot = plot7,
       width = 8, height = 8, dpi = 300, bg = "white")


# --- grafico 8: a mas poblacion, se bota mas basura por persona? ---
# (scatter + linea de regresion, igual que en EDA_PREDIOS.R)

plot8 <- df_2024 %>%
  filter(pob_total > 0, !is.na(gpc_mun)) %>%
  ggplot(aes(x = pob_total, y = gpc_mun)) +
  geom_point(pch = 21, fill = "#2c7fb8", alpha = 0.35, size = 2) +
  geom_smooth(method = "lm", color = "#d95f0e", fill = "#d95f0e", alpha = 0.15) +
  scale_x_log10(labels = comma) +
  labs(
    title    = "Los distritos con más gente también\ntienden a botar más basura por persona",
    subtitle = "Población distrital (escala log) vs. GPC municipal, Perú, 2024",
    x        = "Población total del distrito (escala log)",
    y        = "kg por habitante al día",
    caption  = "Fuente: MINAM - SIGERSOL / INEI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot8

ggsave("figures/08_dispersion_poblacion_gpc.png", plot = plot8,
       width = 8, height = 8, dpi = 300, bg = "white")


# --- grafico 9: como se ha movido la GPC de Junin vs el Peru,
# 2019-2024 (para ver si la brecha se cierra o sigue igual) ---

plot9 <- tendencia_peru_junin %>%
  ggplot(aes(x = anio, y = gpc, color = es_junin)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_text(aes(label = number(gpc, accuracy = 0.001)), vjust = -1.2, size = 3.3,
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2019, 2024, 1)) +
  scale_color_manual(values = c("Junín" = "#d95f0e", "Perú (todo)" = "#2c7fb8")) +
  labs(
    title    = "Junín sube al mismo ritmo que el país,\npero la brecha casi no se cierra",
    subtitle = "GPC municipal ponderada por población, 2019-2024",
    x        = "Año",
    y        = "kg por habitante al día",
    color    = "",
    caption  = "Fuente: MINAM - SIGERSOL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

plot9

ggsave("figures/09_tendencia_junin_peru.png", plot = plot9,
       width = 8, height = 8, dpi = 300, bg = "white")


# CREAMOS EL COLLAGE

library(patchwork)

collage <- (plot2 + plot3 + plot1) / (plot4 + plot5 + plot6) / (plot7 + plot8 + plot9) +
  patchwork::plot_annotation(
    title = "Residuos sólidos en el Perú y en Junín (2019-2024)",
    theme = theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5))
  )

ggsave("figures/collage_graficos.png", plot = collage,
       width = 26, height = 24, dpi = 300, bg = "white")

