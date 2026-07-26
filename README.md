# Residuos Sólidos en el Perú y en Junín (2019-2024)

Trabajo final del curso de R — Análisis Exploratorio de Datos (EDA)

## 1. Contexto del conjunto de datos

- **Institución:** Ministerio del Ambiente (MINAM), a través del Sistema de Información para la Gestión de Residuos Sólidos (SIGERSOL).
- **De qué trata:** cada municipalidad del Perú reporta cuánta basura (residuos domiciliarios y municipales) genera su distrito cada año. Con eso arman esta base, a nivel de los 1,891 distritos del país, entre 2019 y 2024.
- **Variables principales:**
  - `departamento`, `provincia`, `distrito`, `region_natural`, `anio`
  - `pob_total`: población del distrito (INEI)
  - `gpc_mun`: generación per cápita de residuos municipales (kg por habitante al día)
  - `gen_mun_tanio`: total de toneladas de basura generadas en el año (no per cápita)
  - `clasificacion_mef`: qué tan "de ciudad" es la municipalidad según el MEF (A = ciudad principal grande, hasta G = la más rural)

## 2. Estructura del repositorio

```
residuos-solidos-peru-junin/
├── data/
│   ├── 1_Dataset_residuos_solidos.csv
│   ├── 2_Metadatos_residuos_solidos.docx
│   └── 3_DiccionarioDatos_residuos_solidos.xlsx
├── figures/
│   ├── 01_ranking_departamentos.png
│   ├── 02_mapa_peru_gpc.png
│   ├── 03_mapa_junin_provincias.png
│   ├── 04_top_distritos_junin.png
│   ├── 05_boxplot_region_natural.png
│   ├── 06_histograma_gpc.png
│   ├── 07_boxplot_clasificacion_mef.png
│   ├── 08_dispersion_poblacion_gpc.png
│   ├── 09_tendencia_junin_peru.png
│   ├── 10_analisis_final_brecha_junin.png
│   └── collage_graficos.png
├── scripts/
│   ├── EDA_RESIDUOS_SOLIDOS.R
│   └── 04_analisis_final.R
└── README.md
```

## 3. Parte 1 — Principales hallazgos del EDA

- **Junín está cerca del promedio nacional** de generación de basura per cápita (0.60 kg/hab/día vs. 0.68 del país), muy lejos de Lima (0.96).
- **Dentro de Junín, Huancayo genera casi el triple per cápita** que el resto de provincias — y junto con El Tambo y Chilca (los 3 distritos que forman la ciudad de Huancayo) concentran el grueso de toda la basura de la región.
- **La sierra genera menos basura per cápita** que la costa y la selva a nivel nacional.
- **Mientras más "de ciudad" es una municipalidad (clasificación MEF), más basura per cápita genera** — hay una relación bien clara ahí.
- Los distritos con más población también tienden a generar algo más de basura por persona.
- **Tanto el Perú como Junín han ido generando más basura per cápita cada año** entre 2019 y 2024, pero la brecha entre ambos casi no se ha cerrado.

## 4. Parte 2 — Pregunta de análisis

**¿La brecha de Junín frente al resto del país se explica solo porque tiene más distritos rurales, o hay algo más detrás?**

Se comparó la generación per cápita de Junín contra el resto del país, pero **dentro de cada categoría de clasificación MEF** (comparando distritos del mismo tipo entre sí, no Junín contra todo el país de frente).

**Conclusiones:**

1. La brecha **no** se explica solo por tener más zona rural: incluso comparando un distrito rural de Junín contra un distrito rural de cualquier otra parte del país, Junín genera menos basura per cápita en casi todas las categorías (D, E, F, G).
2. La única excepción es la categoría A (ciudades principales, ahí está Huancayo): en ese grupo Junín está ligeramente por encima del resto del país, no por debajo.
3. Esto sugiere que el problema de Junín no es solamente su ruralidad — puede deberse a menor consumo real, o a que el servicio de recojo de basura no llega a todos los distritos rurales y por eso no se está contabilizando toda la basura que en realidad se genera. Valdría la pena revisarlo con las municipalidades.

## 5. Fuente

MINAM - SIGERSOL, Plataforma Nacional de Datos Abiertos del Estado Peruano. Elaboración propia.
