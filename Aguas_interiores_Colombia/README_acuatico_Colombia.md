#💧  Áreas prioritarias para la conservación de ecosistemas de aguas interiores de Colombia 🇨🇴

La PSC en los ecosistemas de aguas dulce interiores presenta rezagos teóricos y metodológicos en comparación a los ámbitos terrestres y marinos, debido a la complejidad de la conectividad fluvial, la falta de datos de distribución de especies y su alta variabilidad espacial y temporal. Algunos desafíos metodológicos que complican los procesos de priorización en estos ecosistemas son: 
*	Definición de las unidades de planeación acuáticas.
*	Conservación basada en procesos, en lugar de conservación unicamente basada en área.
* Selección de verdaderos sustitutos (biodiversidad representativa) de la biodiversidad acuática.

En este repositorio se compilan las rutinas para la priorización de ecosistemas de aguas interiores con base en metas nacionales y globales. En este caso se reescalo para toda Colombia, las metodologías utilizadas en el repositorio de la Orinoquía.

---
# Dependencias 🇨🇴
* [R](https://cran.r-project.org/mirrors.html)

# Prerequisitos 🇨🇴
El paquete [prioritizr](https://prioritizr.net/) permite ejecutar las funciones más importantes para la priorización de zonas de conservación. En su repositorio se puede encontrar una descripción detallada de cada una de sus funciones.

```R
# Instalación prioritizr
install.packages("prioritizr", repos = "https://cran.rstudio.com/")

# Librerias necesarias
library(terra)
library(sf)
library(prioritizr)
library(dplyr)
library(tidyr)
library(fasterize)
library(openxlsx)
library(crayon)
library(furrr)
library(future)
library(future.apply)
library(progressr)
library(progress)
library(exactextractr)

# Versiones utilizadas
package_versions <- list(
  terra = "1.8-15",
  sf = "1.0-18", 
  prioritizr = "8.0.4",
  dplyr = "1.1.4",
  tidyr = "1.3.1",
  fasterize = "1.1.0",
  openxlsx = "4.2.8",
  crayon = "1.5.3",
  furrr = "0.3.1",
  future = "1.58.0",
  progress = "1.2.3",
  future.apply = "1.20.1",
  progressr = "0.18.0"
)
```
---
# Descripción flujo de análisis 🇨🇴

La PSC para las aguas interiores de Colombia siguió una metodología de cuatro etapas (diagrama abajo): (i) Conceptualización: se definieron unidades de planificación, metas y objetivos de conservación, incluyendo la selección de portafolios (por ejemplo, escenarios con y sin restricciones); (ii) Preprocesamiento de datos: configuración del conjunto de datos de entrada (e.g. características hidrológicas y distribuciones de especies)  para garantizar consistencia espacial y temática; (iii) Algoritmo de optimización: el modelo PrioritizR fue configurado con restricciones espaciales, métricas de conectividad y capas de costo, y ejecutado paralelamente e iterativamente para generar áreas prioritarias; y (iv) Postprocesamiento: los resultados fueron evaluados con base en la representatividad de las aguas interiores e interpretados frente a otros productos espaciales (por ejemplo, mapas de cobertura del suelo).

![Image](https://github.com/PEM-Humboldt/singularidad-m1/blob/c7c27070daa186bf5bf9753432b2d035842e6ced/Imagenes/Workflow_aguas_interiorires_Colombia.png)


## Ejecución del algoritmo 🇨🇴
Específicamente la etapa cuatro del flujo de análisis comprende las funciones principales para el desarrollo del algoritmo de priorización, en esta fase se generan 60 portafolios que resultan de la combinación de metas de conservación (10-100%) y factores de penalidad (0-100) Por la complejidad de las combinaciónes entre estas variables de análisis, se utilizó una estructura paralelizada (paquetes `furr`, `future` y `future.apply`) que ayudan a reducir significativamente los tiempos de ejecución. Este repositorio contiene sola una version para la ejecución del algoritmo: 

* Costos por conectividad: PrioritizR_Run_SingularidadM1_acuatica.R

Esta rutina se componen de 8 secciones las cuales pueden visualizarse fácilmente en RStudio:

![Image](https://github.com/PEM-Humboldt/singularidad-m1-2023/blob/aabf8efe33f2557afb01f132271292dd56363477/Imagenes/Secciones_rutina_aguas_interiores.png)


## Archivos necesarios 🇨🇴
Para ambos ruinas se necesitan al menos siete archivos principales que son nombrados en el código de la siguiente manera:
```R
# INSUMOS -----------------------------------------------------------------

# Área de estudio
ae <- st_read('Area_estudio/Microcuencas.shp')
# Especies
spp.list <- list.files('Caracteristicas/Especies/biomodelos', full.names = T)
# Ecosistemas
eco.list <- list.files('Caracteristicas/Ecosistemas/Estandarizados', full.names = T)
# Cultura
cul.list <- list.files('Caracteristicas/Cultura/Estandarizados', full.names = T)
# Inclusiones
locked.in1 <- raster("Restricciones/Inclusion/RUNAP_1000_stdr.tif")
# Costos
# Costos por integridad
costo.int <- st_read('Costos/Integridad_total_cor.shp')
# costos por conectividad
conectividad <- st_read("Conectividad/microcuencas_con_CI.shp")
```
## Problema de optimización 🇨🇴

Posteriormente se desarrolla un problema de optimización mediante la función `problem` en donde se incluyen todos componentes típicos de un problema de priorización (restricciones, penalidades, características de conservación y costos) como se detalla a cuantinuación. Las dos rutinas se plantearon de forma complementaria, en donde se consideran criterios de *Integridad* y *Conectividad*, pero en componentes diferentes del problema de priorización. Es decir, en los costos por integridad se usan penalidades de conectividad, y en costos por conectividad, se usan penalidades de integridad.

| Componente | Descripción | Comando |
| :--- | :--- | :--- |
| **Características** | 1077 mapas de distribución de especies y ecosistemas relacionados con sistemas de aguas interiores.| `problem(features = capa_características)` |
| **Costos** | Una aproximaciones de costos por *Conectividad*. Define las unidades de planificación, asociando cada unidad con un valor de costo. | `problem(x = capa_costos, cost_column = 'Nombre_columna')` |
| **Penalidades** | Castigan o premian áreas específicas basándose en criterios ecológicos de *Huella Humana*. Los valores se modifican según un factor de penalidad (p) numérico. | `add_linear_penalties(penalty = p, data = 'columna_penalidad')` |
| **Metas** | Definen el porcentaje de representatividad (t) de las características a alcanzar en las áreas priorizadas. Valores escalados 0-1 (1 = 100% de representatividad). | `add_relative_targets(t)` |
| **Objetivos** | Eje principal del problema de optimización: establece la relación entre representación de características y costos (maximizar representación, minimizar costos ecológicos). | `add_min_set_objective()` |


---

# Errores comunes

🚨Paquetes no instalados o conflictos entre versiones.

📁 Archivos de entrada no encontrados o rutas incorrectas.

🗺️ Inconsistencias entre los CRS de diferentes capas.

💾 Agotamiento de memoria o fallos en procesamiento paralelo: El número de workers (núcleos) utilizados, es de los factores más comunes de error en la rutina. Se recomienda hacer pruebas experimentales para encontrar el número que más se ajuste a la memoría disponible en el computador. Se recomienda usar entre 6 y 8 workers si el computador lo permite.


# Autores(as) y contacto
* **[Elkin Alexi Noguera Urbano](https://github.com/elkalexno)** - *Investigador Titular. I. Humboldt* -  Contacto: enoguera@humboldt.org.co
* Maria Alejandra Molina Berbeo  *Investigador Asistente. I. Humboldt* - Contacto: mmolina@humboldt.org.co 
* Henry Manuel Garcia Diego *Investigador Asistente. I. Humboldt* - Contacto: hmgarcia@humboldt.org.co 
* **[Edwin Uribe Velasquez](https://github.com/edwinuribeecobio)** - *Investigador Asistente. I. Humboldt* - Contacto: euribe@humboldt.org.co

## Licencia

Este proyecto está licenciado bajo la licencia MIT. Para obtener más información, consulte el archivo [LICENCIA](https://github.com/PEM-Humboldt/singularidad-m1-2023/blob/5775e9725df540cf04fb170b167f19b88f00bedf/LICENSE). 



# Referencias



Funciones de referencia: https://prioritizr.net/reference/index.html

Repositorio de referencia y tutoriales: https://prioritizr.net/
