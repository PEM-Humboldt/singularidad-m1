# ..............................................................................
# SCRIPT: PrioritizR_Run_SingularidadM1_acuatica.R
# PROYECTO: Áreas de singularidad acuática Colombia 
# ..............................................................................

# DESCRIPCIÓN:
# - Utiliza valores de conectividad como costos principales (no como penalización)
# - Aplica penalizaciones lineales basadas en Huella ecológica
# - Targets (10-100%) y penalidades (0-100)
# - Procesamiento paralelo con future y furrr
# - Sistema de registro robusto con manejo de errores
# - El procesamiento paralelo de la version para Colombia presenta algunas mejoras
# - con respecto a la version de la Orinoquía


# AUTOR: [Prioridades de conservación]
# FECHA: [15/12/2025]

# COMENTARIOS:
# LOS COMENTARIOS CON "(USUARIO)" INDICAN LAS FUNCIONES DONDE EL USUARIO DEBE AJUSTAR
# ..............................................................................

# 1. CONFIGURACIÓN INICIAL Y DEPENDENCIAS --------------------------------------

# Verificar e instalar paquete pacman si es necesario
if (!require("pacman")) install.packages("pacman")

# Cargar/instalar todos los paquetes necesarios
pacman::p_load(
  # Análisis espacial
  terra, sf, raster, fasterize,
  # Priorización
  prioritizr, prioritizrdata,
  # Manipulación de datos
  dplyr, tidyr, stringr,
  # Visualización
  ggplot2, ggspatial, viridis,
  # Exportación
  openxlsx,
  # Procesamiento paralelo
  furrr, future, future.apply, progressr,
  # Utilidades
  crayon, progress, exactextractr
)


# 2. CONFIGURACIÓN DEL ENTORNO -------------------------------------------------

# Configurar procesamiento paralelo (USUARIO)
plan(multisession, workers = 8)
# Desactivar cálculos S2 para sf (mejor rendimiento con datos proyectados)
sf_use_s2(FALSE)

# Desactivar notación científica
options(scipen = 999)

# Registrar tiempo de inicio global
global_start_time <- Sys.time()

# Establecer directorio de trabajo (USUARIO)
setwd('D:/PrioritizR_Run_Colombia')

# Directorio base para resultados de conectividad (USUARIO)
output.base.dir <- "Resultados_conectividad"

# 3. CARGA Y PREPARACIÓN DE DATOS ----------------------------------------------

# 3.1. Área de estudio - Microcuencas
ae <- st_read('Costos/costos_conectividad.shp')

# 3.2. Características de biodiversidad
# 3.2.1. Especies (archivos de Biomodelos)
spp.list <- list.files('Caracteristicas/Especies', full.names = TRUE)
spp.list <- rast(spp.list)

# 3.2.2. Ecosistemas
eco.list <- list.files('Caracteristicas/Ecosistemas', full.names = TRUE)
eco.list <- rast(eco.list)
features.list <- c(spp.list, eco.list)
new_names <- gsub(" ", "", names(features.list))
# Limpiar nombres con espacios
names(features.list) <- new_names

# Crear primer stack de características (solo especies y ecosistemas)
raster_stack_v1 <- features.list
crs(raster_stack_v1) <- "EPSG:9377"  # Establecer CRS apropiado

# 3.4.2. Costos principales basados en conectividad
conectividad <- st_read('Costos/costos_conectividad.shp')
conectividad <- st_transform(conectividad, crs = 9377)  # Asegurar CRS consistente
conectividad$costos.conectividad <- 1 - conectividad$nrm__MW 
# plot(conectividad['costos.conectividad'])  

# Crear raster template para visualización
rast_templ <- rast(resolution = 1000, crs = "EPSG:9377", ext = extent(ae))

# 3.6. Preparar datos para penalizaciones lineales
# Incorporar información de integridad para usar en penalizaciones
penalidad.r <- rast('Penalidades/Integridad/penalidad_integridad_COL.tif')
penalidad.r <- 100 - penalidad.r

# 3.7. Plantilla de microcuencas con información de área
# Área total de referencia


# 3.8. PREPROCESAMIENTO DE CARACTERÍSTICAS Y DATOS DE ENTRADA ------------------

# 3.8.1. Matriz de representación (rij matrix) para optimización
# Crear matriz que relaciona unidades de planificación con características de biodiversidad
# Cada celda representa la proporción de cobertura de una característica en una microcuenca
pre_proc_data <- rij_matrix(conectividad, raster_stack_v1)
# Transponer y convertir a dataframe para mejor manipulación
pre_proc_data <- as.data.frame(t(as.matrix(pre_proc_data)))

# 3.8.2. Correcciones de formato y nombres
# Corregir nombre de especie con guion problemático para compatibilidad con prioritizr
names(pre_proc_data)[
  names(pre_proc_data) == 'Leporinus_y-ophorus_10_MAXENT'] <- 
  'Leporinus_yophorus_10_MAXENT'

# 3.8.3. Integración de datos en un único objeto espacial
# Combinar geometrías de microcuencas con datos de características y costos
conectividad.preproc <- cbind(conectividad, pre_proc_data)
# Redondear valores de conectividad a 1 decimal para mejorar eficiencia en procesamiento
conectividad.preproc$costos.conectividad <- round(conectividad.preproc$costos.conectividad, 1)

# 3.8.4. Extracción de valores de penalidad por integridad ecológica
# Los valores de penalidad tambien se ingresan como Dataframe para mejorar evidencia
# Calcular valor promedio de penalidad para cada microcuenca mediante extracción zonal
penalidad.data <- exactextractr::exact_extract(
  x = penalidad.r,                         # Raster de penalidad
  y = conectividad[,"HYBAS_I"],           # Geometrías de microcuencas
  fun = 'mean'                            # Función estadística a aplicar
)

# 3.8.5. Normalización y preparación de datos de penalidad
# Agregar valores de penalidad al objeto principal
conectividad.preproc$Penalidad <- penalidad.data
# Normalizar valores de penalidad: convertir de porcentaje (0-100) a proporción (0-1)
conectividad.preproc$Penalidad <- round(conectividad.preproc$Penalidad/100, 2)

# 3.8.6. Limpieza de datos
# Eliminar microcuencas sin datos de penalidad (valores NA)
conectividad.preproc <- conectividad.preproc[!is.na(conectividad.preproc$Penalidad), ]

# 3.8.7. Creación de plantilla base para análisis
# Extraer solo geometrías y área de las microcuencas procesadas
plantilla.micro <- conectividad.preproc[,"HYBAS_I"]
# Calcular área de cada microcuenca en kilómetros cuadrados
plantilla.micro$area_km2 <- as.numeric(st_area(plantilla.micro) / 10^6)

# 3.8.8. Cálculo del área total de referencia
# Sumar áreas de todas las microcuencas para cálculos de representatividad
total_area_km2 <- sum(plantilla.micro$area_km2)


# PRUEBA DE CONCEPTOS (TESTER) -------------------------------------------------

# Bloque de prueba para verificar configuración antes de ejecución completa
if (F) { # (USUARIO)
  tester_start_time <- Sys.time()
  
  # Construir problema de prueba
  p1 <- problem(conectividad.preproc,  features = names(pre_proc_data), 
                cost_column = 'costos.conectividad') %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_default_solver(verbose = T, gap = 0.02) %>%
    add_binary_decisions() %>%
    add_linear_penalties(penalty = 20, data = 'Penalidad')
  
  # Resolver y medir tiempo
  s1 <- solve(p1)
  tester_end_time <- Sys.time()
  
  # Reportar tiempo de prueba
  cat("Tiempo de prueba:", tester_end_time - tester_start_time, "\n")
  
  # Visualizar solución de prueba
  plot(s1['solution_1'], main = "Solución de prueba - Costos por Conectividad")
}

# 4. PARÁMETROS DE EJECUCIÓN ---------------------------------------------------
# Definir parámetros para los escenarios (USUARIO)
targets <- seq(0.1, 1, 0.1)  # Targets del 10% al 100% 
penalties <- seq(0, 100, 20)   # Rango de penalizaciones (0-100)
scenarios <- c("solucion1") # Solo un escenario para esta corrida

# 5. PREPARACIÓN DE CARPETAS DE RESULTADOS -------------------------------------

# Crear directorio principal si no existe
if (!dir.exists(output.base.dir)) {
  dir.create(output.base.dir)
  cat(blue(paste0("Carpeta principal creada: ", output.base.dir, '\n')))
}

# Crear subdirectorios para cada escenario
for (folder in scenarios) {
  path <- file.path(output.base.dir, folder)
  if (!dir.exists(path)) {
    dir.create(path)
    cat(blue(paste0("Subcarpeta creada: ", path, '\n')))
  }
}

# 6. DEFINICIÓN DE FUNCIONES PRINCIPALES ---------------------------------------
# 6.1. Función de registro robusto
write_log <- function(log_path, escenario, start_time, end_time, summary_table, 
                      penalties, targets, error = NULL) {
  # Crear directorio para logs si no existe
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  
  # Calcular tiempo total de ejecución
  tiempo_total <- round(difftime(end_time, start_time, units = "mins"), 2)
  
  # Iniciar captura de output
  sink(log_path)
  
  # Encabezado del log
  cat("=============================================\n")
  cat(" PRIORIZACIÓN - LOG EJECUCIÓN (COSTOS CONECTIVIDAD)\n")
  cat("=============================================\n")
  cat("Escenario:", escenario, "\n")
  cat("Inicio:", format(start_time), "\n")
  cat("Fin:", format(end_time), "\n")
  cat("Duración (minutos):", tiempo_total, "\n\n")
  
  # Parámetros de ejecución
  cat("Parámetros:\n")
  cat("Penalties:", paste(penalties, collapse = ", "), "\n")
  cat("Targets:", paste(targets, collapse = ", "), "\n")
  cat("Total soluciones:", ifelse(is.null(summary_table), 0, nrow(summary_table)), "\n\n")
  
  # Resumen de áreas priorizadas
  if (!is.null(summary_table) && nrow(summary_table) > 0) {
    cat("Resumen de área priorizada (km2):\n")
    print(summary_table %>% summarise(
      min_area = as.numeric(min(Area_priorizada_km2)),
      max_area = as.numeric(max(Area_priorizada_km2)),
      mean_area = as.numeric(mean(Area_priorizada_km2))
    ))
  }
  
  # Información de error si existe
  if (!is.null(error)) {
    cat("\n*** ERROR DETECTADO ***\n")
    cat(as.character(error), "\n")
  }
  
  cat("=============================================\n")
  
  # Finalizar captura
  sink()
}

# 6.2. Función de optimización por escenario
run_scenario <- function(escenario, p, t) {
  # Determinar configuración según escenario
  raster_stack <- raster_stack_v1  # Solo especies y ecosistemas
  
  # Construir problema de optimización con costos de conectividad
  p1 <-  problem(conectividad.preproc,  features = names(pre_proc_data), 
                 cost_column = 'costos.conectividad') %>%
    add_min_set_objective() %>%
    add_relative_targets(t) %>%
    add_default_solver(verbose = F, gap = 0.01) %>%
    add_binary_decisions() %>% 
    add_linear_penalties(penalty = p, data = 'Penalidad')
  
  # Resolver problema
  s1 <- solve(p1)
  
  # Evaluar cumplimiento de targets
  target_coverage <- eval_target_coverage_summary(p1, s1[,"solution_1"])
  
  # Calcular métricas de la solución
  plantilla.micro$temp_solution <- s1$solution_1
  seleccionadas <- plantilla.micro %>% filter(temp_solution == 1)
  num_microcuencas <- nrow(seleccionadas)
  area_priorizada <- sum(st_area(seleccionadas)) / 10^6  
  representatividad <- (area_priorizada / total_area_km2) * 100
  
  # Devolver resultados
  list(
    scenario = escenario,
    name = paste0("Penalty_", p, "_Target", t),
    penalty = p, 
    target = t,
    num_microcuencas = num_microcuencas,
    area_km2 = area_priorizada,
    representatividad = representatividad,
    coverage = target_coverage,
    solution = s1$solution_1
  )
}

# 7. EJECUCIÓN PRINCIPAL POR ESCENARIOS ----------------------------------------

# Iterar sobre cada escenario definido
for (escenario in scenarios) {
  
  cat(crayon::blue(paste0("\nIniciando ejecución de: ", escenario, " ...\n")))
  
  # Verificar/crear directorio de salida
  escenario_dir <- file.path(output.base.dir, escenario)
  if (!dir.exists(escenario_dir)) {
    dir.create(escenario_dir, recursive = TRUE)
  }
  
  # Crear grid de parámetros para este escenario
  param_grid <- expand.grid(penalty = penalties, target = targets, 
                            stringsAsFactors = FALSE)
  
  # Registrar tiempo de inicio del escenario
  escenario_start_time <- Sys.time()
  
  # Variables para captura de resultados y errores
  error_catch <- NULL
  resultados <- list()
  summary_table <- NULL
  
  # Ejecutar optimizaciones en paralelo con manejo de errores
  tryCatch({
    # Habilitar reporte de progreso
    with_progress({
      p <- progressor(along = 1:nrow(param_grid))
      
      resultados <- future_pmap(
        list(param_grid$penalty, param_grid$target),
        function(p_penalty, t_target) {
          result <- tryCatch({
            run_scenario(escenario, p_penalty, t_target)
          }, error = function(e) {
            list(error = e$message, penalty = p_penalty, target = t_target, scenario = escenario)
          })
          
          # Actualizar progreso
          p()
          
          return(result)
        },
        .options = furrr_options(seed = TRUE)
      )
    })
    
    # Filtrar resultados exitosos
    successful_results <- resultados[sapply(resultados, function(x) is.null(x$error))]
    failed_results <- resultados[sapply(resultados, function(x) !is.null(x$error))]
    
    if (length(failed_results) > 0) {
      warning(paste(length(failed_results), "ejecuciones fallaron"))
      # Mostrar errores
      for (fail in failed_results) {
        cat(crayon::yellow(paste0("  Falló: penalty=", fail$penalty, 
                                  ", target=", fail$target, 
                                  " - ", fail$error, "\n")))
      }
    }
    
    if (length(successful_results) > 0) {
      # Crear tabla resumen de resultados
      summary_table <- bind_rows(lapply(successful_results, function(res) {
        data.frame(
          Escenario = res$scenario,
          Solucion = res$name, 
          Penalty = res$penalty, 
          Target = res$target,
          Microcuencas = res$num_microcuencas, 
          Area_priorizada_km2 = res$area_km2,
          Representatividad_porcentaje = res$representatividad
        )
      }))
      
      # Preparar plantilla con todas las soluciones
      plantilla.micro.temp <- plantilla.micro
      for (res in successful_results) {
        plantilla.micro.temp[[res$name]] <- res$solution
      }
      
      # Exportar resultados espaciales
      st_write(plantilla.micro.temp, 
               file.path(escenario_dir, 
                         paste0("Colombia_", escenario, ".shp")), 
               delete_dsn = TRUE)
      
      # Exportar cobertura de targets por solución
      wb <- createWorkbook()
      for (i in seq_along(successful_results)) {
        addWorksheet(wb, sheetName = successful_results[[i]]$name)
        writeData(wb, sheet = successful_results[[i]]$name, 
                  x = successful_results[[i]]$coverage, 
                  rowNames = FALSE)
      }
      saveWorkbook(wb, 
                   file = file.path(escenario_dir, 
                                    paste0("Colombia_", escenario, "_coverage.xlsx")), 
                   overwrite = TRUE)
      
      # Exportar tabla resumen y datos completos
      write.xlsx(summary_table, 
                 file = file.path(escenario_dir, 
                                  paste0("Colombia_", escenario, "_resumen.xlsx")))
      
      save(resultados, summary_table, 
           file = file.path(escenario_dir, 
                            paste0("Colombia_", escenario, ".RData")))
    } else {
      cat(crayon::red("No se completó ninguna ejecución exitosa\n"))
    }
    
  }, error = function(e) {
    error_catch <<- e
  })
  
  # Registrar tiempo de finalización
  escenario_end_time <- Sys.time()
  
  # Generar log del escenario
  log_file <- file.path(escenario_dir, 
                        paste0("Colombia_", escenario, "_log.txt"))
  
  # Asegurar que la función write_log existe
  if (exists("write_log")) {
    write_log(log_file, escenario, escenario_start_time, escenario_end_time, 
              summary_table, penalties, targets, error_catch)
  } 
  
  # Reportar estado de finalización
  if (is.null(error_catch)) {
    cat(crayon::green(paste0("Finalizado correctamente: ", escenario, 
                             " (", length(successful_results), "/", nrow(param_grid), 
                             " ejecuciones exitosas)\n")))
  } else {
    cat(crayon::red(paste0("Error en ", escenario, ": ", error_catch$message, "\n")))
  }
  
  # Limpiar memoria si es necesario
  rm(resultados, successful_results, failed_results, summary_table)
  gc()
}

# 8. FINALIZACIÓN --------------------------------------------------------------

# Registrar tiempo final y mostrar resumen
global_end_time <- Sys.time()
tiempo_total_global <- round(difftime(global_end_time, global_start_time, units = "hours"), 2)

cat(blue(paste0("\n=============================================\n")))
cat(blue(paste0("EJECUCIÓN COMPLETADA - COSTOS POR CONECTIVIDAD\n")))
cat(blue(paste0("Tiempo total: ", tiempo_total_global, " horas\n")))
cat(blue(paste0("Escenarios ejecutados: ", length(scenarios), "\n")))
cat(blue(paste0("Soluciones generadas: ", length(scenarios) * length(penalties) * length(targets), "\n")))
cat(blue(paste0("Resultados en: ", output.base.dir, "\n")))
cat(blue(paste0("=============================================\n")))

