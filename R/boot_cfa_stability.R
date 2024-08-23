boot_cfa_stability <- function(modelo, data, num_replicas, estimator, seed = 2023, n_cores = 4) {
  library(progress)
  library(pbapply)
  library(tidyverse)
  library(lavaan)
  library(semTools)
  library(future)
  library(future.apply)

  # Configura el entorno paralelo
  plan(multisession, workers = n_cores)

  # Especificar una semilla
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Porcentajes deseados
  porcentajes <- seq(0.9, 0.3, -0.1)

  # Crear la barra de progreso para el ajuste del modelo
  total_ticks <- num_replicas * length(porcentajes)
  pb_fit <- progress_bar$new(total = total_ticks, format = "[:bar] :percent", show_after = 0)

  # Función para procesar cada réplica con la barra de progreso integrada
  procesar_replicas <- function(i) {
    replica <- data
    resultados_replica <- list()

    for (j in seq_along(porcentajes)) {
      porcentaje <- porcentajes[j]
      muestra <- replica %>% sample_frac(porcentaje, replace = TRUE)

      fit.original <- cfa(modelo,
                          data = muestra,
                          estimator = estimator,
                          mimic = "Mplus",
                          ordered = TRUE)
      fiabilidad <- semTools::compRelSEM(fit.original, tau.eq = FALSE, ord.scale = TRUE)

      resultados_replica[[paste0(i, "-", porcentaje * 100)]] <- list(fit = fit.original, fiabilidad = fiabilidad)

      # Actualizar la barra de progreso en cada iteración
      pb_fit$tick()
    }

    return(resultados_replica)
  }

  # Realizar las réplicas en paralelo con 'future.seed = TRUE'
  lista_resultados <- future_lapply(1:num_replicas, procesar_replicas, future.seed = TRUE)

  # Unir todos los resultados en una lista plana
  lista_resultados_planos <- do.call(c, lista_resultados)

  # Crear la barra de progreso para el cálculo de las medidas de ajuste
  pb_fit_measures <- progress_bar$new(total = length(lista_resultados_planos), format = "[:bar] :percent", show_after = 0)

  lista_bondad_modelo <- list()
  lista_fiabilidad <- list()

  for (i in seq_along(lista_resultados_planos)) {
    porcentaje <- names(lista_resultados_planos)[i]
    fit_model <- lista_resultados_planos[[i]]$fit
    fiabilidad <- lista_resultados_planos[[i]]$fiabilidad

    medidas_bondad <- lavaan::fitMeasures(fit_model, c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled"))

    lista_bondad_modelo[[porcentaje]] <- medidas_bondad
    lista_fiabilidad[[porcentaje]] <- fiabilidad

    pb_fit_measures$tick()
  }

  resultado0 <- map_dfr(lista_bondad_modelo, bind_rows, .id = "Muestras")
  resultado1 <- map_dfr(lista_fiabilidad, bind_rows, .id = "Muestras")

  resultados <- inner_join(resultado0, resultado1)

  resultados <- resultados %>%
    mutate(Replica = str_extract(Muestras, "\\d+"),
           Porcentaje = str_extract(Muestras, "\\d+$")) %>%
    select(-Muestras)

  return(resultados)
}
