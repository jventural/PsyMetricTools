#' @title Bootstrap CFA Stability Analysis
#' @description Performs bootstrap CFA stability analysis across different sample sizes.
#' @param modelo Lavaan model specification.
#' @param data Data frame with the data.
#' @param num_replicas Number of bootstrap replications.
#' @param estimator Estimator to use (e.g., "WLSMV").
#' @param seed Random seed (default 2023).
#' @param n_cores Number of cores for parallel processing (default 4).
#' @return Data frame with fit measures and reliability across sample sizes.
#' @export
boot_cfa_stability <- function(modelo, data, num_replicas, estimator, seed = 2023, n_cores = 4) {

  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("Package 'progress' is required. Please install it.")
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required. Please install it.")
  }
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Package 'future.apply' is required. Please install it.")
  }

  # Configura el entorno paralelo
  future::plan(future::multisession, workers = n_cores)

  # Especificar una semilla
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Porcentajes deseados
  porcentajes <- seq(0.9, 0.3, -0.1)

  # Crear la barra de progreso para el ajuste del modelo
  total_ticks <- num_replicas * length(porcentajes)
  pb_fit <- progress::progress_bar$new(total = total_ticks, format = "[:bar] :percent", show_after = 0)

  # Funcion para procesar cada replica con la barra de progreso integrada
  procesar_replicas <- function(i) {
    replica <- data
    resultados_replica <- list()

    for (j in seq_along(porcentajes)) {
      porcentaje <- porcentajes[j]
      n_sample <- ceiling(nrow(replica) * porcentaje)
      muestra <- replica[sample(nrow(replica), size = n_sample, replace = TRUE), ]

      fit.original <- lavaan::cfa(modelo,
                          data = muestra,
                          estimator = estimator,
                          mimic = "Mplus",
                          ordered = TRUE)
      fiabilidad <- semTools::compRelSEM(fit.original, tau.eq = FALSE, ord.scale = TRUE)

      resultados_replica[[paste0(i, "-", porcentaje * 100)]] <- list(fit = fit.original, fiabilidad = fiabilidad)

      # Actualizar la barra de progreso en cada iteracion
      pb_fit$tick()
    }

    return(resultados_replica)
  }

  # Realizar las replicas en paralelo con 'future.seed = TRUE'
  lista_resultados <- future.apply::future_lapply(1:num_replicas, procesar_replicas, future.seed = TRUE)

  # Unir todos los resultados en una lista plana
  lista_resultados_planos <- do.call(c, lista_resultados)

  # Crear la barra de progreso para el calculo de las medidas de ajuste
  pb_fit_measures <- progress::progress_bar$new(total = length(lista_resultados_planos), format = "[:bar] :percent", show_after = 0)

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

  resultado0 <- purrr::map_dfr(lista_bondad_modelo, dplyr::bind_rows, .id = "Muestras")
  resultado1 <- purrr::map_dfr(lista_fiabilidad, dplyr::bind_rows, .id = "Muestras")

  # Renombrar columnas de fiabilidad a F1, F2, etc. para compatibilidad con plot_cfa_stability
  omega_cols <- setdiff(names(resultado1), "Muestras")
  new_omega_names <- paste0("F", seq_along(omega_cols))
  names(resultado1)[names(resultado1) %in% omega_cols] <- new_omega_names

  resultados <- dplyr::inner_join(resultado0, resultado1, by = "Muestras")

  resultados <- resultados %>%
    dplyr::mutate(Replica = stringr::str_extract(Muestras, "\\d+"),
           Porcentaje = stringr::str_extract(Muestras, "\\d+$")) %>%
    dplyr::select(-Muestras)

  return(resultados)
}
