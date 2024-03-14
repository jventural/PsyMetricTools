BootstrapEFA_Algorithm <- function(original_data, n_factors, estimator = "WLSMV", name_items = "RAS",
                            n_items = NULL, specific_items = NULL, rotation = "oblimin", verbose = FALSE,
                            n_samples = 100, seed = NULL) {
  #función internal
  generateBootstrapSamples <- function(data, n_samples = 100, seed = NULL) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    bootstrapped_samples <- lapply(1:n_samples, function(i) {
      data[sample(nrow(data), replace = TRUE), ]
    })
    return(bootstrapped_samples)
  }

  library(pbapply)  # Asegúrate de que esta línea esté al principio de tu script
  # Genera muestras bootstrap
  bootstrapped_samples <- generateBootstrapSamples(original_data, n_samples, seed)

  # Inicializa contadores para advertencias y errores
  count_warnings <- 0
  count_errors <- 0

  bootstrap_results <- list()  # Inicializa la lista de resultados

  for(i in 1:length(bootstrapped_samples)) {
    cat(sprintf("Iniciando la iteración %d de %d \n", i, n_samples))
    pb <- txtProgressBar(min = 0, max = 1, style = 3)  # Crea una barra de progreso para cada iteración

    result <- tryCatch({
      iterative_factor_analysis(bootstrapped_samples[[i]], n_factors, estimator, name_items, n_items, specific_items, rotation, verbose)
    }, error = function(e) {
      message("\n Error durante el bootstrap: ", e$message)
      count_errors <<- count_errors + 1
      NULL  # Devuelve NULL para muestras que resulten en error.
    }, warning = function(w) {
      message("\n Advertencia durante el bootstrap: ", w$message)
      count_warnings <<- count_warnings + 1
      NULL  # Devuelve NULL para muestras que resulten en advertencia.
    })

    if (!is.null(result)) {
      bootstrap_results[[i]] <- result
    }

    setTxtProgressBar(pb, 1)  # Actualiza la barra de progreso al final de cada iteración
    close(pb)
    cat("Iteración", i, "completada\n")
  }

  # Filtra los resultados nulos que podrían haberse generado debido a errores
  bootstrap_results <- Filter(Negate(is.null), bootstrap_results)

  # Procesar los resultados para empaquetarlos adecuadamente
  processed_results <- lapply(bootstrap_results, function(result) {
    list(
      final_model = result$final_model,
      excluded_items = result$excluded_items,
      final_items = result$final_items,
      all_specifications = result$all_specifications,
      all_fit_measures = result$all_fit_measures,
      all_results = result$all_results,
      all_interFactors = result$all_interFactors
    )
  })

  # Agrega los contadores de advertencias y errores a los resultados procesados
  results_summary <- list(
    processed_results = processed_results,
    count_warnings = count_warnings,
    count_errors = count_errors
  )

  # Retorna la lista de resultados bootstrap procesados junto con los conteos de advertencias y errores
  return(results_summary)
}
