BootstrapCFA_Algorithm <-  function(originalData, initialModel, n_samples = 10, seed = NULL, MAX_ITERATIONS = 10, MI_THRESHOLD = 5) {

  generateBootstrapSamples <- function(data, n_samples = 10, seed = NULL) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    bootstrapped_samples <- lapply(1:n_samples, function(i) {
      dplyr::sample_n(data, nrow(data), replace = TRUE)
    })
    return(bootstrapped_samples)
  }

  bootstrapped_samples <- generateBootstrapSamples(originalData, n_samples, seed)

  cfaResults <- list()

  for (i in 1:length(bootstrapped_samples)) {
    # Mensaje que indica en qué iteración de muestra de bootstrap se encuentra
    cat("Iniciando la iteración", i, "de", length(bootstrapped_samples), "\n")
    bootstrapData <- bootstrapped_samples[[i]]

    # Iniciar la barra de progreso para la iteración actual
    pb <- txtProgressBar(min = 0, max = MAX_ITERATIONS, style = 3)

    cfaResult <- iterativeModelCFA(data = bootstrapData, initialModel = initialModel, MAX_ITERATIONS = MAX_ITERATIONS, MI_THRESHOLD = MI_THRESHOLD)

    # Cerrar la barra de progreso de la iteración actual
    close(pb)

    cat("Iteración", i, "completada\n\n")  # Mensaje al completar la iteración
    cfaResults[[i]] <- cfaResult
  }

  return(cfaResults)
}
