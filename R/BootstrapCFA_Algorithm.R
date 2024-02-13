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

  # Iniciar la barra de progreso
  pb <- txtProgressBar(min = 0, max = length(bootstrapped_samples), style = 3)

  for (i in 1:length(bootstrapped_samples)) {
    bootstrapData <- bootstrapped_samples[[i]]
    cfaResult <- iterativeModelCFA(data = bootstrapData, initialModel = initialModel, MAX_ITERATIONS = MAX_ITERATIONS, MI_THRESHOLD = MI_THRESHOLD)
    cfaResults[[i]] <- cfaResult

    # Actualizar la barra de progreso
    setTxtProgressBar(pb, i)
  }

  # Cerrar la barra de progreso
  close(pb)

  return(cfaResults)
}
