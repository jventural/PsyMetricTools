BootstrapEFA_Algorithm <- function(original_data, initialModel, n_samples = 100, seed = NULL) {

  generateBootstrapSamples <- function(data, n_samples = 10, seed = NULL) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    bootstrapped_samples <- lapply(1:n_samples, function(i) {
      sample_n(data, n(), replace = TRUE)
    })
    return(bootstrapped_samples)
  }

  # Generar muestras de bootstrap
  bootstrapped_samples <- generateBootstrapSamples(data = original_data, n_samples = n_samples, seed = seed)

  # Inicializar contador de errores
  error_count <- 0

  results <- lapply(bootstrapped_samples, function(bootstrap_sample) {
    tryCatch({
      # Ejecutar el análisis EFA iterativo
      result <- iterativeModelEFA(data = bootstrap_sample, initialModel = initialModel)

      # Si el análisis es exitoso, preparar los resultados
      combined_results <- data.frame(
        Bondades = I(list(result$Bondades)),
        ExcludedItems = I(list(result$ExcludedItems))
      )

      return(list(
        Models = result$Models,
        CombinedResults = combined_results,
        FinalIteration = result$FinalIteration,
        AllResultsDf = result$AllResultsDf,
        FinalModelInfo = result$FinalModelInfo
      ))
    }, error = function(e) {
      # Incrementar el contador de errores
      error_count <<- error_count + 1

      # Manejar el error pero no detener la ejecución; simplemente retorna NULL
      message("Error de convergencia detectado: ", e$message)
      return(NULL) # Retorna NULL para análisis fallidos
    })
  })

  # Filtrar los resultados para excluir los análisis fallidos (NULLs)
  results <- Filter(Negate(is.null), results)

  # Retornar resultados y la cantidad de errores
  return(list(Results = results, ErrorCount = error_count))
}
