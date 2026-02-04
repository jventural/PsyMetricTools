#' @name extractAllFitMeasures
#' @export
extractAllFitMeasures <- function(res) {
  # Verificar si 'res' contiene 'processed_results'
  if (!"processed_results" %in% names(res)) {
    stop("La estructura de 'res' no contiene 'processed_results', verifica la estructura de datos.")
  }

  # Inicializar un dataframe vacío para almacenar todas las medidas de ajuste
  all_fit_measures_df <- data.frame()

  # Iterar a través de cada resultado procesado en 'processed_results'
  for(replication_id in seq_along(res$processed_results)) {
    # Obtener los resultados de la replicación actual
    replication_results <- res$processed_results[[replication_id]]

    # Verificar si existen medidas de ajuste
    if (!is.null(replication_results$all_fit_measures)) {
      # Iterar a través de cada conjunto de medidas de ajuste dentro de la replicación
      for(iteration_id in seq_along(replication_results$all_fit_measures)) {
        # Obtener las medidas de ajuste de la iteración actual
        fit_measures <- replication_results$all_fit_measures[[iteration_id]]

        # Convertir las medidas de ajuste en un dataframe si aún no lo es
        if (!is.data.frame(fit_measures)) {
          fit_measures <- as.data.frame(t(fit_measures))
          # Ajustar nombres de columnas si es necesario debido a la transposición
          names(fit_measures) <- c("measure", "value")
        }

        # Añadir identificadores de replicación e iteración al dataframe de medidas de ajuste
        fit_measures$replication_id <- replication_id
        fit_measures$iteration_id <- iteration_id

        # Combinar el dataframe actual con el acumulado
        all_fit_measures_df <- rbind(all_fit_measures_df, fit_measures)
      }
    }
  }

  return(all_fit_measures_df)
}
