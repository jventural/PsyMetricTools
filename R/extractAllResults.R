#' @name extractAllResults
#' @export
extractAllResults <- function(res) {
  # Verificar si 'res' contiene 'processed_results'
  if (!"processed_results" %in% names(res)) {
    stop("La estructura de 'res' no contiene 'processed_results', verifica la estructura de datos.")
  }

  # Inicializar una lista para almacenar los dataframes temporales
  all_results_list <- list()

  # Contador para mantener el índice en all_results_list
  list_counter <- 1

  # Iterar a través de cada resultado procesado en 'processed_results'
  for(replication_id in seq_along(res$processed_results)) {
    # Verificar si existen 'all_results'
    if (!is.null(res$processed_results[[replication_id]]$all_results)) {
      # Obtener los resultados de la replicación actual
      replication_results <- res$processed_results[[replication_id]]$all_results

      # Iterar a través de cada conjunto de resultados dentro de la replicación
      for(iteration_id in seq_along(replication_results)) {
        # Obtener los resultados de la iteración actual
        results <- replication_results[[iteration_id]]

        # Asegurar que los resultados están en formato de dataframe
        if (!is.data.frame(results)) {
          results <- as.data.frame(t(results))
          # Ajustar nombres de columnas si es necesario debido a la transposición
          names(results) <- c("Variable", "Estimate", "SE", "Z", "P", "CI Lower", "CI Upper")
        }

        # Añadir identificadores de replicación e iteración al dataframe de resultados
        results$replication_id <- replication_id
        results$iteration_id <- iteration_id

        # Almacenar el dataframe en la lista
        all_results_list[[list_counter]] <- results
        list_counter <- list_counter + 1
      }
    }
  }

  # Combinar todos los dataframes almacenados en la lista en un único dataframe
  all_results_df <- do.call(rbind, all_results_list)

  return(all_results_df)
}
