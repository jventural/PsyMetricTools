#' Extract All Inter-Factor Correlations from Bootstrap Results
#'
#' Extracts and combines inter-factor correlations from all bootstrap replications.
#'
#' @param res A bootstrap result object containing processed_results.
#'
#' @return A data frame with factor correlations from all replications and iterations.
#'
#' @export
extractAllInterFactors <- function(res) {
  # Verificar si 'res' contiene 'processed_results'
  if (!"processed_results" %in% names(res)) {
    stop("La estructura de 'res' no contiene 'processed_results', verifica la estructura de datos.")
  }

  # Inicializar una lista para almacenar los dataframes temporales
  all_interFactors_list <- list()

  # Contador para mantener el índice en all_interFactors_list
  list_counter <- 1

  # Iterar a través de cada resultado procesado en 'processed_results'
  for(replication_id in seq_along(res$processed_results)) {
    # Verificar si existen 'all_interFactors'
    if (!is.null(res$processed_results[[replication_id]]$all_interFactors)) {
      # Obtener los interFactors de la replicación actual
      replication_interFactors <- res$processed_results[[replication_id]]$all_interFactors

      # Iterar a través de cada conjunto de interFactors dentro de la replicación
      for(iteration_id in seq_along(replication_interFactors)) {
        # Obtener los interFactors de la iteración actual
        interFactors <- replication_interFactors[[iteration_id]]

        # Convertir la matriz de interFactors a un dataframe
        interFactors_df <- as.data.frame(as.table(interFactors))

        # Nombrar las columnas apropiadamente
        colnames(interFactors_df) <- c("Factor1", "Factor2", "Correlation")

        # Añadir identificadores de replicación e iteración al dataframe de interFactors
        interFactors_df$replication_id <- replication_id
        interFactors_df$iteration_id <- iteration_id

        # Almacenar el dataframe en la lista
        all_interFactors_list[[list_counter]] <- interFactors_df
        list_counter <- list_counter + 1
      }
    }
  }

  # Combinar todos los dataframes almacenados en la lista en un único dataframe
  all_interFactors_df <- do.call(rbind, all_interFactors_list)

  return(all_interFactors_df)
}
