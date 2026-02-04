#' Extract Fit Measures from Bootstrap CFA Results
#'
#' Extracts and combines fit measures from bootstrap CFA analyses.
#'
#' @param results A list of CFA results containing FitMeasuresDf.
#'
#' @return A combined data frame of fit measures with sample and iteration identifiers.
#'
#' @export
extract_bondad_boot_AFC <- function(results) {
  modified_dfs <- list()

  for(i in 1:length(results)) {
    # Prepara el dataframe o matriz
    if(is.matrix(results[[i]]$FitMeasuresDf)) {
      temp_df <- as.data.frame(results[[i]]$FitMeasuresDf)
    } else {
      temp_df <- results[[i]]$FitMeasuresDf
    }

    # Asegura que no haya nombres de filas preexistentes para evitar problemas al combinar
    row.names(temp_df) <- NULL

    # Añade la columna muestra
    temp_df <- temp_df %>% mutate(muestra = i)

    # Añade una columna para indicar la iteración de forma explícita
    # Corrección: genera una secuencia de números basada en el número de filas del dataframe
    temp_df$Iteration <- seq_len(nrow(temp_df))

    modified_dfs[[i]] <- temp_df
  }

  # Combina todos los dataframes modificados en uno solo
  combined_df <- bind_rows(modified_dfs)

  return(combined_df)
}
