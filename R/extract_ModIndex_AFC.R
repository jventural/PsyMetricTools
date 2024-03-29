extract_ModIndex_AFC <- function(results) {
  # Inicializa una lista para almacenar los dataframes modificados
  modified_dfs <- list()

  # Recorre cada elemento de la lista para añadir la columna muestra y recoger los dataframes
  for(i in 1:length(results)) {
    # Añade la columna muestra usando mutate y asume que cada elemento tiene un $ModificationsDf
    modified_dfs[[i]] <- results[[i]]$ModificationsDf %>%
      mutate(muestra = i)
  }

  # Combina todos los dataframes modificados en uno solo
  combined_df <- bind_rows(modified_dfs)

  return(combined_df)
}
