#' Extract Modification Indices from CFA Results
#'
#' Extracts and combines modification indices from multiple CFA analyses.
#'
#' @param results A list of CFA results containing ModificationsDf.
#'
#' @return A combined data frame of modification indices with sample identifiers.
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # Create sample data
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Assume you have multiple CFA results from boot_cfa or similar
#' # Each result contains a ModificationsDf element
#' results_list <- list(
#'   list(ModificationsDf = modificationIndices(fit1, sort = TRUE)),
#'   list(ModificationsDf = modificationIndices(fit2, sort = TRUE)),
#'   list(ModificationsDf = modificationIndices(fit3, sort = TRUE))
#' )
#'
#' # Extract and combine modification indices
#' all_mod_indices <- extract_ModIndex_AFC(results_list)
#'
#' # View combined results with sample identifiers
#' head(all_mod_indices)
#'
#' # Find most frequent modification suggestions
#' all_mod_indices %>%
#'   group_by(lhs, op, rhs) %>%
#'   summarise(n = n(), mean_mi = mean(mi)) %>%
#'   arrange(desc(n))
#' }
#' @export
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
