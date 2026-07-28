#' Extract Modification Indices from CFA Results
#'
#' Extracts and combines modification indices from multiple CFA analyses.
#'
#' @param results A list of CFA results containing ModificationsDf.
#'
#' @return A combined data frame of modification indices with sample identifiers.
#' @examples
#' \donttest{
#' library(lavaan)
#'
#' # Create sample data
#' set.seed(123)
#' n <- 300
#' g <- rnorm(n)
#' t1 <- 0.7 * g + rnorm(n, 0, 0.7)
#' t2 <- 0.7 * g + rnorm(n, 0, 0.7)
#' sim_item <- function(t) {
#'   as.numeric(cut(t + rnorm(length(t), 0, 0.8), c(-Inf, -1, 0, 1, Inf)))
#' }
#' data <- data.frame(
#'   Item1 = sim_item(t1), Item2 = sim_item(t1), Item3 = sim_item(t1),
#'   Item4 = sim_item(t2), Item5 = sim_item(t2), Item6 = sim_item(t2)
#' )
#'
#' # Fit the same CFA model on three bootstrap samples
#' model <- "F1 =~ Item1 + Item2 + Item3
#'           F2 =~ Item4 + Item5 + Item6"
#' fit1 <- cfa(model, data = data[sample(n, n, replace = TRUE), ])
#' fit2 <- cfa(model, data = data[sample(n, n, replace = TRUE), ])
#' fit3 <- cfa(model, data = data[sample(n, n, replace = TRUE), ])
#'
#' # Each result contains a ModificationsDf element
#' results_list <- list(
#'   list(ModificationsDf = modificationIndices(fit1, sort = TRUE)),
#'   list(ModificationsDf = modificationIndices(fit2, sort = TRUE)),
#'   list(ModificationsDf = modificationIndices(fit3, sort = TRUE))
#' )
#'
#' # Extract and combine modification indices
#' all_mod_indices <- extract_ModIndex_AFC(results_list)
#' head(all_mod_indices)
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
