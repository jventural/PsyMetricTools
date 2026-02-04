#' Extract Fit Measures from Bootstrap EFA Results
#'
#' Extracts and combines fit measures from bootstrap EFA analyses.
#'
#' @param resultados_bootstrap A bootstrap result object with Results list.
#'
#' @return A data frame of fit measures with sample identifiers.
#' @examples
#' \dontrun{
#' # Assuming you have bootstrap EFA results from boot_efa()
#' # This function extracts fit measures from all bootstrap samples
#'
#' # Run bootstrap EFA
#' boot_efa_results <- boot_efa(
#'   data = my_data,
#'   n_factors = 3,
#'   n_items = 15,
#'   name_items = "Item",
#'   n_replications = 100
#' )
#'
#' # Extract fit measures from bootstrap results
#' fit_measures <- extract_bondad_boot_EFA(boot_efa_results)
#'
#' # View results
#' head(fit_measures)
#'
#' # Summarize across samples
#' fit_measures %>%
#'   group_by(ID) %>%
#'   summarise(
#'     mean_CFI = mean(cfi.scaled, na.rm = TRUE),
#'     mean_RMSEA = mean(rmsea.scaled, na.rm = TRUE)
#'   )
#' }
#' @export
extract_bondad_boot_EFA <- function(resultados_bootstrap) {
  # Cargar las librerías necesarias

  # Función interna para convertir Bondades a list y añadir el número de muestra
  convertBondadesToList <- function(bondadesList, muestra) {
    bondadesList %>%
      purrr::map_dfr(~tibble::as_tibble(.), .id = "ID") %>%
      dplyr::mutate(Muestra = muestra)
  }

  # Función interna para aplicar convertBondadesToList a cada elemento de resultados_bootstrap
  applyConvertBondadesToList <- function(bootstrapResults) {
    allBondadesDf <- purrr::map_dfr(seq_along(bootstrapResults$Results), function(muestra) {
      convertBondadesToList(bootstrapResults$Results[[muestra]]$CombinedResults$Bondades, muestra)
    }, .id = "MuestraID")

    # Opcional: Convertir MuestraID a un número si es necesario
    allBondadesDf <- allBondadesDf %>%
      dplyr::mutate(MuestraID = as.integer(MuestraID))

    return(allBondadesDf)
  }

  # Aplicar la función al conjunto de resultados de bootstrap
  Bondades_ajuste <- applyConvertBondadesToList(resultados_bootstrap)

  return(Bondades_ajuste)
}
