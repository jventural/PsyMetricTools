#' Extract Excluded Items from Bootstrap Results
#'
#' Extracts all excluded items across bootstrap samples.
#'
#' @param resultados_bootstrap A bootstrap result object with Results list.
#'
#' @return A data frame with excluded items and sample IDs.
#'
#' @export
extract_ExcludedItems_boot <- function(resultados_bootstrap) {
  allExcludedItemsDf <- do.call(rbind, lapply(seq_along(resultados_bootstrap$Results), function(muestra) {
    convertExcludedItemsToList <- function(excludedItems, muestra) {
      excludedItemsDf <- do.call(rbind, lapply(seq_along(excludedItems), function(i) {
        if(length(unlist(excludedItems[[i]])) > 0) {
          data.frame(
            Value = unlist(excludedItems[[i]]),
            Muestra = muestra, # Usar el número de muestra proporcionado
            stringsAsFactors = FALSE
          )
        } else {
          NULL # No devolver nada si la lista está vacía
        }
      }))

      if(is.null(excludedItemsDf)) { # Si todas las listas estaban vacías
        excludedItemsDf <- data.frame(Value = integer(0), Muestra = integer(0), stringsAsFactors = FALSE)
      }

      return(excludedItemsDf)
    }
    # Ajustar el acceso a ExcludedItems según la nueva estructura
    convertExcludedItemsToList(resultados_bootstrap$Results[[muestra]]$CombinedResults$ExcludedItems, muestra)
  }))

  return(allExcludedItemsDf)
}
