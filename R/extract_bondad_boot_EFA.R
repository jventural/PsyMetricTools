#' @name extract_bondad_boot_EFA
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
