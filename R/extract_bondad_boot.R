extract_bondad_boot <- function(resultados_bootstrap) {
  # Cargar las librerías necesarias
  require(purrr)
  require(dplyr)
  require(tibble)

  # Función interna para convertir Bondades a list y añadir el número de muestra
  convertBondadesToList <- function(bondadesList, muestra) {
    bondadesList %>%
      map_dfr(~as_tibble(.), .id = "ID") %>%
      mutate(Muestra = muestra)
  }

  # Función interna para aplicar convertBondadesToList a cada elemento de resultados_bootstrap
  applyConvertBondadesToList <- function(bootstrapResults) {
    allBondadesDf <- map_dfr(seq_along(bootstrapResults$Results), function(muestra) {
      convertBondadesToList(bootstrapResults$Results[[muestra]]$CombinedResults$Bondades, muestra)
    }, .id = "MuestraID")

    # Opcional: Convertir MuestraID a un número si es necesario
    allBondadesDf <- allBondadesDf %>%
      mutate(MuestraID = as.integer(MuestraID))

    return(allBondadesDf)
  }

  # Aplicar la función al conjunto de resultados de bootstrap
  Bondades_ajuste <- applyConvertBondadesToList(resultados_bootstrap)

  return(Bondades_ajuste)
}
