#' @title Filter Aberrant Response Patterns
#' @description Identifies and filters cases with aberrant response patterns using Mahalanobis distance.
#' @param data Data frame containing the items.
#' @param items Character vector of item names to analyze.
#' @param plot Logical, whether to plot the Mahalanobis distances (default FALSE).
#' @return A list with filtered data and a table of aberrant cases.
#' @export
filtrar_aberrantes <- function(data, items, plot = FALSE) {
  # Asegurarse de que la data tenga una columna de ID
  if(!"ID" %in% colnames(data)) {
    data <- data %>% tibble::rownames_to_column("ID")
  }

  # Seleccionar los ítems específicos y ejecutar mahad
  mahad_flags <- careless::mahad(data %>% dplyr::select(dplyr::all_of(items)), flag = TRUE, confidence = 0.999, plot = plot)

  # Identificar los IDs con patrones aberrantes
  ids_aberrantes <- mahad_flags %>%
    tibble::rownames_to_column("ID") %>%
    dplyr::filter(flagged == TRUE) %>%
    dplyr::pull(ID)

  # Crear data sin los casos aberrantes
  data_filtrada <- data %>%
    dplyr::filter(!ID %in% ids_aberrantes)

  # Preparar selección de columnas para la tabla de casos marcados
  columnas_seleccionadas <- c("ID", items, "d_sq", "flagged")

  # Crear tabla de casos marcados con d_sq, flagged y los ítems específicos
  tabla_aberrantes <- mahad_flags %>%
    tibble::rownames_to_column("ID") %>%
    dplyr::inner_join(data, by = "ID") %>%
    dplyr::select(dplyr::all_of(columnas_seleccionadas))

  # Retornar la data filtrada y la tabla de casos aberrantes
  return(list(data_filtrada = data_filtrada, tabla_aberrantes = tabla_aberrantes))
}
