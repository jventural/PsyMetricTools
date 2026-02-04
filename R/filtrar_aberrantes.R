# Declare global variables for R CMD check
utils::globalVariables(c("row_idx", "d_sq", "flagged"))

#' Filter Aberrant Response Patterns
#'
#' Identifies and filters cases with aberrant response patterns using Mahalanobis distance.
#'
#' @param data Data frame containing the items.
#' @param items Character vector of item names to analyze.
#' @param plot Logical, whether to plot the Mahalanobis distances (default FALSE).
#'
#' @return A list with filtered data and a table of aberrant cases.
#' @examples
#' \dontrun{
#' # Create sample data with some aberrant response patterns
#' set.seed(123)
#' n <- 200
#' data <- data.frame(
#'   ID = 1:n,
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Add some aberrant patterns (all same responses)
#' data[1, 2:6] <- rep(1, 5)
#' data[2, 2:6] <- rep(5, 5)
#'
#' # Filter aberrant cases
#' result <- filtrar_aberrantes(
#'   data = data,
#'   items = c("Item1", "Item2", "Item3", "Item4", "Item5"),
#'   plot = FALSE
#' )
#'
#' # Access filtered data (without aberrant cases)
#' clean_data <- result$data_filtrada
#' nrow(clean_data)
#'
#' # View table of aberrant cases with Mahalanobis distances
#' result$tabla_aberrantes
#'
#' # With visualization of Mahalanobis distances
#' result_plot <- filtrar_aberrantes(
#'   data = data,
#'   items = c("Item1", "Item2", "Item3", "Item4", "Item5"),
#'   plot = TRUE
#' )
#' }
#' @export
filtrar_aberrantes <- function(data, items, plot = FALSE) {
  # Asegurarse de que la data tenga una columna de ID
if (!"ID" %in% colnames(data)) {
    data <- data %>% tibble::rownames_to_column("ID")
  } else {
    # Convertir ID existente a character para consistencia
    data <- data %>% dplyr::mutate(ID = as.character(ID))
  }

  # Seleccionar los items especificos y ejecutar mahad
  mahad_flags <- careless::mahad(data %>% dplyr::select(dplyr::all_of(items)),
                                  flag = TRUE, confidence = 0.999, plot = plot)

  # Agregar ID a mahad_flags (rownames son indices numericos como character)
  mahad_flags <- mahad_flags %>%
    tibble::rownames_to_column("row_idx") %>%
    dplyr::mutate(ID = data$ID[as.numeric(row_idx)])

  # Identificar los IDs con patrones aberrantes
  ids_aberrantes <- mahad_flags %>%
    dplyr::filter(flagged == TRUE) %>%
    dplyr::pull(ID)

  # Crear data sin los casos aberrantes
  data_filtrada <- data %>%
    dplyr::filter(!ID %in% ids_aberrantes)

  # Preparar seleccion de columnas para la tabla de casos marcados
  columnas_seleccionadas <- c("ID", items, "d_sq", "flagged")

  # Crear tabla de casos marcados con d_sq, flagged y los items especificos
  tabla_aberrantes <- mahad_flags %>%
    dplyr::select(ID, d_sq, flagged) %>%
    dplyr::inner_join(data, by = "ID") %>%
    dplyr::select(dplyr::all_of(columnas_seleccionadas))

  # Retornar la data filtrada y la tabla de casos aberrantes
  return(list(data_filtrada = data_filtrada, tabla_aberrantes = tabla_aberrantes))
}
