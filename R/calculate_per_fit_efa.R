#' Calculate Percentage of Bootstrap EFA Models Meeting Fit Thresholds
#'
#' Calculates the percentage of bootstrap EFA replications that meet
#' specified fit measure thresholds.
#'
#' @param boot_efa_results Results from boot_efa function.
#' @param thresholds_str A string specifying thresholds in format
#'   "MEASURE DIRECTION VALUE" separated by commas.
#'   Example: "CFI > 0.95, TLI > 0.95, RMSEA < 0.06, SRMR < 0.08"
#'
#' @return A data frame with percentages for each threshold.
#'
#' @export
#' @examples
#' \dontrun{
#' thresholds <- "CFI > 0.95, TLI > 0.95, RMSEA < 0.06, SRMR < 0.08"
#' calculate_per_fit_efa(results_boot_efa, thresholds)
#' }
calculate_per_fit_efa <- function(boot_efa_results, thresholds_str) {

  # Funcion interna para convertir string de umbrales a data frame
  convert_thresholds_str_to_df <- function(thresholds_str) {
    # Separar el string por comas
    components <- unlist(strsplit(thresholds_str, ", "))

    # Separar cada componente en medida, direccion y umbral
    parts <- lapply(components, function(x) strsplit(x, " ")[[1]])
    measure <- sapply(parts, function(x) x[1])
    direction <- sapply(parts, function(x) x[2])
    threshold <- as.numeric(sapply(parts, function(x) x[3]))

    # Crear y retornar el data frame
    data.frame(measure = measure, threshold = threshold, direction = direction, stringsAsFactors = FALSE)
  }

  # Convertir el string de umbrales a data frame
  measure_thresholds <- convert_thresholds_str_to_df(thresholds_str)

  # Extraer fit_measures del objeto boot_efa_results
  fit_measures_df <- boot_efa_results$Replicaciones %>%
    dplyr::filter(converged == TRUE) %>%
    dplyr::select(fit_measures) %>%
    tidyr::unnest(fit_measures)

  # Inicializar una lista vacia para guardar los resultados
  result_list <- vector("list", length = nrow(measure_thresholds))
  names(result_list) <- measure_thresholds$measure

  # Iterar sobre las medidas y umbrales
  for(i in seq_along(result_list)){
    measure <- measure_thresholds$measure[i]
    threshold <- measure_thresholds$threshold[i]
    direction <- measure_thresholds$direction[i]

    # Verificar si la columna existe (buscar en mayusculas y minusculas)
    col_name <- names(fit_measures_df)[grepl(paste0("^", measure, "$"), names(fit_measures_df), ignore.case = TRUE)][1]

    if (is.na(col_name)) {
      warning(paste0("La medida '", measure, "' no se encontro en los resultados."))
      result_list[[measure]] <- NA
      next
    }

    # Filtrar los datos
    if (direction == "<") {
      filtered_data <- fit_measures_df %>% dplyr::filter(!!rlang::sym(col_name) < threshold)
    } else {
      filtered_data <- fit_measures_df %>% dplyr::filter(!!rlang::sym(col_name) > threshold)
    }

    # Calcular el porcentaje
    percent <- nrow(filtered_data) / nrow(fit_measures_df) * 100
    result_list[[measure]] <- percent
  }

  # Convertir la lista en una tabla
  result_table <- as.data.frame(t(unlist(result_list)), stringsAsFactors = FALSE)
  colnames(result_table) <- names(result_list)
  rownames(result_table) <- NULL

  # Formatear los porcentajes
  result_table[] <- lapply(result_table, function(x) {
    if (is.na(x)) {
      return("NA")
    } else {
      return(paste0(formatC(x, format = "f", digits = 1), "%"))
    }
  })

  return(result_table)
}
