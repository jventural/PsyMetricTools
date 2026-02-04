#' Calculate Percentage of Fit Indices Meeting Thresholds
#'
#' Calculates the percentage of bootstrap replications meeting specified fit thresholds.
#'
#' @param df_repli Data frame with bootstrap replications containing fit_measures1.
#' @param thresholds_str String specifying thresholds (e.g., "CFI > 0.90, RMSEA < 0.08").
#'
#' @return A data frame with percentages for each fit measure.
#' @examples
#' \dontrun{
#' # First run boot_cfa to get bootstrap results
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
#' model <- "
#'   F1 =~ Item1 + Item2 + Item3
#'   F2 =~ Item4 + Item5 + Item6
#' "
#'
#' boot_results <- boot_cfa(
#'   new_df = data,
#'   model_string = model,
#'   item_prefix = "Item",
#'   n_replications = 100
#' )
#'
#' # Define threshold string
#' thresholds <- "CFI > 0.95, TLI > 0.95, RMSEA < 0.06, SRMR < 0.08"
#'
#' # Calculate percentage of bootstrap replications meeting thresholds
#' fit_percentages <- calculate_per_fit(boot_results, thresholds)
#' print(fit_percentages)
#'
#' # Different thresholds (more lenient)
#' thresholds2 <- "CFI > 0.90, TLI > 0.90, RMSEA < 0.08"
#' calculate_per_fit(boot_results, thresholds2)
#' }
#' @export
calculate_per_fit <- function(df_repli, thresholds_str) {
  #función interna
  convert_thresholds_str_to_df <- function(thresholds_str) {
    # Separar el string por comas
    components <- unlist(strsplit(thresholds_str, ", "))

    # Separar cada componente en medida, dirección y umbral
    parts <- lapply(components, function(x) strsplit(x, " ")[[1]])
    measure <- sapply(parts, function(x) x[1])
    direction <- sapply(parts, function(x) x[2])
    threshold <- as.numeric(sapply(parts, function(x) x[3]))

    # Crear y retornar el data frame
    data.frame(measure = measure, threshold = threshold, direction = direction, stringsAsFactors = FALSE)
  }

  # Convertir el string de umbrales a data frame
  measure_thresholds <- convert_thresholds_str_to_df(thresholds_str)

  # Convertir los datos a un data frame
  fit_measures1_df <- purrr::map_dfr(df_repli$fit_measures1, ~tibble::as_tibble(.))

  # Inicializar una lista vacía para guardar los resultados
  result_list <- vector("list", length = nrow(measure_thresholds))
  names(result_list) <- measure_thresholds$measure

  # Iterar sobre las medidas y umbrales
  for(i in seq_along(result_list)){
    measure <- measure_thresholds$measure[i]
    threshold <- measure_thresholds$threshold[i]
    direction <- measure_thresholds$direction[i]

    # Filtrar los datos
    if (direction == "<") {
      filtered_data <- fit_measures1_df %>% dplyr::filter(!!rlang::sym(measure) < threshold)
    } else {
      filtered_data <- fit_measures1_df %>% dplyr::filter(!!rlang::sym(measure) > threshold)
    }

    # Calcular el porcentaje
    percent <- nrow(filtered_data) / nrow(fit_measures1_df) * 100
    result_list[[measure]] <- percent
  }

  # Convertir la lista en una tabla
  result_table <- as.data.frame(t(unlist(result_list)), stringsAsFactors = FALSE)
  colnames(result_table) <- names(result_list)
  rownames(result_table) <- NULL

  # Formatear los porcentajes
  result_table[] <- lapply(result_table, function(x) paste0(formatC(x, format = "f", digits = 1), "%"))

  return(result_table)
}
