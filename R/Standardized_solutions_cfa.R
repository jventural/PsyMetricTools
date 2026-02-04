#' @title Standardized Factor Solutions for CFA
#' @description Extracts and formats standardized factor loadings from lavaan CFA.
#' @param specification A lavaan model object.
#' @param name_items Prefix for item names.
#' @param apply_threshold Logical, whether to apply 0.30 threshold (default TRUE).
#' @return A data frame with items and factor loadings.
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # Create sample data
#' set.seed(123)
#' n <- 300
#' data_cfa <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Define CFA model
#' model <- "
#'   f1 =~ Item1 + Item2 + Item3
#'   f2 =~ Item4 + Item5 + Item6
#' "
#'
#' # Fit the model
#' fit <- cfa(model, data = data_cfa, ordered = TRUE, estimator = "WLSMV")
#'
#' # Extract standardized solutions with threshold
#' loadings <- Standardized_solutions_cfa(
#'   specification = fit,
#'   name_items = "Item",
#'   apply_threshold = TRUE
#' )
#' print(loadings)
#'
#' # Without threshold (show all loadings)
#' loadings_full <- Standardized_solutions_cfa(
#'   specification = fit,
#'   name_items = "Item",
#'   apply_threshold = FALSE
#' )
#' print(loadings_full)
#' }
#' @export
Standardized_solutions_cfa <- function (specification, name_items, apply_threshold = TRUE)
{
  # Check for required packages
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }

  result <- lavaan::standardizedsolution(specification) %>% dplyr::filter(op ==
                                                             "=~") %>% dplyr::mutate(item = stringr::str_remove(rhs, name_items) %>%
                                                                                as.double(), factor = stringr::str_remove(lhs, "f")) %>% dplyr::select(lhs,
                                                                                                                                       rhs, est.std) %>% tidyr::pivot_wider(names_from = lhs, values_from = c(est.std))

  # obtener los nombres de las columnas que comienzan con "f"
  factor_names <- colnames(result)[startsWith(colnames(result), "f")]

  if (apply_threshold) {
    result <- result %>% dplyr::mutate(dplyr::across(dplyr::all_of(factor_names),
                                       ~dplyr::case_when(. <= 0.3 ~ 0, TRUE ~ .)))
  }

  result <- result %>% dplyr::arrange(dplyr::across(dplyr::all_of(factor_names), desc)) %>%
    dplyr::rename(Items = rhs)
  return(result)
}
