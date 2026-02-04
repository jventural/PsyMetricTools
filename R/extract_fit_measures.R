#' Extract Fit Measures from Lavaan Specifications
#'
#' Extracts fit measures (chi-square, SRMR, WRMR, CFI, TLI, RMSEA) from lavaan models.
#'
#' @param Specifications A list of lavaan model objects.
#'
#' @return A data frame with fit measures for each factor solution.
#' @examples
#' \dontrun{
#' # First, run EFA_modern to get specifications
#' set.seed(123)
#' n <- 300
#' data_efa <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Run EFA
#' efa_result <- EFA_modern(
#'   n_factors = 3,
#'   n_items = 6,
#'   name_items = "Item",
#'   data = data_efa,
#'   apply_threshold = TRUE
#' )
#'
#' # Extract fit measures from all factor solutions
#' fit_table <- extract_fit_measures(efa_result$Specifications)
#' print(fit_table)
#'
#' # The result contains: Factores, chisq.scaled, df.scaled, srmr, wrmr,
#' # cfi.scaled, tli.scaled, rmsea.scaled
#' }
#' @export
extract_fit_measures <- function(Specifications) {
  # Verificar que los paquetes requeridos estén instalados
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required but not installed. Please install it with install.packages('lavaan')")
  }

  CD <- list()

  for (i in 1:length(Specifications)) {
    bondad_modelo <- lavaan::fitMeasures(Specifications[[i]], c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))
    CD[[i]] <- bondad_modelo
  }

  Bondades_Original <- purrr::map_dfr(CD, dplyr::bind_rows) %>% as.data.frame() %>% round(3) %>%
    dplyr::mutate(Factores = rep(paste0("f", 1:length(Specifications)))) %>% dplyr::relocate(Factores, .before = chisq.scaled)

  return(Bondades_Original)
}

