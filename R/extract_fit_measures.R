#' Extract Fit Measures from Lavaan Specifications
#'
#' Extracts fit measures (chi-square, SRMR, WRMR, CFI, TLI, RMSEA) from lavaan models.
#'
#' @param Specifications A list of lavaan model objects.
#'
#' @return A data frame with fit measures for each factor solution.
#' @examples
#' \donttest{
#' # First, run EFA_modern to get specifications
#' set.seed(123)
#' n <- 300
#' g <- rnorm(n)
#' t1 <- 0.7 * g + rnorm(n, 0, 0.7)
#' t2 <- 0.7 * g + rnorm(n, 0, 0.7)
#' sim_item <- function(t) {
#'   as.numeric(cut(t + rnorm(length(t), 0, 0.8), c(-Inf, -1, 0, 1, Inf)))
#' }
#' data_efa <- data.frame(
#'   Item1 = sim_item(t1), Item2 = sim_item(t1), Item3 = sim_item(t1),
#'   Item4 = sim_item(t2), Item5 = sim_item(t2), Item6 = sim_item(t2)
#' )
#'
#' # Run EFA
#' efa_result <- EFA_modern(
#'   n_factors = 2,
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

