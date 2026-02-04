#' Fit Multiple Lavaan Model Specifications
#'
#' Fits multiple lavaan EFA/CFA models and returns fitted model objects.
#'
#' @param modelos List of model syntax strings.
#' @param data Data frame with item responses.
#' @param estimator Estimation method (e.g., "WLSMV").
#' @param rotation Rotation method for EFA (default: "oblimin").
#' @param ordered Logical, whether variables are ordered (default: TRUE).
#' @param verbose Logical, whether to print model output (default: FALSE).
#'
#' @return A list of fitted lavaan model objects.
#' @examples
#' \dontrun{
#' # Create sample data
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
#' # Generate EFA models for 1 to 3 factors
#' models <- generate_modelos(
#'   n_factors = 3,
#'   name_items = "Item",
#'   n_items = 6
#' )
#'
#' # Fit all models
#' specifications <- specification_models(
#'   modelos = models,
#'   data = data,
#'   estimator = "WLSMV",
#'   rotation = "oblimin",
#'   ordered = TRUE
#' )
#'
#' # Access fitted model for 2-factor solution
#' summary(specifications[[2]])
#'
#' # Extract fit indices
#' fit_table <- extract_fit_measures(specifications)
#' print(fit_table)
#' }
#' @export
specification_models <- function(modelos, data, estimator, rotation = "oblimin",
                                  ordered = TRUE, verbose = FALSE) {
  # Check for required package
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required but not installed.")
  }

  specification <- list()

  for (i in 1:length(modelos)) {
    fit.original = lavaan::cfa(paste0(modelos[i]),
                       data = data,
                       estimator = estimator,
                       rotation = rotation,
                       mimic = "Mplus",
                       ordered = ordered,
                       verbose = verbose)
    specification[[i]] = fit.original

    if (verbose) {
      print(fit.original)
    }
  }

  return(specification)
}
